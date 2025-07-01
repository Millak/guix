/* GNU Guix --- Functional package management for GNU
   Copyright (C) 2025 Caleb Ristvedt <reepca@russelstein.xyz>

   This file is part of GNU Guix.

   GNU Guix is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3 of the License, or (at
   your option) any later version.

   GNU Guix is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.  */

/* Process spawning and setup code.  */

#include <spawn.hh>
#include <util.hh>
#include <affinity.hh>
#include <stddef.h>
#include <unistd.h>
#include <grp.h>
#include <limits.h>
#include <sys/wait.h>
#include <fcntl.h>
#include <cstring>
#include <cstdlib>
#include <cstdint>

#if HAVE_SYS_MOUNT_H
#include <sys/mount.h>
#endif

#if HAVE_SCHED_H
#include <sched.h>
#endif

#if HAVE_STATVFS
#include <sys/statvfs.h>
#endif

#if HAVE_SYS_SYSCALL_H
#include <sys/syscall.h>
#endif

#if HAVE_SYS_PRCTL_H
#include <sys/prctl.h>
#endif

#ifdef __linux__
#include <sys/personality.h>
#include <linux/seccomp.h>
#include <linux/filter.h>
#endif

#if defined(SYS_pivot_root)
#define pivot_root(new_root, put_old) (syscall(SYS_pivot_root, new_root,put_old))
#endif


#define CLONE_ENABLED defined(CLONE_NEWNS)

#if CLONE_ENABLED
#include <sys/ioctl.h>
#include <net/if.h>
#include <netinet/in.h>
#endif

namespace nix {


void addPhaseAfter(Phases & phases, string afterLabel, string addLabel, Action addAction)
{
    for(auto i = phases.begin(); i != phases.end(); i++)
        if((*i).label == afterLabel) {
            i++; /* std::vector::insert inserts before, not after */
            Phase p;
            p.label = addLabel;
            p.action = addAction;
            phases.insert(i, p);
            return;
        }
    throw Error(format("label `%1%' not found in phases") % afterLabel);
}


void addPhaseBefore(Phases & phases, string beforeLabel, string addLabel, Action addAction)
{
    for(auto i = phases.begin(); i != phases.end(); i++)
        if((*i).label == beforeLabel) {
            Phase p;
            p.label = addLabel;
            p.action = addAction;
            phases.insert(i, p);
            return;
        }
    throw Error(format("label `%1%' not found in phases") % beforeLabel);
}


void prependPhase(Phases & phases, string addLabel, Action addAction)
{
    Phase p;
    p.label = addLabel;
    p.action = addAction;
    phases.insert(phases.begin(), p);
}


void appendPhase(Phases & phases, string addLabel, Action addAction)
{
    Phase p;
    p.label = addLabel;
    p.action = addAction;
    phases.push_back(p);
}


void deletePhase(Phases & phases, string delLabel)
{
    for(auto i = phases.begin(); i != phases.end(); i++)
        if((*i).label == delLabel) {
            phases.erase(i);
            return;
        }
    throw Error(format("label `%1%' not found in phases") % delLabel);
}


void replacePhase(Phases & phases, string replaceLabel, Action newAction)
{
    for(auto i = phases.begin(); i != phases.end(); i++)
        if((*i).label == replaceLabel) {
            (*i).action = newAction;
            return;
        }
    throw Error(format("label `%1' not found in phases") % replaceLabel);
}


/* A curated selection of predefined actions */

void reset_writeToStderrAction(SpawnContext & ctx)
{
    _writeToStderr = 0;
}


void restoreAffinityAction(SpawnContext & ctx)
{
    restoreAffinity();
}


void setsidAction(SpawnContext & ctx)
{
    /* Puts the current process in a separate session, which implies a
       separate process group, so it doesn't receive group-directed signals
       sent at the parent.  The new session initially has no controlling
       terminal, so it also doesn't receive terminal signals and can't open
       /dev/tty. */
    if(ctx.setsid && setsid() == (pid_t)-1)
        throw SysError("creating a new session");
}


void earlyIOSetupAction(SpawnContext & ctx)
{
    for(auto i = ctx.earlyCloseFDs.begin(); i != ctx.earlyCloseFDs.end(); i++)
        if(close(*i) == -1)
            throw SysError("closing fd");

    if(ctx.logFD != -1) {
        if(dup2(ctx.logFD, STDOUT_FILENO) == -1)
            throw SysError("cannot dup2 log fd into stdout fd");
        if(dup2(ctx.logFD, STDERR_FILENO) == -1)
            throw SysError("cannot dup2 log fd into stderr fd");
    }

    if(ctx.setStdin) {
        if(ctx.stdinFD != -1) {
            if(dup2(ctx.stdinFD, STDIN_FILENO) == -1)
                throw SysError("cannot dup2 fd into stdin fd");
        }
        else {
            /* Doesn't make sense for it to be writable, but compatibility... */
            AutoCloseFD fd = open(ctx.stdinFile.c_str(), O_RDWR);
            if(fd == -1)
                throw SysError(format("cannot open `%1%'") % ctx.stdinFile);
            if(dup2(fd, STDIN_FILENO) == -1)
                throw SysError("cannot dup2 fd into stdin fd");
        }
    }
}


void dropAmbientCapabilitiesAction(SpawnContext & ctx)
{
  /* Drop ambient capabilities such as CAP_CHOWN that might have been granted
     when starting guix-daemon.  */
    if(ctx.dropAmbientCapabilities)
#if HAVE_SYS_PRCTL_H
        prctl(PR_CAP_AMBIENT, PR_CAP_AMBIENT_CLEAR_ALL, 0, 0, 0);
#else
        throw Error("dropping ambient capabilities is not supported on this system");
#endif
}


void chrootAction(SpawnContext & ctx)
{
    if(ctx.doChroot)
#if HAVE_CHROOT
        if(chroot(ctx.chrootRootDir.c_str()) == -1)
            throw SysError(format("cannot change root directory to '%1%'") % ctx.chrootRootDir);
#else
    throw Error("chroot is not supported on this system");
#endif
}


void chdirAction(SpawnContext & ctx)
{
    if(ctx.setcwd)
        if(chdir(ctx.cwd.c_str()) == -1)
            throw SysError(format("changing into `%1%'") % ctx.cwd);
}


void closeMostFDsAction(SpawnContext & ctx)
{
    if(ctx.closeMostFDs) closeMostFDs(ctx.preserveFDs);
    for(auto i = ctx.preserveFDs.begin(); i != ctx.preserveFDs.end(); i++)
        keepOnExec(*i);
}


void setPersonalityAction(SpawnContext & ctx)
{
  if(ctx.setPersona)
#ifdef __linux__
    if(personality(ctx.persona) == -1)
      throw SysError("cannot set personality");
#else
    throw Error("setting the personality is not supported on this system");
#endif
}


void oomSacrificeAction(SpawnContext & ctx)
{
#ifdef __linux__
    if(ctx.oomSacrifice)
        /* Ask the kernel to eagerly kill us & our children if it runs out of
           memory, regardless of blame, to preserve ‘real’ user data &
           state. */
        try {
            writeFile("/proc/self/oom_score_adj", "1000"); // 100%
        } catch(...) { ignoreException(); }
#endif
}


void setIDsAction(SpawnContext & ctx)
{
    if(ctx.setSupplementaryGroups)
        if(setgroups(ctx.supplementaryGroups.size(),
                     ctx.supplementaryGroups.data()) == -1)
            throw SysError("cannot set supplementary groups");

    if(ctx.setgid)
        if(setgid(ctx.group) == -1 ||
           getgid() != ctx.group ||
           getegid() != ctx.group)
            throw SysError("setgid failed");

    if(ctx.setuid)
        if(setuid(ctx.user) == -1 ||
           getuid() != ctx.user ||
           geteuid() != ctx.user)
            throw SysError("setuid failed");
}

void setNoNewPrivsAction(SpawnContext & ctx)
{
  if(ctx.setNoNewPrivs)
#if __linux__ && defined(PR_SET_NO_NEW_PRIVS)
      if(prctl(PR_SET_NO_NEW_PRIVS, 0, 0, 0, 0) == -1)
          throw SysError("setting PR_SET_NO_NEW_PRIVS");
#else
      throw Error("setting PR_SET_NO_NEW_PRIVS not supported on this system");
#endif
}

void addSeccompFilterAction(SpawnContext & ctx)
{
    if(ctx.addSeccompFilter) {
#if __linux__ && defined(PR_SET_SECCOMP) && defined(SECCOMP_MODE_FILTER)
        /* We use no extra functionality from the seccomp system call, so
         * just use prctl. */
        if(ctx.seccompFilter.size() > USHRT_MAX)
            throw Error("seccomp filter too large");
        struct sock_fprog prog;
        prog.len = (unsigned short) ctx.seccompFilter.size();
        prog.filter = ctx.seccompFilter.data();
        if(prctl(PR_SET_SECCOMP, SECCOMP_MODE_FILTER, &prog) == -1)
            throw SysError("installing seccomp filter");
#else
        throw Error("setting seccomp filter not supported on this system");
#endif
    }
}


void restoreSIGPIPEAction(SpawnContext & ctx)
{
    /* Restore default handling of SIGPIPE, otherwise some programs will
       randomly say "Broken pipe". */
    struct sigaction act, oact;
    act.sa_handler = SIG_DFL;
    act.sa_flags = 0;
    sigemptyset(&act.sa_mask);
    if (sigaction(SIGPIPE, &act, &oact)) throw SysError("resetting SIGPIPE");
}


void setupSuccessAction(SpawnContext & ctx)
{
    if(ctx.signalSetupSuccess)
        writeFull(STDERR_FILENO, "\n");
}


void execAction(SpawnContext & ctx)
{
    Strings envStrs;
    std::vector<char *> envPtrs;
    char **env;
    if(ctx.inheritEnv) {
        for(auto i = ctx.env.begin(); i != ctx.env.end(); i++)
            if(setenv(i->first.c_str(), i->second.c_str(), 1) == -1)
                throw SysError("setenv");
        env = environ;
    } else {
        for(auto i = ctx.env.begin(); i != ctx.env.end(); i++)
            envStrs.push_back(i->first + "=" + i->second);
        /* Need to keep the envPtrs vector alive as long as its .data()! */
        envPtrs = stringsToCharPtrs(envStrs);
        env = envPtrs.data();
    }
    if(execvpe(ctx.program.c_str(), stringsToCharPtrs(ctx.args).data(), env) == -1)
        throw SysError(format("executing `%1%'") % ctx.program);
}


Phases getBasicSpawnPhases()
{
    return { { "reset_writeToStderr",     reset_writeToStderrAction },
             { "restoreAffinity",         restoreAffinityAction },
             { "setsid",                  setsidAction },
             { "earlyIOSetup",            earlyIOSetupAction },
             { "dropAmbientCapabilities", dropAmbientCapabilitiesAction },
             { "chroot",                  chrootAction },
             { "chdir",                   chdirAction },
             { "closeMostFDs",            closeMostFDsAction },
             { "setPersonality",          setPersonalityAction },
             { "oomSacrifice",            oomSacrificeAction },
             { "setIDs",                  setIDsAction },
             { "setNoNewPrivs",           setNoNewPrivsAction },
             { "addSeccompFilter",        addSeccompFilterAction },
             { "restoreSIGPIPE",          restoreSIGPIPEAction },
             { "setupSuccess",            setupSuccessAction },
             { "exec",                    execAction } };
}


void usernsInitSyncAction(SpawnContext & sctx)
{
#if CLONE_ENABLED
    CloneSpawnContext & ctx = (CloneSpawnContext &) sctx;
    if((ctx.cloneFlags & CLONE_NEWUSER) != 0) {
        /* Close the earlyCloseFDs before we try reading anything */
        for(auto i = ctx.earlyCloseFDs.begin(); i != ctx.earlyCloseFDs.end(); i++)
            if(close(*i) == -1)
                throw SysError("closing fd");
        /* Don't try closing them again later */
        ctx.earlyCloseFDs.clear();
        /* Wait for the parent process to initialize the UID/GID mapping of
           our user namespace.  */
        waitForMessage(ctx.setupFD, "go\n");
    }
#endif
}


void usernsSetIDsAction(SpawnContext & sctx)
{
#if CLONE_ENABLED
    CloneSpawnContext & ctx = (CloneSpawnContext &) sctx;
    if((ctx.cloneFlags & CLONE_NEWUSER) != 0) {
        /* Note: 'man capabilities' says that a transition from zero to
           nonzero uids causes capabilities to be lost, but doesn't say what
           happens when a transition from an unmapped (possibly zero) uid to a
           nonzero uid happens. */
        if(ctx.usernsSetuid)
            /* Since we presumably have CAP_SETUID, this sets the real,
               effective, saved, and filesystem uids */
            if(setuid(ctx.usernsUser) != 0)
                throw SysError("setuid");
        if(ctx.usernsSetgid)
            /* Ditto but with gids */
            if(setgid(ctx.usernsGroup) != 0)
                throw SysError("setgid");
    }
#endif
}


void initLoopbackAction(SpawnContext & sctx)
{
#if CLONE_ENABLED
    CloneSpawnContext & ctx = (CloneSpawnContext &) sctx;
    if(((ctx.cloneFlags & CLONE_NEWNET) != 0) && ctx.initLoopback) {
        AutoCloseFD fd(socket(PF_INET, SOCK_DGRAM, IPPROTO_IP));
        if (fd == -1) throw SysError("cannot open IP socket");

        struct ifreq ifr;
        strcpy(ifr.ifr_name, "lo");
        ifr.ifr_flags = IFF_UP | IFF_LOOPBACK | IFF_RUNNING;
        if (ioctl(fd, SIOCSIFFLAGS, &ifr) == -1)
            throw SysError("cannot set loopback interface flags");

        fd.close();
    }
#endif
}


void setHostAndDomainAction(SpawnContext & sctx)
{
#if CLONE_ENABLED
    CloneSpawnContext & ctx = (CloneSpawnContext &) sctx;
    if((ctx.cloneFlags & CLONE_NEWUTS) != 0) {
        if (sethostname(ctx.hostname.c_str(),
                        strlen(ctx.hostname.c_str())) == -1)
            throw SysError("cannot set host name");
        if (setdomainname(ctx.domainname.c_str(),
                          strlen(ctx.domainname.c_str())) == -1)
            throw SysError("cannot set domain name");
    }
#endif
}


void makeFilesystemsPrivateAction(SpawnContext & sctx)
{
#if CLONE_ENABLED && HAVE_SYS_MOUNT_H && defined(MS_REC) && defined(MS_PRIVATE)
    CloneSpawnContext & ctx = (CloneSpawnContext &) sctx;
    if((ctx.cloneFlags & CLONE_NEWNS) != 0) {
        if(mount(0, "/", 0, MS_REC|MS_PRIVATE, 0) == -1)
            throw SysError("unable to make `/' private mount");
    }
#endif
}


void makeChrootSeparateFilesystemAction(SpawnContext & sctx)
{
#if CLONE_ENABLED && HAVE_SYS_MOUNT_H && defined(MS_BIND)
    CloneSpawnContext & ctx = (CloneSpawnContext &) sctx;
    if(((ctx.cloneFlags & CLONE_NEWNS) != 0) && ctx.doChroot) {
        /* Bind-mount chroot directory to itself, to treat it as a different
           filesystem from /, as needed for pivot_root.  Alternatively, mount
           a tmpfs on it. */
        if(ctx.mountTmpfsOnChroot) {
            if(mount("none", ctx.chrootRootDir.c_str(), "tmpfs", 0, 0) == -1)
                throw SysError(format("unable to mount tmpfs on `%1%'") % ctx.chrootRootDir);
        }
        else {
            if(mount(ctx.chrootRootDir.c_str(), ctx.chrootRootDir.c_str(), 0, MS_BIND, 0) == -1)
                throw SysError(format("unable to bind mount ‘%1%’") % ctx.chrootRootDir);
        }
    }
#endif
}


static int statfsToMountFlags(int f_flags)
{
#if HAVE_SYS_MOUNT_H && HAVE_STATVFS
    int ret = 0;
#if defined(ST_RDONLY) && defined(MS_RDONLY)
    if((f_flags & ST_RDONLY) != 0)     ret |= MS_RDONLY;
#endif
#if defined(ST_NOSUID) && defined(MS_NOSUID)
    if((f_flags & ST_NOSUID) != 0)     ret |= MS_NOSUID;
#endif
#if defined(ST_NODEV) && defined(MS_NODEV)
    if((f_flags & ST_NODEV) != 0)      ret |= MS_NODEV;
#endif
#if defined(ST_NOEXEC) && defined(MS_NOEXEC)
    if((f_flags & ST_NOEXEC) != 0)     ret |= MS_NOEXEC;
#endif
#if defined(ST_NOATIME) && defined(MS_NOATIME)
    if((f_flags & ST_NOATIME) != 0)    ret |= MS_NOATIME;
#endif
#if defined(ST_NODIRATIME) && defined(MS_NODIRATIME)
    if((f_flags & ST_NODIRATIME) != 0) ret |= MS_NODIRATIME;
#endif
#if defined(ST_RELATIME) && defined(MS_RELATIME)
    if((f_flags & ST_RELATIME) != 0)   ret |= MS_RELATIME;
#endif
    return ret;
#else
    throw Error("statfsToMountFlags not supported on this platform");
#endif
}


void bindMount(Path source, Path target, bool readOnly)
{
#if HAVE_SYS_MOUNT_H && defined(MS_BIND)
    struct stat st;
    if (lstat(source.c_str(), &st) == -1)
        throw SysError(format("getting attributes of path `%1%'") % source);

    if(S_ISDIR(st.st_mode))
        createDirs(target);
    else if(S_ISLNK(st.st_mode)) {
        /* bind-mounts follow symlinks, thus representing their target and not
           the symlink itself.  Create a copy of the symlink instead.*/
        createDirs(dirOf(target));
        createSymlink(readLink(source), target);
        return;
    }
    else {
        struct stat st2;
        createDirs(dirOf(target));
        /* Alternate between trying to create placeholder file at target and
         * checking for its existence and type */
        while(true){
            if(lstat(target.c_str(), &st2) != -1) {
                if(!S_ISREG(st2.st_mode))
                    throw Error(format("mount target `%1%' exists but is not a regular file") % target);
                break;
            }
            if(errno != ENOENT) {
                throw SysError(format("stat'ing path `%1%'") % target);
            }
            AutoCloseFD fd = open(target.c_str(),
                                  O_WRONLY | O_NOFOLLOW | O_CREAT | O_EXCL,
                                  0600);
            if(fd != -1) {
                fd.close(); /* Now exists and is a fresh regular file */
                break;
            }
            /* Note: because of O_CREAT | O_EXCL, EACCES can only mean a
             * permission issue with the parent directory */
            if(errno != EEXIST)
                throw SysError(format("Creating placeholder regular file target mount `%1%'") % target);
        }
    }

    /* This may fail with EINVAL unless we specify MS_REC, specifically if we
       are in an unprivileged mount namespace and not specifying MS_REC would
       reveal subtrees that had been covered up. */
    if (mount(source.c_str(), target.c_str(), 0, MS_BIND|MS_REC, 0) == -1)
        throw SysError(format("bind mount from `%1%' to `%2%' failed") % source % target);
    if(readOnly) {
#if defined(MS_REMOUNT) && defined(MS_RDONLY)
        /* Extra flags passed with MS_BIND are ignored, hence the extra
           MS_REMOUNT.  */
        unsigned long mount_flags = MS_BIND | MS_REMOUNT | MS_RDONLY;
        /* MS_BIND | MS_REMOUNT sets all mountpoint flags, so we may get EPERM
           unless we preserve the other flags (for example because it would
           result in trying to clear the nosuid flag). */
#if HAVE_STATVFS
        struct statvfs stvfs;
        if(statvfs(target.c_str(), &stvfs) == -1)
            throw SysError(format("statvfs of `%1%'") % target);
        mount_flags |= statfsToMountFlags(stvfs.f_flag);
#endif

        if (mount(source.c_str(), target.c_str(), 0, mount_flags, 0) == -1)
            throw SysError(format("read-only remount of `%1%' failed") % target);
#else
        throw Error("remounting read-only is not supported on this platform");
#endif
    }
#endif
}


void mountIntoChroot(std::map<Path, Path> filesInChroot,
                     set<Path> readOnlyFiles,
                     Path chrootRootDir)
{
#if HAVE_SYS_MOUNT_H && defined(MS_BIND)
    for(auto i = filesInChroot.begin(); i != filesInChroot.end(); i++) {
        Path source = i->second;
        Path target = chrootRootDir + i->first;
        bool readOnly = readOnlyFiles.find(i->first) != readOnlyFiles.end();
        bindMount(source, target, readOnly);
    }
#else
    throw Error("bind mounting not supported on this platform");
#endif
}


void mountIntoChrootAction(SpawnContext & sctx)
{
#if CLONE_ENABLED && HAVE_SYS_MOUNT_H && defined(MS_BIND)
    CloneSpawnContext & ctx = (CloneSpawnContext &) sctx;
    if((ctx.cloneFlags & CLONE_NEWNS) != 0 && ctx.doChroot) {
        mountIntoChroot(ctx.filesInChroot, ctx.readOnlyFilesInChroot, ctx.chrootRootDir);
    }
#endif
}


void mountProcAction(SpawnContext & sctx)
{
#if CLONE_ENABLED && HAVE_SYS_MOUNT_H
    CloneSpawnContext & ctx = (CloneSpawnContext &) sctx;
    if((ctx.cloneFlags & CLONE_NEWNS) != 0 && ctx.mountProc) {
        Path target = (ctx.doChroot ? ctx.chrootRootDir : "") + "/proc";
        createDirs(target);
        if(mount("none", target.c_str(), "proc", 0, 0) == -1)
            throw SysError(format("mounting `%1%'") % target);
    }
#endif
}


void mountDevshmAction(SpawnContext & sctx)
{
#if CLONE_ENABLED && HAVE_SYS_MOUNT_H
    CloneSpawnContext & ctx = (CloneSpawnContext &) sctx;
    if((ctx.cloneFlags & CLONE_NEWNS) != 0 && ctx.mountDevshm) {
        Path target = (ctx.doChroot ? ctx.chrootRootDir : "") + "/dev/shm";
        createDirs(target);
        if(mount("none", target.c_str(), "tmpfs", 0, 0) == -1)
            throw SysError(format("mounting `%1%'") % target);
    }
#endif
}


void mountDevptsAction(SpawnContext & sctx)
{
#if CLONE_ENABLED && HAVE_SYS_MOUNT_H
    CloneSpawnContext & ctx = (CloneSpawnContext &) sctx;
    if((ctx.cloneFlags & CLONE_NEWNS) != 0 && ctx.maybeMountDevpts) {
        Path chroot = (ctx.doChroot ? ctx.chrootRootDir : "");
        Path target = chroot + "/dev/pts";
        if(pathExists(chroot + "/dev/ptmx")) return;
        createDirs(target);
        if(mount("none", target.c_str(), "devpts", 0, "newinstance,mode=0620") == -1)
            throw SysError(format("mounting `%1%'") % target);
        createSymlink("/dev/pts/ptmx", chroot + "/dev/ptmx");
        /* Make sure /dev/pts/ptmx is world-writable.  With some Linux
           versions, it is created with permissions 0.  */
        Path targetPtmx = chroot + "/dev/pts/ptmx";
        if (chmod(targetPtmx.c_str(), 0666) == -1)
            throw SysError(format("setting permissions on `%1%'") % targetPtmx);
    }
#endif
}


void pivotRootAction(SpawnContext & sctx)
{
#if CLONE_ENABLED && HAVE_SYS_MOUNT_H
    CloneSpawnContext & ctx = (CloneSpawnContext &) sctx;
    if((ctx.cloneFlags & CLONE_NEWNS) != 0 && ctx.doChroot) {
        if (chdir(ctx.chrootRootDir.c_str()) == -1)
            throw SysError(format("cannot change directory to '%1%'") % ctx.chrootRootDir);

        if (mkdir("real-root", 0) == -1)
            throw SysError("cannot create real-root directory");

        if (pivot_root(".", "real-root") == -1)
            throw SysError(format("cannot pivot old root directory onto '%1%'") % (ctx.chrootRootDir + "/real-root"));

        if (chroot(".") == -1)
            throw SysError(format("cannot change root directory to '%1%'") % ctx.chrootRootDir);

        if (umount2("real-root", MNT_DETACH) == -1)
            throw SysError("cannot unmount real root filesystem");

        if (rmdir("real-root") == -1)
            throw SysError("cannot remove real-root directory");
    }
#endif
}


string idMapToIdentityMap(const string & map)
{
    std::vector<string> mapLines =
        tokenizeString<std::vector<string> >(map, "\n");
    string out;

    for(auto & i : mapLines) {
        std::vector<string> elements =
            tokenizeString<std::vector<string> >(i, " ");
        out.append(elements.at(0) + " " + elements.at(0) + " " + elements.at(2) + "\n");
    }
    return out;
}


/* Initializing a user namespace with more than one id mapped requires
 * capabilities in the *parent* user namespace, which may not even have any
 * processes in it after unshare is called.  So fork a child and have it do
 * the initialization. */
void unshareAndInitUserns(int flags, const string & uidMap,
                          const string & gidMap, bool allowSetgroups)
{
#if CLONE_ENABLED
    pid_t pid_ = getpid();
    string pid = std::to_string(pid_);
    Pipe toChild;
    Pipe fromChild;
    toChild.create();
    fromChild.create();
    pid_t child = fork();
    if(child == -1)
        throw SysError("creating child process");
    if(child == 0) {
        try {
            toChild.writeSide.close();
            fromChild.readSide.close();
            waitForMessage(toChild.readSide, "ready\n");
            writeFile("/proc/" + pid + "/uid_map", uidMap);
            writeFile("/proc/" + pid + "/setgroups",
                      allowSetgroups ? "allow" : "deny");
            writeFile("/proc/" + pid + "/gid_map", gidMap);
            writeFull(fromChild.writeSide, (unsigned char*)"go\n", 3);
        } catch(...) {
            /* Don't unwind the stack in case of exception, halt
             * immediately. */
            _exit(1);
        }
        _exit(EXIT_SUCCESS);
    } else {
        toChild.readSide.close();
        fromChild.writeSide.close();
        if(unshare(flags) == -1)
            throw SysError("unshare");
        writeFull(toChild.writeSide, (unsigned char*)"ready\n", 6);
        waitForMessage(fromChild.readSide, "go\n");
        int status;
        while(waitpid(child, &status, 0) == -1) {
            if(errno != EINTR)
                throw SysError("reaping userns init process");
        }
        if(!(WIFEXITED(status) != 0 && WEXITSTATUS(status) == EXIT_SUCCESS))
            throw Error(format("userns init child exited with status %1%") % WEXITSTATUS(status));
    }
#endif
}


void lockMountsAction(SpawnContext & sctx)
{
#if CLONE_ENABLED && HAVE_SYS_MOUNT_H
    CloneSpawnContext & ctx = (CloneSpawnContext &) sctx;
    if(ctx.lockMounts) {
        string uidMap;
        string gidMap;
        if(ctx.lockMountsMapAll) {
            string oldUidMap = readFile("/proc/self/uid_map", true);
            string oldGidMap = readFile("/proc/self/gid_map", true);
            uidMap = idMapToIdentityMap(oldUidMap);
            gidMap = idMapToIdentityMap(oldGidMap);
        } else {
            string uid = std::to_string(getuid());
            string gid = std::to_string(getgid());
            uidMap = uid + " " + uid + " 1";
            gidMap = gid + " " + gid + " 1";
        }
        unshareAndInitUserns(CLONE_NEWNS | CLONE_NEWUSER,
                             uidMap, gidMap, ctx.lockMountsAllowSetgroups);
        /* Check that mounts inherited in our new mount namespace are "locked"
           together and cannot be separated from within our mount namespace.
           Since umount(2) is documented to fail with EINVAL when attempting
           to unmount one of the mounts that are locked together, check that
           this is what we get.  */
        int ret = umount("/proc");
        assert(ret == -1 && errno == EINVAL);
    }
#endif
}


Phases getCloneSpawnPhases()
{
#if CLONE_ENABLED
    return { { "reset_writeToStderr",          reset_writeToStderrAction          },
             { "usernsInitSync",               usernsInitSyncAction               },
             { "usernsSetIDs",                 usernsSetIDsAction                 },
             { "restoreAffinity",              restoreAffinityAction              },
             { "setsid",                       setsidAction                       },
             { "earlyIOSetup",                 earlyIOSetupAction                 },
             { "dropAmbientCapabilities",      dropAmbientCapabilitiesAction      },
             { "initLoopback",                 initLoopbackAction                 },
             { "setHostAndDomain",             setHostAndDomainAction             },
             { "makeFilesystemsPrivate",       makeFilesystemsPrivateAction       },
             { "makeChrootSeparateFilesystem", makeChrootSeparateFilesystemAction },
             { "mountIntoChroot",              mountIntoChrootAction              },
             { "mountProc",                    mountProcAction                    },
             { "mountDevshm",                  mountDevshmAction                  },
             { "mountDevpts",                  mountDevptsAction                  },
             { "chroot",                       pivotRootAction                    },
             { "chdir",                        chdirAction                        },
             { "closeMostFDs",                 closeMostFDsAction                 },
             { "setPersonality",               setPersonalityAction               },
             { "oomSacrifice",                 oomSacrificeAction                 },
             /* Being put in a user namespace with only the current ids mapped
                would tend to prevent switching to other ones, but if this
                comes after setIDs then the per-process "dumpable" flag may be
                reset, which will cause /proc/self to become root-owned,
                making /proc/self/uid_map inaccessible.  If you need
                lockMounts to preserve the id mappings, and you have the
                necessary capabilities in the parent user namespace, set
                CloneSpawnContext.lockMountsMapAll = true. */
             { "lockMounts",                   lockMountsAction                   },
             { "setIDs",                       setIDsAction                       },
             { "setNoNewPrivs",                setNoNewPrivsAction                },
             { "addSeccompFilter",             addSeccompFilterAction             },
             { "restoreSIGPIPE",               restoreSIGPIPEAction               },
             { "setupSuccess",                 setupSuccessAction                 },
             { "exec",                         execAction                         }};
#else
    throw Error("clone not supported on this platform");
#endif
}


void runChildSetup(SpawnContext & ctx)
{
    ctx.currentPhase = 0;
    try {
        /* Should not return regularly from this */
        while(true) {
            ctx.phases.at(ctx.currentPhase).action(ctx);
            ctx.currentPhase++;
        }
    } catch (std::exception & e) {
        try {
            writeFull(STDERR_FILENO,
                      "while setting up the child process: " +
                      (ctx.currentPhase < (ssize_t)ctx.phases.size() ?
                       "in phase " + ctx.phases[ctx.currentPhase].label + ": " : "") +
                      string(e.what()) + "\n");
        } catch (std::exception & e2) {
            _exit(1);
        }
        _exit(1);
    }
    abort(); /* Should never be reached */
}


int runChildSetupEntry(void *data)
{
    runChildSetup(* (SpawnContext *)data);
    return 1;
}


int cloneChild(CloneSpawnContext & ctx)
{
#if CLONE_ENABLED
    char stack[32 * 1024];
    /* Ensure proper alignment on the stack.  On aarch64, it has to be 16
       bytes.  */
    char *alignedStack = (char *)(((uintptr_t)stack + sizeof(stack) - 8) & ~(uintptr_t)0xf);
    int ret = clone(runChildSetupEntry, alignedStack, ctx.cloneFlags, (void *) &ctx);
    if(ret == -1)
        throw SysError("clone");
    return ret;
#else
    throw Error("clone not supported on this platform");
#endif
}


}
