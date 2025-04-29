#pragma once

#include <util.hh>
#include <map>
#include <stddef.h>
#ifdef __linux__
#include <linux/filter.h>
#endif

namespace nix {
struct SpawnContext; /* Forward declaration */
typedef void (Action)(SpawnContext & ctx);

struct Phase {
    string label;
    Action * action;
};

typedef std::vector<Phase> Phases;

/* Common structure read from / written to by setup phases in a newly-spawned
   child process.  Configure this to determine which per-process or
   per-thread attributes should be set. */
struct SpawnContext {
    ssize_t currentPhase = 0;
    Phases phases;
    Strings args; /* Will be passed as-is to execve, does not implicitly add
                   * program basename as argv[0]! */
    Path program;
    bool inheritEnv = true; /* True to use the current environment after env
                             * has been applied to it, false to use strictly
                             * env. */
    std::map<string, string> env;
    bool setPersona = false;
    int persona;
    int logFD = -1; /* -1 to keep stdout and stderr */
    set<int> earlyCloseFDs; /* Typically for closing inherited unused pipe or
                             * socket ends to prevent hangs when reading or
                             * writing. */
    bool closeMostFDs = false;
    set<int> preserveFDs; /* 0, 1, and 2 are always implicitly preserved. */
    bool setStdin = false;
    int stdinFD = -1; /* fd or -1 */
    Path stdinFile; /* used if stdinFD == -1 */
    bool setuid = false;
    uid_t user;
    bool setgid = false;
    gid_t group;
    bool setSupplementaryGroups = false;
    std::vector<gid_t> supplementaryGroups;
    bool setsid = false;
    bool oomSacrifice = false; /* Whether to attempt to offer the child
                                * process to the OOM killer if possible. */
    bool setcwd = false;
    Path cwd;
    bool signalSetupSuccess = false; /* Whether the parent is waiting for a
                                      * message that setup succeeded.  By
                                      * default success is signaled by
                                      * writing a single newline to stderr. */
    bool dropAmbientCapabilities = false; /* Whether to drop ambient
                                           * capabilities if on a system that
                                           * supports them. */
    bool setNoNewPrivs = false;
    bool addSeccompFilter = false;
#if __linux__
    std::vector<struct sock_filter> seccompFilter;
#endif
    bool doChroot = false;
    Path chrootRootDir;
    void * extraData;  /* Extra user data */
};

/* Like SpawnContext, but with extra fields for setting up Linux namespaces,
   as created by clone or unshare. */
struct CloneSpawnContext : SpawnContext {
    int cloneFlags = 0;
    std::map<Path, Path> filesInChroot; /* map from path inside chroot to
                                         * path outside of chroot */
    set<Path> readOnlyFilesInChroot;
    bool mountTmpfsOnChroot = false; /* req. CLONE_NEWNS and doChroot */
    bool mountProc = false;
    bool mountDevshm = false;
    bool maybeMountDevpts = false; /* Only mounted if /dev/ptmx doesn't exist
                                    * after any chroot, if applicable. */
    bool lockMounts = false; /* Whether to lock mounts by creating a fresh
                              * user and mount namespace, see
                              * mount_namespaces(7). */
    bool lockMountsMapAll = false; /* Whether to map all currently-mapped
                                      users and groups when locking mounts or
                                      only the current ones. */
    bool lockMountsAllowSetgroups = false;
    int setupFD = -1; /* Used for userns init sync and other stuff */
    string hostname; /* Requires CLONE_NEWUTS */
    string domainname; /* Same */
    bool initLoopback = false; /* Also requires CLONE_NEWNET in cloneFlags */
    /* These may be used if CLONE_NEWUSER in cloneFlags.  These are to be
       used when an id other than the current uid/gid has been mapped into the
       child's user namespace, and it now needs to setuid/setgid to an id
       that is mapped. */
    bool usernsSetuid = false;
    uid_t usernsUser;
    bool usernsSetgid = false;
    gid_t usernsGroup;
};

void addPhaseAfter(Phases & phases, string afterLabel, string addLabel, Action addAction);

void addPhaseBefore(Phases & phases, string beforeLabel, string addLabel, Action addAction);

void prependPhase(Phases & phases, string addLabel, Action addAction);

void appendPhase(Phases & phases, string addLabel, Action addAction);

void deletePhase(Phases & phases, string delLabel);

void replacePhase(Phases & phases, string replaceLabel, Action newAction);

Action reset_writeToStderrAction;
Action restoreAffinityAction;
Action setsidAction;
Action earlyIOSetupAction;
Action dropAmbientCapabilitiesAction;
Action chrootAction;
Action chdirAction;
Action closeMostFDsAction;
Action setPersonalityAction;
Action oomSacrificeAction;
Action setIDsAction;
Action setNoNewPrivsAction;
Action addSeccompFilterAction;
Action restoreSIGPIPEAction;
Action setupSuccessAction;
Action execAction;

Phases getBasicSpawnPhases();

void bindMount(Path source, Path target, bool readOnly);

void mountIntoChroot(std::map<Path, Path> filesInChroot,
                     set<Path> readOnlyFiles,
                     Path chrootRootDir);

Action usernsInitSyncAction;
Action usernsSetIDsAction;
Action initLoopbackAction;
Action setHostAndDomainAction;
Action makeFilesystemsPrivateAction;
Action makeChrootSeparateFilesystemAction;
Action mountIntoChrootAction;
Action mountProcAction;
Action mountDevshmAction;
Action mountDevptsAction;
Action pivotRootAction;
Action lockMountsAction;

Phases getCloneSpawnPhases();

/* Helpers */
string idMapToIdentityMap(const string & map);
void unshareAndInitUserns(int flags, const string & uidMap,
                          const string & gidMap, bool allowSetgroups);

/* Run the phases of ctx in order, catching and reporting any exception, and
 * exiting in all cases. */
void runChildSetup(SpawnContext & ctx);

/* Helper to call runChildSetup that can be passed to the variant of clone
 * that expects a callback. */
int runChildSetupEntry(void *data);

/* Create a new process using clone that will immediately call runChildSetup
 * with the provided CloneSpawnContext.  Return the pid of the new process. */
int cloneChild(CloneSpawnContext & ctx);
}
