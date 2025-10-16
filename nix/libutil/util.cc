#include "config.h"

#include "util.hh"
#include "affinity.hh"

#include <iostream>
#include <cerrno>
#include <cstdio>
#include <cstdlib>
#include <sstream>
#include <cstring>

#include <sys/wait.h>
#include <unistd.h>
#include <fcntl.h>
#include <limits.h>
#include <sys/socket.h>

#ifdef __APPLE__
#include <sys/syscall.h>
#endif

#ifdef __linux__
#include <sys/prctl.h>
#endif

#ifdef HAVE_LINUX_CLOSE_RANGE_H
# include <linux/close_range.h>
#endif


extern char * * environ;


namespace nix {


BaseError::BaseError(const FormatOrString & fs, unsigned int status)
    : status(status)
{
    err = fs.s;
}


BaseError & BaseError::addPrefix(const FormatOrString & fs)
{
    prefix_ = fs.s + prefix_;
    return *this;
}


SysError::SysError(const FormatOrString & fs)
    : Error(format("%1%: %2%") % fs.s % strerror(errno))
    , errNo(errno)
{
}


string getEnv(const string & key, const string & def)
{
    char * value = getenv(key.c_str());
    return value ? string(value) : def;
}


string findProgram(const string & program)
{
    if(program.empty()) return "";

    if(program[0] == '/') return pathExists(program) ? program : "";

    char *path_ = getenv("PATH");
    if(path_ == NULL) return "";
    string path = path_;

    Strings dirs = tokenizeString<Strings>(path, ":");
    for (const auto& i : dirs) {
        if(i == "") continue;
        string f = i + "/" + program;
        if(pathExists(f)) return f;
    }

    return "";
}


Path absPath(Path path, Path dir)
{
    if (path[0] != '/') {
        if (dir == "") {
#ifdef __GNU__
            /* GNU (aka. GNU/Hurd) doesn't have any limitation on path
               lengths and doesn't define `PATH_MAX'.  */
            char *buf = getcwd(NULL, 0);
            if (buf == NULL)
#else
            char buf[PATH_MAX];
            if (!getcwd(buf, sizeof(buf)))
#endif
                throw SysError("cannot get cwd");
            dir = buf;
#ifdef __GNU__
            free(buf);
#endif
        }
        path = dir + "/" + path;
    }
    return canonPath(path);
}


Path canonPath(const Path & path, bool resolveSymlinks)
{
    string s;

    if (path[0] != '/')
        throw Error(format("not an absolute path: `%1%'") % path);

    string::const_iterator i = path.begin(), end = path.end();
    string temp;

    /* Count the number of times we follow a symlink and stop at some
       arbitrary (but high) limit to prevent infinite loops. */
    unsigned int followCount = 0, maxFollow = 1024;

    while (1) {

        /* Skip slashes. */
        while (i != end && *i == '/') i++;
        if (i == end) break;

        /* Ignore `.'. */
        if (*i == '.' && (i + 1 == end || i[1] == '/'))
            i++;

        /* If `..', delete the last component. */
        else if (*i == '.' && i + 1 < end && i[1] == '.' &&
            (i + 2 == end || i[2] == '/'))
        {
            if (!s.empty()) s.erase(s.rfind('/'));
            i += 2;
        }

        /* Normal component; copy it. */
        else {
            s += '/';
            while (i != end && *i != '/') s += *i++;

            /* If s points to a symlink, resolve it and restart (since
               the symlink target might contain new symlinks). */
            if (resolveSymlinks && isLink(s)) {
                if (++followCount >= maxFollow)
                    throw Error(format("infinite symlink recursion in path `%1%'") % path);
                temp = absPath(readLink(s), dirOf(s))
                    + string(i, end);
                i = temp.begin(); /* restart */
                end = temp.end();
                s = "";
            }
        }
    }

    return s.empty() ? "/" : s;
}


Path dirOf(const Path & path)
{
    Path::size_type pos = path.rfind('/');
    if (pos == string::npos)
        throw Error(format("invalid file name `%1%'") % path);
    return pos == 0 ? "/" : Path(path, 0, pos);
}


string baseNameOf(const Path & path)
{
    Path::size_type pos = path.rfind('/');
    if (pos == string::npos)
        throw Error(format("invalid file name `%1%'") % path);
    return string(path, pos + 1);
}


bool isInDir(const Path & path, const Path & dir)
{
    return path[0] == '/'
        && string(path, 0, dir.size()) == dir
        && path.size() >= dir.size() + 2
        && path[dir.size()] == '/';
}


struct stat lstat(const Path & path)
{
    struct stat st;
    if (lstat(path.c_str(), &st))
        throw SysError(format("getting status of `%1%'") % path);
    return st;
}


bool pathExists(const Path & path)
{
    int res;
#ifdef HAVE_STATX
    struct statx st;
    res = statx(AT_FDCWD, path.c_str(), AT_SYMLINK_NOFOLLOW, 0, &st);
#else
    struct stat st;
    res = lstat(path.c_str(), &st);
#endif
    if (!res) return true;
    if (errno != ENOENT && errno != ENOTDIR)
        throw SysError(format("getting status of %1%") % path);
    return false;
}


Path readLink(const Path & path)
{
    checkInterrupt();
    struct stat st = lstat(path);
    if (!S_ISLNK(st.st_mode))
        throw Error(format("`%1%' is not a symlink") % path);
    std::vector<char> buf(st.st_size);
    ssize_t rlsize = readlink(path.c_str(), buf.data(), st.st_size);
    if (rlsize == -1)
        throw SysError(format("reading symbolic link '%1%'") % path);
    else if (rlsize > st.st_size)
        throw Error(format("symbolic link ‘%1%’ size overflow %2% > %3%")
            % path % rlsize % st.st_size);
    return string(buf.begin(), buf.end());
}


bool isLink(const Path & path)
{
    struct stat st = lstat(path);
    return S_ISLNK(st.st_mode);
}


static DirEntries readDirectory(DIR *dir)
{
    DirEntries entries;
    entries.reserve(64);

    struct dirent * dirent;
    while (errno = 0, dirent = readdir(dir)) { /* sic */
        checkInterrupt();
        string name = dirent->d_name;
        if (name == "." || name == "..") continue;
        entries.emplace_back(name, dirent->d_ino, dirent->d_type);
    }
    if (errno) throw SysError(format("reading directory"));

    return entries;
}

DirEntries readDirectory(const Path & path)
{
    AutoCloseDir dir = opendir(path.c_str());
    if (!dir) throw SysError(format("opening directory `%1%'") % path);
    return readDirectory(dir);
}

static DirEntries readDirectory(int fd)
{
    /* Since 'closedir' closes the underlying file descriptor, duplicate FD
       beforehand.  */
    int fdcopy = dup(fd);
    if (fdcopy < 0) throw SysError("dup");

    AutoCloseDir dir = fdopendir(fdcopy);
    if (!dir) throw SysError(format("opening directory from file descriptor `%1%'") % fd);
    return readDirectory(dir);
}

unsigned char getFileType(const Path & path)
{
    struct stat st = lstat(path);
    if (S_ISDIR(st.st_mode)) return DT_DIR;
    if (S_ISLNK(st.st_mode)) return DT_LNK;
    if (S_ISREG(st.st_mode)) return DT_REG;
    return DT_UNKNOWN;
}


string readFile(int fd)
{
    struct stat st;
    if (fstat(fd, &st) == -1)
        throw SysError("statting file");

    std::vector<unsigned char> buf(st.st_size);
    readFull(fd, buf.data(), buf.size());

    return string((char *) buf.data(), buf.size());
}


string readFile(const Path & path, bool drain)
{
    AutoCloseFD fd = open(path.c_str(), O_RDONLY);
    if (fd == -1)
        throw SysError(format("reading file `%1%'") % path);
    return drain ? drainFD(fd) : readFile(fd);
}


void writeFile(const Path & path, const string & s)
{
    AutoCloseFD fd = open(path.c_str(), O_WRONLY | O_TRUNC | O_CREAT, 0666);
    if (fd == -1)
        throw SysError(format("writing file '%1%'") % path);
    writeFull(fd, s);
}


string readLine(int fd)
{
    string s;
    while (1) {
        checkInterrupt();
        char ch;
        ssize_t rd = read(fd, &ch, 1);
        if (rd == -1) {
            if (errno != EINTR)
                throw SysError("reading a line");
        } else if (rd == 0)
            throw EndOfFile("unexpected EOF reading a line");
        else {
            if (ch == '\n') return s;
            s += ch;
        }
    }
}


void writeLine(int fd, string s)
{
    s += '\n';
    writeFull(fd, s);
}


static void _deletePathAt(int fd, const Path & path, const Path & fullPath, unsigned long long & bytesFreed, size_t linkThreshold)
{
    checkInterrupt();

    printMsg(lvlVomit, format("%1%") % fullPath);

#ifdef HAVE_STATX
# define st_mode stx_mode
# define st_size stx_size
# define st_nlink stx_nlink
#define fstatat(fd, path, stat, flags)           \
    statx(fd, path, flags, STATX_SIZE | STATX_NLINK | STATX_MODE, stat)
#define fstat(fd, stat)   \
    statx(fd, "", AT_EMPTY_PATH, STATX_SIZE | STATX_NLINK | STATX_MODE, stat)
    struct statx st;
#else
    struct stat st;
#endif
    if (fstatat(fd, path.c_str(), &st, AT_SYMLINK_NOFOLLOW))
        throw SysError(format("getting status of `%1%'") % fullPath);

    /* Note: if another process modifies what is at 'path' between now and
       when we actually delete it, this may be inaccurate, but I know of no
       way to check which file we actually deleted after the fact. */
    if (!S_ISDIR(st.st_mode) && st.st_nlink <= linkThreshold)
	bytesFreed += st.st_size;

    if (S_ISDIR(st.st_mode)) {
      /* Note: fds required scales with depth of directory nesting */
      AutoCloseFD dirfd = openat(fd, path.c_str(),
                                 O_RDONLY |
                                 O_DIRECTORY |
                                 O_NOFOLLOW |
                                 O_CLOEXEC);
      if(!dirfd.isOpen())
        throw SysError(format("opening `%1%'") % fullPath);

      /* st.st_mode may currently be from a different file than what we
         actually opened, get it straight from the file instead */
      if(fstat(dirfd, &st))
        throw SysError(format("re-getting status of `%1'") % fullPath);

      /* Make the directory writable. */
      if (!(st.st_mode & S_IWUSR)) {
        if (fchmod(dirfd, st.st_mode | S_IWUSR) == -1)
          throw SysError(format("making `%1%' writable") % fullPath);
      }

      for (auto & i : readDirectory(dirfd))
        _deletePathAt(dirfd, i.name, path + "/" + i.name, bytesFreed, linkThreshold);
    }

    int ret;
    ret = unlinkat(fd, path.c_str(), S_ISDIR(st.st_mode) ? AT_REMOVEDIR : 0 );
    if (ret == -1)
        throw SysError(format("cannot unlink `%1%'") % fullPath);

#undef st_mode
#undef st_size
#undef st_nlink
#undef fstatat
#undef fstat
}

static void _deletePath(const Path & path, unsigned long long & bytesFreed, size_t linkThreshold)
{
  _deletePathAt(AT_FDCWD, path, path, bytesFreed, linkThreshold);
}


void deletePath(const Path & path)
{
    unsigned long long dummy;
    deletePath(path, dummy);
}


void deletePath(const Path & path, unsigned long long & bytesFreed, size_t linkThreshold)
{
    startNest(nest, lvlDebug,
        format("recursively deleting path `%1%'") % path);
    bytesFreed = 0;
    _deletePath(path, bytesFreed, linkThreshold);
}

static void copyFile(int sourceFd, int destinationFd)
{
    struct stat st;
    if (fstat(sourceFd, &st) == -1) throw SysError("statting file");

    ssize_t result = copy_file_range(sourceFd, NULL, destinationFd, NULL, st.st_size, 0);
    if (result < 0 && errno == ENOSYS) {
	for (size_t remaining = st.st_size; remaining > 0; ) {
	    unsigned char buf[8192];
	    size_t count = std::min(remaining, sizeof buf);

	    readFull(sourceFd, buf, count);
	    writeFull(destinationFd, buf, count);
	    remaining -= count;
	}
    } else {
	if (result < 0)
	    throw SysError(format("copy_file_range `%1%' to `%2%'") % sourceFd % destinationFd);

	/* If 'copy_file_range' copied less than requested, try again.  */
	for (ssize_t copied = result; copied < st.st_size; copied += result) {
	    result = copy_file_range(sourceFd, NULL, destinationFd, NULL,
				     st.st_size - copied, 0);
	    if (result < 0)
		throw SysError(format("copy_file_range `%1%' to `%2%'") % sourceFd % destinationFd);
	}
    }
}

static void copyFileRecursively(int sourceroot, const Path &source,
				int destinationroot, const Path &destination,
				bool deleteSource)
{
    struct stat st;
    if (fstatat(sourceroot, source.c_str(), &st, AT_SYMLINK_NOFOLLOW) == -1)
	throw SysError(format("statting file `%1%'") % source);

    if (S_ISREG(st.st_mode)) {
	AutoCloseFD sourceFd = openat(sourceroot, source.c_str(),
				      O_CLOEXEC | O_NOFOLLOW | O_RDONLY);
	if (sourceFd == -1) throw SysError(format("opening `%1%'") % source);

	AutoCloseFD destinationFd = openat(destinationroot, destination.c_str(),
					   O_CLOEXEC | O_CREAT | O_WRONLY | O_TRUNC
					   | O_NOFOLLOW | O_EXCL,
					   st.st_mode);
	if (destinationFd == -1) throw SysError(format("opening `%1%'") % source);

	copyFile(sourceFd, destinationFd);
	fchown(destinationFd, st.st_uid, st.st_gid);
    } else if (S_ISLNK(st.st_mode)) {
    std::vector<char> target(st.st_size + 1);
	ssize_t result = readlinkat(sourceroot, source.c_str(), target.data(), st.st_size);
	if (result != st.st_size) throw SysError("reading symlink target");
	target[st.st_size] = '\0';
	int err = symlinkat(target.data(), destinationroot, destination.c_str());
	if (err != 0)
	    throw SysError(format("creating symlink `%1%'") % destination);
	fchownat(destinationroot, destination.c_str(),
		 st.st_uid, st.st_gid, AT_SYMLINK_NOFOLLOW);
    } else if (S_ISDIR(st.st_mode)) {
	int err = mkdirat(destinationroot, destination.c_str(), 0755);
	if (err != 0)
	    throw SysError(format("creating directory `%1%'") % destination);

	AutoCloseFD destinationFd = openat(destinationroot, destination.c_str(),
					   O_CLOEXEC | O_RDONLY | O_DIRECTORY
					   | O_NOFOLLOW);
	if (err != 0)
	    throw SysError(format("opening directory `%1%'") % destination);

	AutoCloseFD sourceFd = openat(sourceroot, source.c_str(),
				      O_CLOEXEC | O_NOFOLLOW | O_RDONLY);
	if (sourceFd == -1)
	    throw SysError(format("opening `%1%'") % source);

        if (deleteSource && !(st.st_mode & S_IWUSR)) {
	    /* Ensure the directory is writable so files within it can be
	       deleted.  */
            if (fchmod(sourceFd, st.st_mode | S_IWUSR) == -1)
                throw SysError(format("making `%1%' directory writable") % source);
        }

        for (auto & i : readDirectory(sourceFd))
	    copyFileRecursively((int)sourceFd, i.name, (int)destinationFd, i.name,
				deleteSource);
	fchown(destinationFd, st.st_uid, st.st_gid);
    } else throw Error(format("refusing to copy irregular file `%1%'") % source);

    if (deleteSource)
	unlinkat(sourceroot, source.c_str(),
		 S_ISDIR(st.st_mode) ? AT_REMOVEDIR : 0);
}

void copyFileRecursively(const Path &source, const Path &destination, bool deleteSource)
{
    copyFileRecursively(AT_FDCWD, source, AT_FDCWD, destination, deleteSource);
}

static Path tempName(Path tmpRoot, const Path & prefix, bool includePid,
    int & counter)
{
    tmpRoot = canonPath(tmpRoot.empty() ? getEnv("TMPDIR", "/tmp") : tmpRoot, true);
    if (includePid)
        return (format("%1%/%2%-%3%-%4%") % tmpRoot % prefix % getpid() % counter++).str();
    else
        return (format("%1%/%2%-%3%") % tmpRoot % prefix % counter++).str();
}


Path createTempDir(const Path & tmpRoot, const Path & prefix,
    bool includePid, bool useGlobalCounter, mode_t mode)
{
    static int globalCounter = 0;
    int localCounter = 0;
    int & counter(useGlobalCounter ? globalCounter : localCounter);

    while (1) {
        checkInterrupt();
        Path tmpDir = tempName(tmpRoot, prefix, includePid, counter);
        if (mkdir(tmpDir.c_str(), mode) == 0) {
            /* Explicitly set the group of the directory.  This is to
               work around around problems caused by BSD's group
               ownership semantics (directories inherit the group of
               the parent).  For instance, the group of /tmp on
               FreeBSD is "wheel", so all directories created in /tmp
               will be owned by "wheel"; but if the user is not in
               "wheel", then "tar" will fail to unpack archives that
               have the setgid bit set on directories. */
            if (chown(tmpDir.c_str(), (uid_t) -1, getegid()) != 0)
                throw SysError(format("setting group of directory `%1%'") % tmpDir);
            return tmpDir;
        }
        if (errno != EEXIST)
            throw SysError(format("creating directory `%1%'") % tmpDir);
    }
}


Paths createDirs(const Path & path)
{
    Paths created;
    if (path == "/") return created;

    struct stat st;
    if (lstat(path.c_str(), &st) == -1) {
        created = createDirs(dirOf(path));
        if (mkdir(path.c_str(), 0777) == -1 && errno != EEXIST)
            throw SysError(format("creating directory `%1%'") % path);
        st = lstat(path);
        created.push_back(path);
    }

    if (S_ISLNK(st.st_mode) && stat(path.c_str(), &st) == -1)
        throw SysError(format("statting symlink `%1%'") % path);

    if (!S_ISDIR(st.st_mode)) throw Error(format("`%1%' is not a directory") % path);

    return created;
}


void createSymlink(const Path & target, const Path & link)
{
    if (symlink(target.c_str(), link.c_str()))
        throw SysError(format("creating symlink from `%1%' to `%2%'") % link % target);
}


LogType logType = ltPretty;
Verbosity verbosity = lvlInfo;

static int nestingLevel = 0;


Nest::Nest()
{
    nest = false;
}


Nest::~Nest()
{
    close();
}


static string escVerbosity(Verbosity level)
{
    return std::to_string((int) level);
}


void Nest::open(Verbosity level, const FormatOrString & fs)
{
    if (level <= verbosity) {
        if (logType == ltEscapes)
            std::cerr << "\033[" << escVerbosity(level) << "p"
                      << fs.s << "\n";
        else
            printMsg_(level, fs);
        nest = true;
        nestingLevel++;
    }
}


void Nest::close()
{
    if (nest) {
        nestingLevel--;
        if (logType == ltEscapes)
            std::cerr << "\033[q";
        nest = false;
    }
}


void printMsg_(Verbosity level, const FormatOrString & fs)
{
    checkInterrupt();
    if (level > verbosity) return;
    string prefix;
    if (logType == ltPretty)
        for (int i = 0; i < nestingLevel; i++)
            prefix += "|   ";
    else if (logType == ltEscapes && level != lvlInfo)
        prefix = "\033[" + escVerbosity(level) + "s";
    string s = (format("%1%%2%\n") % prefix % fs.s).str();
    writeToStderr(s);
}


void warnOnce(bool & haveWarned, const FormatOrString & fs)
{
    if (!haveWarned) {
        printMsg(lvlError, format("warning: %1%") % fs.s);
        haveWarned = true;
    }
}


void writeToStderr(const string & s)
{
    try {
        if (_writeToStderr)
            _writeToStderr((const unsigned char *) s.data(), s.size());
        else
            writeFull(STDERR_FILENO, s);
    } catch (SysError & e) {
        /* Ignore failing writes to stderr if we're in an exception
           handler, otherwise throw an exception.  We need to ignore
           write errors in exception handlers to ensure that cleanup
           code runs to completion if the other side of stderr has
           been closed unexpectedly. */
        if (!std::uncaught_exception()) throw;
    }
}


void (*_writeToStderr) (const unsigned char * buf, size_t count) = 0;


void readFull(int fd, unsigned char * buf, size_t count)
{
    while (count) {
        checkInterrupt();
        ssize_t res = read(fd, (char *) buf, count);
        if (res == -1) {
            if (errno == EINTR) continue;
            throw SysError("reading from file");
        }
        if (res == 0) throw EndOfFile("unexpected end-of-file");
        count -= res;
        buf += res;
    }
}


void writeFull(int fd, const unsigned char * buf, size_t count)
{
    while (count) {
        checkInterrupt();
        ssize_t res = write(fd, (char *) buf, count);
        if (res == -1) {
            if (errno == EINTR) continue;
            throw SysError("writing to file");
        }
        count -= res;
        buf += res;
    }
}


void writeFull(int fd, const string & s)
{
    writeFull(fd, (const unsigned char *) s.data(), s.size());
}


string drainFD(int fd)
{
    string result;
    unsigned char buffer[4096];
    while (1) {
        checkInterrupt();
        ssize_t rd = read(fd, buffer, sizeof buffer);
        if (rd == -1) {
            if (errno != EINTR)
                throw SysError("reading from file");
        }
        else if (rd == 0) break;
        else result.append((char *) buffer, rd);
    }
    return result;
}


/* Wait on FD until MESSAGE has been read.  */
void waitForMessage(int fd, const string & message)
{
    string str(message.length(), '\0');
    readFull(fd, (unsigned char*)str.data(), message.length());
    if (str != message)
	throw Error(format("did not receive message '%1%' on file descriptor %2%")
	    % message % fd);
}



//////////////////////////////////////////////////////////////////////


AutoDelete::AutoDelete(const string & p, bool recursive) : path(p)
{
    del = true;
    this->recursive = recursive;
}

AutoDelete::~AutoDelete()
{
    try {
        if (del) {
            if (recursive)
                deletePath(path);
            else {
                if (remove(path.c_str()) == -1)
                    throw SysError(format("cannot unlink `%1%'") % path);
            }
        }
    } catch (...) {
        ignoreException();
    }
}

void AutoDelete::cancel()
{
    del = false;
}



//////////////////////////////////////////////////////////////////////


AutoCloseFD::AutoCloseFD()
{
    fd = -1;
}


AutoCloseFD::AutoCloseFD(int fd)
{
    this->fd = fd;
}


AutoCloseFD::AutoCloseFD(const AutoCloseFD & fd)
{
    /* Copying an AutoCloseFD isn't allowed (who should get to close
       it?).  But as an edge case, allow copying of closed
       AutoCloseFDs.  This is necessary due to tiresome reasons
       involving copy constructor use on default object values in STL
       containers (like when you do `map[value]' where value isn't in
       the map yet). */
    this->fd = fd.fd;
    if (this->fd != -1) abort();
}


AutoCloseFD::~AutoCloseFD()
{
    try {
        close();
    } catch (...) {
        ignoreException();
    }
}


void AutoCloseFD::operator =(int fd)
{
    if (this->fd != fd) close();
    this->fd = fd;
}


AutoCloseFD::operator int() const
{
    return fd;
}


void AutoCloseFD::close()
{
    if (fd != -1) {
        if (::close(fd) == -1)
            /* This should never happen. */
            throw SysError(format("closing file descriptor %1%") % fd);
        fd = -1;
    }
}


bool AutoCloseFD::isOpen()
{
    return fd != -1;
}


/* Pass responsibility for closing this fd to the caller. */
int AutoCloseFD::borrow()
{
    int oldFD = fd;
    fd = -1;
    return oldFD;
}


void Pipe::create()
{
    int fds[2];
    if (pipe(fds) != 0) throw SysError("creating pipe");
    readSide = fds[0];
    writeSide = fds[1];
    closeOnExec(readSide);
    closeOnExec(writeSide);
}


void sendFD(int sock, int fd)
{
    ssize_t rc;
    struct msghdr msg;
    struct cmsghdr *cmsg;
    char cmsgbuf[CMSG_SPACE(sizeof(fd))];
    struct iovec iov;
    char dummy = '\0';
    memset(&msg, 0, sizeof(msg));
    iov.iov_base = &dummy;
    iov.iov_len = 1;
    msg.msg_iov = &iov;
    msg.msg_iovlen = 1;
    msg.msg_control = cmsgbuf;
    msg.msg_controllen = sizeof(cmsgbuf);
    cmsg = CMSG_FIRSTHDR(&msg);
    cmsg->cmsg_level = SOL_SOCKET;
    cmsg->cmsg_type = SCM_RIGHTS;
    cmsg->cmsg_len = CMSG_LEN(sizeof(fd));
    memcpy(CMSG_DATA(cmsg), &fd, sizeof(fd));
    msg.msg_controllen = cmsg->cmsg_len;
    do
    {
        rc = sendmsg(sock, &msg, 0);
    } while(rc < 0 && errno == EINTR);
    if(rc < 0)
        throw SysError("sending fd");
}


int receiveFD(int sock)
{
    int fd;
    ssize_t rc;
    struct msghdr msg;
    struct cmsghdr *cmsg;
    char cmsgbuf[CMSG_SPACE(sizeof(fd))];
    struct iovec iov;
    char dummy = '\0';
    memset(&msg, 0, sizeof(msg));
    iov.iov_base = &dummy;
    iov.iov_len = 1;
    msg.msg_iov = &iov;
    msg.msg_iovlen = 1;
    msg.msg_control = cmsgbuf;
    msg.msg_controllen = sizeof(cmsgbuf);
    do
    {
        rc = recvmsg(sock, &msg, 0);
    } while(rc < 0 && errno == EINTR);
    if (rc < 0)
        throw SysError("receiving fd");
    if (rc == 0)
        throw Error("received EOF (empty message) while receiving fd");

    cmsg = CMSG_FIRSTHDR(&msg);
    if (cmsg == NULL || cmsg->cmsg_type != SCM_RIGHTS)
        throw Error("received message without an fd");
    memcpy(&fd, CMSG_DATA(cmsg), sizeof(fd));
    return fd;
}

//////////////////////////////////////////////////////////////////////


AutoCloseDir::AutoCloseDir()
{
    dir = 0;
}


AutoCloseDir::AutoCloseDir(DIR * dir)
{
    this->dir = dir;
}


AutoCloseDir::~AutoCloseDir()
{
    close();
}


void AutoCloseDir::operator =(DIR * dir)
{
    this->dir = dir;
}


AutoCloseDir::operator DIR *()
{
    return dir;
}


void AutoCloseDir::close()
{
    if (dir) {
        closedir(dir);
        dir = 0;
    }
}


//////////////////////////////////////////////////////////////////////


Pid::Pid()
    : pid(-1), separatePG(false), killSignal(SIGKILL)
{
}


Pid::Pid(pid_t pid)
    : pid(pid), separatePG(false), killSignal(SIGKILL)
{
}


Pid::~Pid()
{
    kill();
}


void Pid::operator =(pid_t pid)
{
    if (this->pid != pid) kill();
    this->pid = pid;
    killSignal = SIGKILL; // reset signal to default
}


Pid::operator pid_t()
{
    return pid;
}


void Pid::kill(bool quiet)
{
    if (pid == -1 || pid == 0) return;

    if (!quiet)
        printMsg(lvlError, format("killing process %1%") % pid);

    /* Send the requested signal to the child.  If it has its own
       process group, send the signal to every process in the child
       process group (which hopefully includes *all* its children). */
    if (::kill(separatePG ? -pid : pid, killSignal) != 0)
        printMsg(lvlError, (SysError(format("killing process %1%") % pid).msg()));

    /* Wait until the child dies, disregarding the exit status. */
    int status;
    while (waitpid(pid, &status, 0) == -1) {
        checkInterrupt();
        if (errno != EINTR) {
            printMsg(lvlError,
                (SysError(format("waiting for process %1%") % pid).msg()));
            break;
        }
    }

    pid = -1;
}


int Pid::wait(bool block)
{
    assert(pid != -1);
    while (1) {
        int status;
        int res = waitpid(pid, &status, block ? 0 : WNOHANG);
        if (res == pid) {
            pid = -1;
            return status;
        }
        if (res == 0 && !block) return -1;
        if (errno != EINTR)
            throw SysError("cannot get child exit status");
        checkInterrupt();
    }
}


void Pid::setSeparatePG(bool separatePG)
{
    this->separatePG = separatePG;
}


void Pid::setKillSignal(int signal)
{
    this->killSignal = signal;
}


void killUser(uid_t uid)
{
    debug(format("killing all processes running under uid `%1%'") % uid);

    assert(uid != 0); /* just to be safe... */

    /* The system call kill(-1, sig) sends the signal `sig' to all
       users to which the current process can send signals.  So we
       fork a process, switch to uid, and send a mass kill. */

    Pid pid = startProcess([&]() {

        if (setuid(uid) == -1)
            throw SysError("setting uid");

        while (true) {
#ifdef __APPLE__
            /* OSX's kill syscall takes a third parameter that, among
               other things, determines if kill(-1, signo) affects the
               calling process. In the OSX libc, it's set to true,
               which means "follow POSIX", which we don't want here
                 */
            if (syscall(SYS_kill, -1, SIGKILL, false) == 0) break;
#elif __GNU__
            /* Killing all a user's processes using PID=-1 does currently
               not work on the Hurd.  */
            if (kill(getpid(), SIGKILL) == 0) break;
#else
            if (kill(-1, SIGKILL) == 0) break;
#endif
            if (errno == ESRCH) break; /* no more processes */
            if (errno != EINTR)
                throw SysError(format("cannot kill processes for uid `%1%'") % uid);
        }

        _exit(0);
    });

    int status = pid.wait(true);
#if __GNU__
    /* When the child killed itself, status = SIGKILL.  */
    if (status == SIGKILL) return;
#endif
    if (status != 0)
        throw Error(format("cannot kill processes for uid `%1%': %2%") % uid % statusToString(status));

    /* !!! We should really do some check to make sure that there are
       no processes left running under `uid', but there is no portable
       way to do so (I think).  The most reliable way may be `ps -eo
       uid | grep -q $uid'. */
}


//////////////////////////////////////////////////////////////////////


pid_t startProcess(std::function<void()> fun,
    bool dieWithParent, const string & errorPrefix, bool runExitHandlers)
{
    pid_t pid = fork();
    if (pid == -1) throw SysError("unable to fork");

    if (pid == 0) {
        _writeToStderr = 0;
        try {
#if __linux__
            if (dieWithParent && prctl(PR_SET_PDEATHSIG, SIGKILL) == -1)
                throw SysError("setting death signal");
#endif
            restoreAffinity();
            fun();
        } catch (std::exception & e) {
            try {
                std::cerr << errorPrefix << e.what() << "\n";
            } catch (...) { }
        } catch (...) { }
        if (runExitHandlers)
            exit(1);
        else
            _exit(1);
    }

    return pid;
}


std::vector<char *> stringsToCharPtrs(const Strings & ss)
{
    std::vector<char *> res;
    for (auto & s : ss) res.push_back((char *) s.c_str());
    res.push_back(0);
    return res;
}


string runProgram(Path program, bool searchPath, const Strings & args)
{
    checkInterrupt();

    /* Create a pipe. */
    Pipe pipe;
    pipe.create();

    /* Fork. */
    Pid pid = startProcess([&]() {
        if (dup2(pipe.writeSide, STDOUT_FILENO) == -1)
            throw SysError("dupping stdout");

        Strings args_(args);
        args_.push_front(program);

        if (searchPath)
            execvp(program.c_str(), stringsToCharPtrs(args_).data());
        else
            execv(program.c_str(), stringsToCharPtrs(args_).data());

	int err = errno;
        printMsg(lvlError, format("executing `%1%': %2%") % program % strerror(err));
	_exit(127);
    });

    pipe.writeSide.close();

    string result = drainFD(pipe.readSide);

    /* Wait for the child to finish. */
    int status = pid.wait(true);
    if (!statusOk(status))
        throw ExecError(format("program `%1%' %2%")
            % program % statusToString(status));

    return result;
}


void closeMostFDs(const set<int> & exceptions)
{
#ifdef HAVE_CLOSE_RANGE
    if (exceptions.empty())
	 close_range(3, ~0U, 0);
    else
#endif
    {
	 int maxFD = 0;
	 maxFD = sysconf(_SC_OPEN_MAX);
	 for (int fd = 0; fd < maxFD; ++fd)
	      if (fd != STDIN_FILENO && fd != STDOUT_FILENO && fd != STDERR_FILENO
		  && exceptions.find(fd) == exceptions.end())
		   close(fd); /* ignore result */
    }
}


void closeOnExec(int fd)
{
    int prev;
    if ((prev = fcntl(fd, F_GETFD, 0)) == -1 ||
        fcntl(fd, F_SETFD, prev | FD_CLOEXEC) == -1)
        throw SysError("setting close-on-exec flag");
}

void keepOnExec(int fd)
{
    int prev;
    if ((prev = fcntl(fd, F_GETFD, 0)) == -1 ||
        fcntl(fd, F_SETFD, prev & ~FD_CLOEXEC) == -1)
        throw SysError("clearing close-on-exec flag");
}

//////////////////////////////////////////////////////////////////////


volatile sig_atomic_t _isInterrupted = 0;

void _interrupted()
{
    /* Block user interrupts while an exception is being handled.
       Throwing an exception while another exception is being handled
       kills the program! */
    if (!std::uncaught_exception()) {
        _isInterrupted = 0;
        throw Interrupted("interrupted by the user");
    }
}



//////////////////////////////////////////////////////////////////////


template<class C> C tokenizeString(const string & s, const string & separators)
{
    C result;
    string::size_type pos = s.find_first_not_of(separators, 0);
    while (pos != string::npos) {
        string::size_type end = s.find_first_of(separators, pos + 1);
        if (end == string::npos) end = s.size();
        string token(s, pos, end - pos);
        result.insert(result.end(), token);
        pos = s.find_first_not_of(separators, end);
    }
    return result;
}

template Strings tokenizeString(const string & s, const string & separators);
template StringSet tokenizeString(const string & s, const string & separators);
template vector<string> tokenizeString(const string & s, const string & separators);


string concatStringsSep(const string & sep, const Strings & ss)
{
    string s;
    for (const auto& i : ss) {
        if (s.size() != 0) s += sep;
        s += i;
    }
    return s;
}


string concatStringsSep(const string & sep, const StringSet & ss)
{
    string s;
    for (const auto& i : ss) {
        if (s.size() != 0) s += sep;
        s += i;
    }
    return s;
}


string chomp(const string & s)
{
    size_t i = s.find_last_not_of(" \n\r\t");
    return i == string::npos ? "" : string(s, 0, i + 1);
}


string statusToString(int status)
{
    if (!WIFEXITED(status) || WEXITSTATUS(status) != 0) {
        if (WIFEXITED(status))
            return (format("failed with exit code %1%") % WEXITSTATUS(status)).str();
        else if (WIFSIGNALED(status)) {
            int sig = WTERMSIG(status);
#if HAVE_STRSIGNAL
            const char * description = strsignal(sig);
            return (format("failed due to signal %1% (%2%)") % sig % description).str();
#else
            return (format("failed due to signal %1%") % sig).str();
#endif
        }
        else
            return "died abnormally";
    } else return "succeeded";
}


bool statusOk(int status)
{
    return WIFEXITED(status) && WEXITSTATUS(status) == 0;
}


bool hasSuffix(const string & s, const string & suffix)
{
    return s.size() >= suffix.size() && string(s, s.size() - suffix.size()) == suffix;
}


void expect(std::istream & str, const string & s)
{
    std::vector<char> s2(s.size());
    str.read(s2.data(), s2.size());
    if (string(s2.begin(), s2.end()) != s)
        throw FormatError(format("expected string `%1%'") % s);
}


string parseString(std::istream & str)
{
    string res;
    expect(str, "\"");
    int c;
    while ((c = str.get()) != '"')
        if (c == '\\') {
            c = str.get();
            if (c == 'n') res += '\n';
            else if (c == 'r') res += '\r';
            else if (c == 't') res += '\t';
            else res += c;
        }
        else res += c;
    return res;
}


bool endOfList(std::istream & str)
{
    if (str.peek() == ',') {
        str.get();
        return false;
    }
    if (str.peek() == ']') {
        str.get();
        return true;
    }
    return false;
}

string decodeOctalEscaped(const string & s)
{
    string r;
    for (string::const_iterator i = s.begin(); i != s.end(); ) {
        if (*i != '\\') { r += *(i++); continue; }
        unsigned char c = 0;
        ++i;
        for(int j = 0; j < 3; j++) {
          if(i == s.end() || *i < '0' || *i >= '8')
            throw Error("malformed octal escape");
          c = c * 8 + (*i - '0');
          ++i;
        }
        r += c;
    }
    return r;
}


void ignoreException()
{
    try {
        throw;
    } catch (std::exception & e) {
        printMsg(lvlError, format("error (ignored): %1%") % e.what());
    }
}

static const string pathNullDevice = "/dev/null";

/* Common initialisation performed in child processes. */
void commonChildInit(Pipe & logPipe)
{
    /* Put the child in a separate session (and thus a separate
       process group) so that it has no controlling terminal (meaning
       that e.g. ssh cannot open /dev/tty) and it doesn't receive
       terminal signals. */
    if (setsid() == -1)
        throw SysError(format("creating a new session"));

    /* Close the read end so only the parent holds a reference to it.  */
    logPipe.readSide.close();

    /* Dup the write side of the logger pipe into stderr. */
    if (dup2(logPipe.writeSide, STDERR_FILENO) == -1)
        throw SysError("cannot pipe standard error into log file");

    /* Dup stderr to stdout. */
    if (dup2(STDERR_FILENO, STDOUT_FILENO) == -1)
        throw SysError("cannot dup stderr into stdout");

    /* Reroute stdin to /dev/null. */
    int fdDevNull = open(pathNullDevice.c_str(), O_RDWR);
    if (fdDevNull == -1)
        throw SysError(format("cannot open `%1%'") % pathNullDevice);
    if (dup2(fdDevNull, STDIN_FILENO) == -1)
        throw SysError("cannot dup null device into stdin");
    close(fdDevNull);
}

//////////////////////////////////////////////////////////////////////

Agent::Agent(const string &command, const Strings &args, const std::map<string, string> &env)
{
    debug(format("starting agent '%1%'") % command);

    /* Create a pipe to get the output of the child. */
    fromAgent.create();

    /* Create the communication pipes. */
    toAgent.create();

    /* Create a pipe to get the output of the builder. */
    builderOut.create();

    /* Fork the hook. */
    pid = startProcess([&]() {

        commonChildInit(fromAgent);

	for (auto pair: env) {
	    setenv(pair.first.c_str(), pair.second.c_str(), 1);
	}

        if (chdir("/") == -1) throw SysError("changing into `/");

        /* Dup the communication pipes. */
        if (dup2(toAgent.readSide, STDIN_FILENO) == -1)
            throw SysError("dupping to-hook read side");

        /* Use fd 4 for the builder's stdout/stderr. */
        if (dup2(builderOut.writeSide, 4) == -1)
            throw SysError("dupping builder's stdout/stderr");

	Strings allArgs;
	allArgs.push_back(command);
	allArgs.insert(allArgs.end(), args.begin(), args.end()); // append

        execv(command.c_str(), stringsToCharPtrs(allArgs).data());

        throw SysError(format("executing `%1%'") % command);
    });

    pid.setSeparatePG(true);
    fromAgent.writeSide.close();
    toAgent.readSide.close();
}


Agent::~Agent()
{
    try {
        toAgent.writeSide.close();
        pid.kill(true);
    } catch (...) {
        ignoreException();
    }
}


}
