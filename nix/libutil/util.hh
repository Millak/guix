#pragma once

#include "types.hh"

#include <sstream>
#include <string_view>

#include <sys/types.h>
#include <sys/stat.h>
#include <dirent.h>
#include <unistd.h>
#include <signal.h>
#include <map>
#include <functional>

#include <cstdio>


namespace nix {


/* Return an environment variable. */
string getEnv(const string & key, const string & def = "");

/* Find the absolute filename corresponding to PROGRAM, searching PATH if
   PROGRAM is a relative filename.  If PROGRAM is an absolute filename for a
   file that doesn't exist, or it can't be found in PATH, then return the
   empty string. */
string findProgram(const string & program);

/* Return an absolutized path, resolving paths relative to the
   specified directory, or the current directory otherwise.  The path
   is also canonicalised. */
Path absPath(Path path, Path dir = "");

/* Canonicalise a path by removing all `.' or `..' components and
   double or trailing slashes.  Optionally resolves all symlink
   components such that each component of the resulting path is *not*
   a symbolic link. */
Path canonPath(const Path & path, bool resolveSymlinks = false);

/* Return the directory part of the given canonical path, i.e.,
   everything before the final `/'.  If the path is the root or an
   immediate child thereof (e.g., `/foo'), this means an empty string
   is returned. */
Path dirOf(const Path & path);

/* Return the base name of the given canonical path, i.e., everything
   following the final `/'. */
string baseNameOf(const Path & path);

/* Check whether a given path is a descendant of the given
   directory. */
bool isInDir(const Path & path, const Path & dir);

/* Get status of `path'. */
struct stat lstat(const Path & path);

/* Return true iff the given path exists. */
bool pathExists(const Path & path);

/* Read the contents (target) of a symbolic link.  The result is not
   in any way canonicalised. */
Path readLink(const Path & path);

bool isLink(const Path & path);

/* Read the contents of a directory.  The entries `.' and `..' are
   removed. */
struct DirEntry
{
    string name;
    ino_t ino;
    unsigned char type; // one of DT_*
    DirEntry(const string & name, ino_t ino, unsigned char type)
        : name(name), ino(ino), type(type) { }
};

typedef vector<DirEntry> DirEntries;

DirEntries readDirectory(const Path & path);

unsigned char getFileType(const Path & path);

/* Read the contents of a file into a string. */
string readFile(int fd);
string readFile(const Path & path, bool drain = false);

/* Write a string to a file. */
void writeFile(const Path & path, const string & s);

/* Read a line from a file descriptor. */
string readLine(int fd);

/* Write a line to a file descriptor. */
void writeLine(int fd, string s);

/* Delete a path; i.e., in the case of a directory, it is deleted
   recursively.  Don't use this at home, kids.  The second variant
   returns the number of bytes and blocks freed, and 'linkThreshold' denotes
   the number of links under which a file is accounted for in 'bytesFreed'.  */
void deletePath(const Path & path);

void deletePath(const Path & path, unsigned long long & bytesFreed,
    size_t linkThreshold = 1);

/* Copy SOURCE to DESTINATION, recursively, preserving ownership.  Throw if
   SOURCE contains a file that is not a regular file, symlink, or directory.
   When DELETESOURCE is true, delete source files once they have been
   copied.  */
void copyFileRecursively(const Path &source, const Path &destination,
    bool deleteSource = false);

/* Create a temporary directory. */
Path createTempDir(const Path & tmpRoot = "", const Path & prefix = "nix",
    bool includePid = true, bool useGlobalCounter = true, mode_t mode = 0755);

/* Create a directory and all its parents, if necessary.  Returns the
   list of created directories, in order of creation. */
Paths createDirs(const Path & path);

/* Create a symlink. */
void createSymlink(const Path & target, const Path & link);


/* Messages. */


typedef enum {
    ltPretty,   /* nice, nested output */
    ltEscapes,  /* nesting indicated using escape codes (for log2xml) */
    ltFlat      /* no nesting */
} LogType;

extern LogType logType;
extern Verbosity verbosity; /* suppress msgs > this */

class Nest
{
private:
    bool nest;
public:
    Nest();
    ~Nest();
    void open(Verbosity level, std::string_view fs);
    void close();
};

void printMsg_(Verbosity level, std::string_view fs);

#define startNest(varName, level, f) \
    Nest varName; \
    if (level <= verbosity) { \
      varName.open(level, (f)); \
    }

#define printMsg(level, f) \
    do { \
        if (level <= nix::verbosity) { \
            nix::printMsg_(level, (f)); \
        } \
    } while (0)

#define debug(f) printMsg(lvlDebug, f)

void warnOnce(bool & haveWarned, std::string_view fs);

void writeToStderr(const string & s);

extern void (*_writeToStderr) (const unsigned char * buf, size_t count);


/* Wrappers arount read()/write() that read/write exactly the
   requested number of bytes. */
void readFull(int fd, unsigned char * buf, size_t count);
void writeFull(int fd, const unsigned char * buf, size_t count);
void writeFull(int fd, const string & s);

MakeError(EndOfFile, Error)


/* Read a file descriptor until EOF occurs. */
string drainFD(int fd);

void waitForMessage(int fd, const string & message);



/* Automatic cleanup of resources. */


class AutoDelete
{
    Path path;
    bool del;
    bool recursive;
public:
    AutoDelete(const Path & p, bool recursive = true);
    ~AutoDelete();
    void cancel();
};


class AutoCloseFD
{
    int fd;
public:
    AutoCloseFD();
    AutoCloseFD(int fd);
    AutoCloseFD(const AutoCloseFD & fd);
    ~AutoCloseFD();
    void operator =(int fd);
    operator int() const;
    void close();
    bool isOpen();
    int borrow();
};

/* Send and receive an FD on a unix-domain socket, along with a single null
   byte of regular data. */
void sendFD(int sock, int fd);
int receiveFD(int sock);

class Pipe
{
public:
    AutoCloseFD readSide, writeSide;
    void create();
};


class AutoCloseDir
{
    DIR * dir;
public:
    AutoCloseDir();
    AutoCloseDir(DIR * dir);
    ~AutoCloseDir();
    void operator =(DIR * dir);
    operator DIR *();
    void close();
};


class Pid
{
    pid_t pid;
    bool separatePG;
    int killSignal;
public:
    Pid();
    Pid(pid_t pid);
    ~Pid();
    void operator =(pid_t pid);
    operator pid_t();
    void kill(bool quiet = false);
    int wait(bool block);
    void setSeparatePG(bool separatePG);
    void setKillSignal(int signal);
};

/* An "agent" is a helper program that runs in the background and that we talk
   to over pipes, such as the "guix offload" program.  */
struct Agent
{
    /* Pipes for talking to the agent. */
    Pipe toAgent;

    /* Pipe for the agent's standard output/error. */
    Pipe fromAgent;

    /* Pipe for build standard output/error--e.g., for build processes started
       by "guix offload".  */
    Pipe builderOut;

    /* The process ID of the agent. */
    Pid pid;

    /* The command and arguments passed to the agent along with a list of
       environment variable name/value pairs.  */
    Agent(const string &command, const Strings &args,
	  const std::map<string, string> &env = std::map<string, string>());

    ~Agent();
};


/* Kill all processes running under the specified uid by sending them
   a SIGKILL. */
void killUser(uid_t uid);


/* Fork a process that runs the given function, and return the child
   pid to the caller. */
pid_t startProcess(std::function<void()> fun, bool dieWithParent = true,
    const string & errorPrefix = "error: ", bool runExitHandlers = false);


/* Run a program and return its stdout in a string (i.e., like the
   shell backtick operator). */
string runProgram(Path program, bool searchPath = false,
    const Strings & args = Strings());

MakeError(ExecError, Error)

/* Convert a list of strings to a null-terminated vector of char
   *'s. The result must not be accessed beyond the lifetime of the
   list of strings. */
std::vector<char *> stringsToCharPtrs(const Strings & ss);

/* Close all file descriptors except stdin, stdout, stderr, and those
   listed in the given set.  Good practice in child processes. */
void closeMostFDs(const set<int> & exceptions);

/* Set the close-on-exec flag for the given file descriptor. */
void closeOnExec(int fd);

/* Clear the close-on-exec flag for the given file descriptor.  */
void keepOnExec(int fd);

/* Common initialisation performed in child processes. */
void commonChildInit(Pipe & logPipe);

/* User interruption. */

extern volatile sig_atomic_t _isInterrupted;

void _interrupted();

void inline checkInterrupt()
{
    if (_isInterrupted) _interrupted();
}

MakeError(Interrupted, BaseError)


/* String tokenizer. */
template<class C> C tokenizeString(const string & s, const string & separators = " \t\n\r");


/* Concatenate the given strings with a separator between the
   elements. */
string concatStringsSep(const string & sep, const Strings & ss);
string concatStringsSep(const string & sep, const StringSet & ss);


/* Remove trailing whitespace from a string. */
string chomp(const string & s);


/* Convert the exit status of a child as returned by wait() into an
   error string. */
string statusToString(int status);

bool statusOk(int status);


/* Parse a string into an integer. */
template<class N> bool string2Int(const string & s, N & n)
{
    std::istringstream str(s);
    str >> n;
    return str && str.get() == EOF;
}


/* Return true iff `s' ends in `suffix'. */
bool hasSuffix(const string & s, const string & suffix);


/* Read string `s' from stream `str'. */
void expect(std::istream & str, std::string_view s);

MakeError(FormatError, Error)


/* Read a C-style string from stream `str'. */
string parseString(std::istream & str);


/* Utility function used to parse legacy ATerms. */
bool endOfList(std::istream & str);


/* Escape a string that contains octal-encoded escape codes such as
   used in /etc/fstab and /proc/mounts (e.g. "foo\040bar" decodes to
   "foo bar"). */
string decodeOctalEscaped(const string & s);


/* Exception handling in destructors: print an error message, then
   ignore the exception. */
void ignoreException();


}
