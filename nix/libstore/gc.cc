#include "globals.hh"
#include "misc.hh"
#include "local-store.hh"

#include <functional>
#include <queue>
#include <random>
#include <algorithm>
#include <format>

#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>
#include <fcntl.h>
#include <unistd.h>
#include <climits>


namespace nix {


static string gcLockName = "gc.lock";
static string tempRootsDir = "temproots";
static string gcRootsDir = "gcroots";


/* Acquire the global GC lock.  This is used to prevent new build
   processes from starting after the temporary root files have been
   read.  To be precise: when they try to create a new temporary root
   file, they will block until the garbage collector has finished /
   yielded the GC lock. */
int LocalStore::openGCLock(LockType lockType)
{
    Path fnGCLock = std::format("{}/{}", settings.nixStateDir, gcLockName);

    debug(std::format("acquiring global GC lock `{}'", fnGCLock));

    AutoCloseFD fdGCLock = open(fnGCLock.c_str(), O_RDWR | O_CREAT, 0600);
    if (fdGCLock == -1)
        throw SysError(std::format("opening global GC lock `{}'", fnGCLock));
    closeOnExec(fdGCLock);

    if (!lockFile(fdGCLock, lockType, false)) {
        printMsg(lvlError, "waiting for the big garbage collector lock...");
        lockFile(fdGCLock, lockType, true);
    }

    /* !!! Restrict read permission on the GC root.  Otherwise any
       process that can open the file for reading can DoS the
       collector. */

    return fdGCLock.borrow();
}


static void makeSymlink(const Path & link, const Path & target)
{
    /* Create directories up to `gcRoot'. */
    createDirs(dirOf(link));

    /* Create the new symlink. */
    Path tempLink = std::format("{}.tmp-{}-{}", link, getpid(), rand());
    createSymlink(target, tempLink);

    /* Atomically replace the old one. */
    if (rename(tempLink.c_str(), link.c_str()) == -1)
        throw SysError(std::format("cannot rename `{}' to `{}'", tempLink, link));
}


void LocalStore::syncWithGC()
{
    AutoCloseFD fdGCLock = openGCLock(ltRead);
}


void LocalStore::addIndirectRoot(const Path & path)
{
    string hash = printHash32(hashString(htSHA1, path));
    Path realRoot = canonPath(std::format("{}/{}/auto/{}",
        settings.nixStateDir, gcRootsDir, hash));
    makeSymlink(realRoot, path);
}


Path addPermRoot(StoreAPI & store, const Path & _storePath,
    const Path & _gcRoot, bool indirect, bool allowOutsideRootsDir)
{
    Path storePath(canonPath(_storePath));
    Path gcRoot(canonPath(_gcRoot));
    assertStorePath(storePath);

    if (isInStore(gcRoot))
        throw Error(std::format(
                "creating a garbage collector root ({}) in the store is forbidden "
                "(are you running nix-build inside the store?)", gcRoot));

    if (indirect) {
        /* Don't clobber the link if it already exists and doesn't
           point to the store. */
        if (pathExists(gcRoot) && (!isLink(gcRoot) || !isInStore(readLink(gcRoot))))
            throw Error(std::format("cannot create symlink `{}'; already exists", gcRoot));
        makeSymlink(gcRoot, storePath);
        store.addIndirectRoot(gcRoot);
    }

    else {
        if (!allowOutsideRootsDir) {
            Path rootsDir = canonPath(std::format("{}/{}", settings.nixStateDir, gcRootsDir));

            if (string(gcRoot, 0, rootsDir.size() + 1) != rootsDir + "/")
                throw Error(std::format(
                    "path `{}' is not a valid garbage collector root; "
                    "it's not in the directory `{}'",
                    gcRoot, rootsDir));
        }

        if (baseNameOf(gcRoot) == baseNameOf(storePath))
            writeFile(gcRoot, "");
        else
            makeSymlink(gcRoot, storePath);
    }

    /* Check that the root can be found by the garbage collector.
       !!! This can be very slow on machines that have many roots.
       Instead of reading all the roots, it would be more efficient to
       check if the root is in a directory in or linked from the
       gcroots directory. */
    if (settings.checkRootReachability) {
        Roots roots = store.findRoots();
        if (roots.find(gcRoot) == roots.end())
            printMsg(lvlError,
                std::format(
                    "warning: `{}' is not in a directory where the garbage collector looks for roots; "
                    "therefore, `{}' might be removed by the garbage collector",
                gcRoot, storePath));
    }

    /* Grab the global GC root, causing us to block while a GC is in
       progress.  This prevents the set of permanent roots from
       increasing while a GC is in progress. */
    store.syncWithGC();

    return gcRoot;
}


void LocalStore::addTempRoot(const Path & path)
{
    /* Create the temporary roots file for this process. */
    if (fdTempRoots == -1) {

        while (1) {
            Path dir = std::format("{}/{}", settings.nixStateDir, tempRootsDir);
            createDirs(dir);

            fnTempRoots = std::format("{}/{}", dir, getpid());

            AutoCloseFD fdGCLock = openGCLock(ltRead);

            if (pathExists(fnTempRoots))
                /* It *must* be stale, since there can be no two
                   processes with the same pid. */
                unlink(fnTempRoots.c_str());

            fdTempRoots = openLockFile(fnTempRoots, true);

            fdGCLock.close();

            debug(std::format("acquiring read lock on `{}'", fnTempRoots));
            lockFile(fdTempRoots, ltRead, true);

            /* Check whether the garbage collector didn't get in our
               way. */
            struct stat st;
            if (fstat(fdTempRoots, &st) == -1)
                throw SysError(std::format("statting `{}'", fnTempRoots));
            if (st.st_size == 0) break;

            /* The garbage collector deleted this file before we could
               get a lock.  (It won't delete the file after we get a
               lock.)  Try again. */
        }

    }

    /* Upgrade the lock to a write lock.  This will cause us to block
       if the garbage collector is holding our lock. */
    debug(std::format("acquiring write lock on `{}'", fnTempRoots));
    lockFile(fdTempRoots, ltWrite, true);

    string s = path + '\0';
    writeFull(fdTempRoots, s);

    /* Downgrade to a read lock. */
    debug(std::format("downgrading to read lock on `{}'", fnTempRoots));
    lockFile(fdTempRoots, ltRead, true);
}


typedef std::shared_ptr<AutoCloseFD> FDPtr;
typedef list<FDPtr> FDs;


static void readTempRoots(PathSet & tempRoots, FDs & fds)
{
    /* Read the `temproots' directory for per-process temporary root
       files. */
    DirEntries tempRootFiles = readDirectory(
        std::format("{}/{}", settings.nixStateDir, tempRootsDir));

    for (auto & i : tempRootFiles) {
        Path path = std::format("{}/{}/{}", settings.nixStateDir, tempRootsDir, i.name);

        debug(std::format("reading temporary root file `{}'", path));
        FDPtr fd(new AutoCloseFD(open(path.c_str(), O_RDWR, 0666)));
        if (*fd == -1) {
            /* It's okay if the file has disappeared. */
            if (errno == ENOENT) continue;
            throw SysError(std::format("opening temporary roots file `{}'", path));
        }

        /* This should work, but doesn't, for some reason. */
        //FDPtr fd(new AutoCloseFD(openLockFile(path, false)));
        //if (*fd == -1) continue;

        /* Try to acquire a write lock without blocking.  This can
           only succeed if the owning process has died.  In that case
           we don't care about its temporary roots. */
        if (lockFile(*fd, ltWrite, false)) {
            printMsg(lvlError, std::format("removing stale temporary roots file `{}'", path));
            unlink(path.c_str());
            writeFull(*fd, "d");
            continue;
        }

        /* Acquire a read lock.  This will prevent the owning process
           from upgrading to a write lock, therefore it will block in
           addTempRoot(). */
        debug(std::format("waiting for read lock on `{}'", path));
        lockFile(*fd, ltRead, true);

        /* Read the entire file. */
        string contents = readFile(*fd);

        /* Extract the roots. */
        string::size_type pos = 0, end;

        while ((end = contents.find((char) 0, pos)) != string::npos) {
            Path root(contents, pos, end - pos);
            debug(std::format("got temporary root `{}'", root));
            assertStorePath(root);
            tempRoots.insert(root);
            pos = end + 1;
        }

        fds.push_back(fd); /* keep open */
    }
}


static void foundRoot(StoreAPI & store,
    const Path & path, const Path & target, Roots & roots)
{
    Path storePath = toStorePath(target);
    if (store.isValidPath(storePath))
        roots[path] = storePath;
    else
        printMsg(lvlInfo, std::format("skipping invalid root from `{}' to `{}'", path, storePath));
}


static void findRoots(StoreAPI & store, const Path & path, unsigned char type, Roots & roots)
{
    try {

        if (type == DT_UNKNOWN)
            type = getFileType(path);

        if (type == DT_DIR) {
            for (auto & i : readDirectory(path))
                findRoots(store, path + "/" + i.name, i.type, roots);
        }

        else if (type == DT_LNK) {
            Path target = readLink(path);
            if (isInStore(target))
                foundRoot(store, path, target, roots);

            /* Handle indirect roots. */
            else {
                target = absPath(target, dirOf(path));
                if (!pathExists(target)) {
                    if (isInDir(path, settings.nixStateDir + "/" + gcRootsDir + "/auto")) {
                        printMsg(lvlInfo, std::format("removing stale link from `{}' to `{}'", path, target));
                        unlink(path.c_str());
                    }
                } else {
                    struct stat st2 = lstat(target);
                    if (!S_ISLNK(st2.st_mode)) return;
                    Path target2 = readLink(target);
                    if (isInStore(target2)) foundRoot(store, target, target2, roots);
                }
            }
        }

        else if (type == DT_REG) {
            Path storePath = settings.nixStore + "/" + baseNameOf(path);
            if (store.isValidPath(storePath))
                roots[path] = storePath;
        }

    }

    catch (SysError & e) {
        /* We only ignore permanent failures. */
        if (e.errNo == EACCES || e.errNo == ENOENT || e.errNo == ENOTDIR)
            printMsg(lvlInfo, std::format("cannot read potential root '{}': {}",
		        path, strerror(e.errNo)));
        else
            throw;
    }
}


Roots LocalStore::findRoots()
{
    Roots roots;

    /* Process direct roots in {gcroots,manifests,profiles}. */
    nix::findRoots(*this, settings.nixStateDir + "/" + gcRootsDir, DT_UNKNOWN, roots);
    if (pathExists(settings.nixStateDir + "/manifests"))
        nix::findRoots(*this, settings.nixStateDir + "/manifests", DT_UNKNOWN, roots);
    nix::findRoots(*this, settings.nixStateDir + "/profiles", DT_UNKNOWN, roots);

    return roots;
}


static void addAdditionalRoots(StoreAPI & store, PathSet & roots)
{
    debug(std::format("executing `{} gc --list-busy' to find additional roots",
	    settings.guixProgram));

    const Strings args = { "gc", "--list-busy" };
    string result = runProgram(settings.guixProgram, false, args);

    StringSet paths = tokenizeString<StringSet>(result, "\n");

    for (auto i : paths) {
        if (isInStore(i)) {
            Path path = toStorePath(i);
            if (roots.find(path) == roots.end() && store.isValidPath(path)) {
                debug(std::format("got additional root `{}'", path));
                roots.insert(path);
            }
        }
    }
}


struct GCLimitReached { };


struct LocalStore::GCState
{
    GCOptions options;
    GCResults & results;
    PathSet roots;
    PathSet tempRoots;
    PathSet dead;
    PathSet alive;
    bool gcKeepOutputs;
    bool gcKeepDerivations;
    unsigned long long bytesInvalidated;
    bool moveToTrash = true;
    Path trashDir;
    bool shouldDelete;
    GCState(GCResults & results_) : results(results_), bytesInvalidated(0) { }
};


bool LocalStore::isActiveTempFile(const GCState & state,
    const Path & path, const string & suffix)
{
    return hasSuffix(path, suffix)
        && state.tempRoots.find(string(path, 0, path.size() - suffix.size())) != state.tempRoots.end();
}


void LocalStore::deleteGarbage(GCState & state, const Path & path)
{
    unsigned long long bytesFreed;

    /* When deduplication is on, store items always have at least two links:
       the one at PATH, and one in /gnu/store/.links.  In that case, increase
       bytesFreed when PATH has two or fewer links.  */
    size_t linkThreshold =
	(settings.autoOptimiseStore && isStorePath(path)) ? 2 : 1;

    deletePath(path, bytesFreed, linkThreshold);
    state.results.bytesFreed += bytesFreed;
}


void LocalStore::deletePathRecursive(GCState & state, const Path & path)
{
    checkInterrupt();

    unsigned long long size = 0;

    if (isValidPath(path)) {
        PathSet referrers;
        queryReferrers(path, referrers);
        for (auto& i : referrers)
            if (i != path) deletePathRecursive(state, i);
        size = queryPathInfo(path).narSize;
        invalidatePathChecked(path);
    }

    struct stat st;
    if (lstat(path.c_str(), &st)) {
        if (errno == ENOENT) return;
        throw SysError(std::format("getting status of {}", path));
    }

    if (state.options.maxFreed != ULLONG_MAX) {
	auto freed = state.results.bytesFreed + state.bytesInvalidated;
	double fraction = ((double) freed) / (double) state.options.maxFreed;
	unsigned int percentage = (fraction > 1. ? 1. : fraction) * 100.;
	printMsg(lvlInfo, std::format("[{}%] deleting '{}'", percentage, path));
    } else {
	auto freed = state.results.bytesFreed + state.bytesInvalidated;
	printMsg(lvlInfo, std::format("[{}] deleting '{}'", showBytes(freed), path));
    }

    state.results.paths.insert(path);

    /* If the path is not a regular file or symlink, move it to the
       trash directory.  The move is to ensure that later (when we're
       not holding the global GC lock) we can delete the path without
       being afraid that the path has become alive again.  Otherwise
       delete it right away. */
    if (state.moveToTrash && S_ISDIR(st.st_mode)) {
        // Estimate the amount freed using the narSize field.  FIXME:
        // if the path was not valid, need to determine the actual
        // size.
        try {
            if (chmod(path.c_str(), st.st_mode | S_IWUSR) == -1)
                throw SysError(std::format("making `{}' writable", path));
            Path tmp = state.trashDir + "/" + baseNameOf(path);
            if (rename(path.c_str(), tmp.c_str()))
                throw SysError(std::format("unable to rename `{}' to `{}'", path, tmp));
            state.bytesInvalidated += size;
        } catch (SysError & e) {
            /* In a Docker container, rename(2) returns EXDEV when the source
               and destination are not both on the "top layer".  See:
               https://bugs.gnu.org/41607 */
            if (e.errNo == ENOSPC || e.errNo == EXDEV) {
                printMsg(lvlInfo, std::format("note: can't create move `{}': {}", path, e.msg()));
                deleteGarbage(state, path);
            }
        }
    } else
        deleteGarbage(state, path);

    if (state.results.bytesFreed + state.bytesInvalidated > state.options.maxFreed) {
        printMsg(lvlInfo, std::format("deleted or invalidated more than {} bytes; stopping", state.options.maxFreed));
        throw GCLimitReached();
    }
}


bool LocalStore::canReachRoot(GCState & state, PathSet & visited, const Path & path)
{
    if (visited.find(path) != visited.end()) return false;

    if (state.alive.find(path) != state.alive.end()) {
        return true;
    }

    if (state.dead.find(path) != state.dead.end()) {
        return false;
    }

    if (state.roots.find(path) != state.roots.end()) {
        printMsg(lvlDebug, std::format("cannot delete `{}' because it's a root", path));
        state.alive.insert(path);
        return true;
    }

    visited.insert(path);

    if (!isValidPath(path)) return false;

    PathSet incoming;

    /* Don't delete this path if any of its referrers are alive. */
    queryReferrers(path, incoming);

    /* If gc-keep-derivations is set and this is a derivation, then
       don't delete the derivation if any of the outputs are alive. */
    if (state.gcKeepDerivations && isDerivation(path)) {
        PathSet outputs = queryDerivationOutputs(path);
        for (auto& i : outputs)
            if (isValidPath(i) && queryDeriver(i) == path)
                incoming.insert(i);
    }

    /* If gc-keep-outputs is set, then don't delete this path if there
       are derivers of this path that are not garbage. */
    if (state.gcKeepOutputs) {
        PathSet derivers = queryValidDerivers(path);
        for (auto& i : derivers)
            incoming.insert(i);
    }

    for (auto& i : incoming)
        if (i != path)
            if (canReachRoot(state, visited, i)) {
                state.alive.insert(path);
                return true;
            }

    return false;
}


void LocalStore::tryToDelete(GCState & state, const Path & path)
{
    checkInterrupt();

    if (path == linksDir || path == state.trashDir) return;

    startNest(nest, lvlDebug, std::format("considering whether to delete `{}'", path));

    if (!isValidPath(path)) {
        /* A lock file belonging to a path that we're building right
           now isn't garbage. */
        if (isActiveTempFile(state, path, ".lock")) return;

        /* Don't delete .chroot directories for derivations that are
           currently being built. */
        if (isActiveTempFile(state, path, ".chroot")) return;
    }

    PathSet visited;

    if (canReachRoot(state, visited, path)) {
        printMsg(lvlDebug, std::format("cannot delete `{}' because it's still reachable", path));
    } else {
        /* No path we visited was a root, so everything is garbage.
           But we only delete ‘path’ and its referrers here so that
           ‘nix-store --delete’ doesn't have the unexpected effect of
           recursing into derivations and outputs. */
        state.dead.insert(visited.begin(), visited.end());
        if (state.shouldDelete)
            deletePathRecursive(state, path);
    }
}


/* Unlink all files in /nix/store/.links that have a link count of 1,
   which indicates that there are no other links and so they can be
   safely deleted.  FIXME: race condition with optimisePath(): we
   might see a link count of 1 just before optimisePath() increases
   the link count. */
void LocalStore::removeUnusedLinks(const GCState & state)
{
    AutoCloseDir dir = opendir(linksDir.c_str());
    if (!dir) throw SysError(std::format("opening directory `{}'", linksDir));

    long long actualSize = 0, unsharedSize = 0;

    struct dirent * dirent;
    while (errno = 0, dirent = readdir(dir)) {
        checkInterrupt();
        string name = dirent->d_name;
        if (name == "." || name == "..") continue;
        Path path = linksDir + "/" + name;

#ifdef HAVE_STATX
# define st_size stx_size
# define st_nlink stx_nlink
	static int statx_flags = AT_SYMLINK_NOFOLLOW | AT_STATX_DONT_SYNC;
	struct statx st;

	if (statx(AT_FDCWD, path.c_str(), statx_flags,
		  STATX_SIZE | STATX_NLINK, &st) == -1) {
	    if (errno == EINVAL) {
		/* Old 3.10 kernels (CentOS 7) don't support
		   AT_STATX_DONT_SYNC, so try again without it.  */
		statx_flags &= ~AT_STATX_DONT_SYNC;
		if (statx(AT_FDCWD, path.c_str(), statx_flags,
			  STATX_SIZE | STATX_NLINK, &st) == -1)
		    throw SysError(std::format("statting `{}'", path));
	    } else {
		throw SysError(std::format("statting `{}'", path));
	    }
	}
#else
        struct stat st;
        if (lstat(path.c_str(), &st) == -1)
            throw SysError(std::format("statting `{}'", path));
#endif

	/* Drop links for files smaller than 'deduplicationMinSize', even if
	   they have more than one hard link.  */
        if (st.st_nlink != 1
	    && ((unsigned long long) st.st_size) >= deduplicationMinSize) {
            actualSize += st.st_size;
            unsharedSize += (st.st_nlink - 1) * st.st_size;
            continue;
        }

        printMsg(lvlTalkative, std::format("deleting unused link `{}'", path));

        if (unlink(path.c_str()) == -1)
            throw SysError(std::format("deleting `{}'", path));

        state.results.bytesFreed += st.st_size;
#undef st_size
#undef st_nlink
    }

    struct stat st;
    if (stat(linksDir.c_str(), &st) == -1)
        throw SysError(std::format("statting `{}'", linksDir));
    long long overhead = st.st_size;
    long long freedbytes = (unsharedSize - actualSize - overhead);

    printMsg(lvlInfo, std::format("note: currently hard linking saves {}", showBytes(freedbytes)));
}


void LocalStore::collectGarbage(const GCOptions & options, GCResults & results)
{
    GCState state(results);
    state.options = options;
    state.trashDir = settings.nixStore + "/trash";
    state.gcKeepOutputs = settings.gcKeepOutputs;
    state.gcKeepDerivations = settings.gcKeepDerivations;

    /* Using `--ignore-liveness' with `--delete' can have unintended
       consequences if `gc-keep-outputs' or `gc-keep-derivations' are
       true (the garbage collector will recurse into deleting the
       outputs or derivers, respectively).  So disable them. */
    if (options.action == GCOptions::gcDeleteSpecific && options.ignoreLiveness) {
        state.gcKeepOutputs = false;
        state.gcKeepDerivations = false;
    }

    state.shouldDelete = options.action == GCOptions::gcDeleteDead || options.action == GCOptions::gcDeleteSpecific;

    /* Acquire the global GC root.  This prevents
       a) New roots from being added.
       b) Processes from creating new temporary root files. */
    AutoCloseFD fdGCLock = openGCLock(ltWrite);

    /* Find the roots.  Since we've grabbed the GC lock, the set of
       permanent roots cannot increase now. */
    printMsg(lvlError, "finding garbage collector roots...");
    Roots rootMap = options.ignoreLiveness ? Roots() : findRoots();

    for (auto& i : rootMap) state.roots.insert(i.second);

    /* Add additional roots returned by 'guix gc --list-busy'.  This is
       typically used to add running programs to the set of roots (to prevent
       them from being garbage collected). */
    if (!options.ignoreLiveness)
        addAdditionalRoots(*this, state.roots);

    /* Read the temporary roots.  This acquires read locks on all
       per-process temporary root files.  So after this point no paths
       can be added to the set of temporary roots. */
    FDs fds;
    readTempRoots(state.tempRoots, fds);
    state.roots.insert(state.tempRoots.begin(), state.tempRoots.end());

    /* After this point the set of roots or temporary roots cannot
       increase, since we hold locks on everything.  So everything
       that is not reachable from `roots' is garbage. */

    if (state.shouldDelete) {
        if (pathExists(state.trashDir)) deleteGarbage(state, state.trashDir);
        try {
            createDirs(state.trashDir);
        } catch (SysError & e) {
            if (e.errNo == ENOSPC) {
                printMsg(lvlInfo, std::format("note: can't create trash directory: {}", e.msg()));
                state.moveToTrash = false;
            }
        }
    }

    /* Now either delete all garbage paths, or just the specified
       paths (for gcDeleteSpecific). */

    if (options.action == GCOptions::gcDeleteSpecific) {

        for (auto& i : options.pathsToDelete) {
            assertStorePath(i);
            tryToDelete(state, i);
            if (state.dead.find(i) == state.dead.end())
                throw Error(std::format("cannot delete path `{}' since it is still alive", i));
        }

    } else if (options.maxFreed > 0) {

        if (state.shouldDelete)
            printMsg(lvlError, "deleting garbage...");
        else
            printMsg(lvlError, "determining live/dead paths...");

        try {

            AutoCloseDir dir = opendir(settings.nixStore.c_str());
            if (!dir) throw SysError(std::format("opening directory `{}'", settings.nixStore));

            /* Read the store and immediately delete all paths that
               aren't valid.  When using --max-freed etc., deleting
               invalid paths is preferred over deleting unreachable
               paths, since unreachable paths could become reachable
               again.  We don't use readDirectory() here so that GCing
               can start faster. */
            Paths entries;
            struct dirent * dirent;
            while (errno = 0, dirent = readdir(dir)) {
                checkInterrupt();
                string name = dirent->d_name;
                if (name == "." || name == "..") continue;
                Path path = settings.nixStore + "/" + name;
                if (isValidPath(path))
                    entries.push_back(path);
                else
                    tryToDelete(state, path);
            }

            dir.close();

            /* Now delete the unreachable valid paths.  Randomise the
               order in which we delete entries to make the collector
               less biased towards deleting paths that come
               alphabetically first (e.g. /nix/store/000...).  This
               matters when using --max-freed etc. */
            vector<Path> entries_(entries.begin(), entries.end());
            std::random_device seeder;
            std::default_random_engine generator(seeder());
            std::shuffle(entries_.begin(), entries_.end(), generator);

            for (auto& i : entries_)
                tryToDelete(state, i);

        } catch (GCLimitReached & e) {
        }
    }

    if (state.options.action == GCOptions::gcReturnLive) {
        state.results.paths = state.alive;
        return;
    }

    if (state.options.action == GCOptions::gcReturnDead) {
        state.results.paths = state.dead;
        return;
    }

    /* Allow other processes to add to the store from here on. */
    fdGCLock.close();
    fds.clear();

    /* Delete the trash directory. */
    printMsg(lvlInfo, std::format("deleting `{}'", state.trashDir));
    deleteGarbage(state, state.trashDir);

    /* Clean up the links directory. */
    if (options.action == GCOptions::gcDeleteDead || options.action == GCOptions::gcDeleteSpecific) {
        printMsg(lvlError, "deleting unused links...");
        removeUnusedLinks(state);
    }

    /* While we're at it, vacuum the database. */
    //if (options.action == GCOptions::gcDeleteDead) vacuumDB();
}


}
