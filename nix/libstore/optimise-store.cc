#include "config.h"

#include "misc.hh"
#include "util.hh"
#include "local-store.hh"
#include "globals.hh"

#include <cstdlib>
#include <cstring>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <errno.h>
#include <stdio.h>
#include <format>

namespace nix {

/* Any file smaller than this is not considered for deduplication.
   Keep in sync with (guix store deduplication).  */
const size_t deduplicationMinSize = 8192;

static void makeWritable(const Path & path)
{
    struct stat st;
    if (lstat(path.c_str(), &st))
        throw SysError(std::format("getting attributes of path `{}'", path));
    if (chmod(path.c_str(), st.st_mode | S_IWUSR) == -1)
        throw SysError(std::format("changing writability of `{}'", path));
}


struct MakeReadOnly
{
    Path path;
    MakeReadOnly(const Path & path) : path(path) { }
    ~MakeReadOnly()
    {
        try {
            /* This will make the path read-only. */
            if (path != "") canonicaliseTimestampAndPermissions(path);
        } catch (...) {
            ignoreException();
        }
    }
};


LocalStore::InodeHash LocalStore::loadInodeHash()
{
    printMsg(lvlDebug, "loading hash inodes in memory");
    InodeHash inodeHash;

    AutoCloseDir dir = opendir(linksDir.c_str());
    if (!dir) throw SysError(std::format("opening directory `{}'", linksDir));

    struct dirent * dirent;
    while (errno = 0, dirent = readdir(dir)) { /* sic */
        checkInterrupt();
        // We don't care if we hit non-hash files, anything goes
        inodeHash.insert(dirent->d_ino);
    }
    if (errno) throw SysError(std::format("reading directory `{}'", linksDir));

    printMsg(lvlTalkative, std::format("loaded {} hash inodes", inodeHash.size()));

    return inodeHash;
}


Strings LocalStore::readDirectoryIgnoringInodes(const Path & path, const InodeHash & inodeHash)
{
    Strings names;

    AutoCloseDir dir = opendir(path.c_str());
    if (!dir) throw SysError(std::format("opening directory `{}'", path));

    struct dirent * dirent;
    while (errno = 0, dirent = readdir(dir)) { /* sic */
        checkInterrupt();

        if (inodeHash.count(dirent->d_ino)) {
            printMsg(lvlDebug, std::format("`{}' is already linked", dirent->d_name));
            continue;
        }

        string name = dirent->d_name;
        if (name == "." || name == "..") continue;
        names.push_back(name);
    }
    if (errno) throw SysError(std::format("reading directory `{}'", path));

    return names;
}


void LocalStore::optimisePath_(OptimiseStats & stats, const Path & path, InodeHash & inodeHash)
{
    checkInterrupt();

    struct stat st;
    if (lstat(path.c_str(), &st))
        throw SysError(std::format("getting attributes of path `{}'", path));

    if (S_ISDIR(st.st_mode)) {
        Strings names = readDirectoryIgnoringInodes(path, inodeHash);
        for (auto& i : names)
            optimisePath_(stats, path + "/" + i, inodeHash);
        return;
    }

    /* We can hard link regular files (and maybe symlinks), but do that only
       for files larger than some threshold.  This avoids adding too many
       entries to '.links', which would slow down 'removeUnusedLinks' while
       saving little space.  */
    if (!S_ISREG(st.st_mode) || ((size_t) st.st_size) < deduplicationMinSize)
	return;

    /* Sometimes SNAFUs can cause files in the store to be
       modified, in particular when running programs as root under
       Guix System (example: $fontconfig/var/cache being modified).  Skip
       those files.  FIXME: check the modification time. */
    if (S_ISREG(st.st_mode) && (st.st_mode & S_IWUSR)) {
        printMsg(lvlError, std::format("skipping suspicious writable file `{}'", path));
        return;
    }

    /* This can still happen on top-level files. */
    if (st.st_nlink > 1 && inodeHash.count(st.st_ino)) {
        printMsg(lvlDebug, std::format("`{}' is already linked, with {} other file(s).", path, (st.st_nlink - 2)));
        return;
    }

    /* Hash the file.  Note that hashPath() returns the hash over the
       NAR serialisation, which includes the execute bit on the file.
       Thus, executable and non-executable files with the same
       contents *won't* be linked (which is good because otherwise the
       permissions would be screwed up).

       Also note that if `path' is a symlink, then we're hashing the
       contents of the symlink (i.e. the result of readlink()), not
       the contents of the target (which may not even exist). */
    Hash hash = hashPath(htSHA256, path).first;
    printMsg(lvlDebug, std::format("`{}' has hash `{}'", path, printHash(hash)));

    /* Check if this is a known hash. */
    Path linkPath = linksDir + "/" + printHash32(hash);

 retry:
    if (!pathExists(linkPath)) {
        /* Nope, create a hard link in the links directory. */
        if (link(path.c_str(), linkPath.c_str()) == 0) {
            inodeHash.insert(st.st_ino);
            return;
        }

	switch (errno) {
	case EEXIST:
	    /* Fall through if another process created ‘linkPath’ before
	       we did. */
	    break;

	case ENOSPC:
	    /* On ext4, that probably means the directory index is full.  When
	       that happens, it's fine to ignore it: we just effectively
	       disable deduplication of this file.  */
	    printMsg(lvlInfo, std::format("cannot link `{}' to `{}': {}",
		    linkPath, path, strerror(ENOSPC)));
	    return;

	default:
            throw SysError(std::format("cannot link `{}' to `{}'", linkPath, path));
	}
    }

    /* Yes!  We've seen a file with the same contents.  Replace the
       current file with a hard link to that file. */
    struct stat stLink;
    if (lstat(linkPath.c_str(), &stLink))
        throw SysError(std::format("getting attributes of path `{}'", linkPath));

    if (st.st_ino == stLink.st_ino) {
        printMsg(lvlDebug, std::format("`{}' is already linked to `{}'", path, linkPath));
        return;
    }

    if (st.st_size != stLink.st_size) {
        printMsg(lvlError, std::format("removing corrupted link `%1%'", linkPath));
        unlink(linkPath.c_str());
        goto retry;
    }

    printMsg(lvlTalkative, std::format("linking `%1%' to `%2%'", path, linkPath));

    /* Make the containing directory writable, but only if it's not
       the store itself (we don't want or need to mess with its
       permissions). */
    bool mustToggle = !isStorePath(path);
    if (mustToggle) makeWritable(dirOf(path));

    /* When we're done, make the directory read-only again and reset
       its timestamp back to 0. */
    MakeReadOnly makeReadOnly(mustToggle ? dirOf(path) : "");

    Path tempLink = std::format("{}/.tmp-link-{}-{}", settings.nixStore, getpid(), rand());

    if (link(linkPath.c_str(), tempLink.c_str()) == -1) {
        if (errno == EMLINK) {
            /* Too many links to the same file (>= 32000 on most file
               systems).  This is likely to happen with empty files.
               Just shrug and ignore. */
            if (st.st_size)
                printMsg(lvlInfo, std::format("`{}' has maximum number of links", linkPath));
            return;
        }
	    throw SysError(std::format("cannot link `{}' to `{}'", tempLink, linkPath));
	}

    /* Atomically replace the old file with the new hard link. */
    if (rename(tempLink.c_str(), path.c_str()) == -1) {
	int renameErrno = errno;
        if (unlink(tempLink.c_str()) == -1)
            printMsg(lvlError, std::format("unable to unlink `{}'", tempLink));
        if (renameErrno == EMLINK) {
            /* Some filesystems generate too many links on the rename,
               rather than on the original link.  (Probably it
               temporarily increases the st_nlink field before
               decreasing it again.) */
            if (st.st_size)
                printMsg(lvlInfo, std::format("`{}' has maximum number of links", linkPath));
            return;
        }
        throw SysError(std::format("cannot rename `{}' to `{}'", tempLink, path));
    }

    stats.filesLinked++;
    stats.bytesFreed += st.st_size;
    stats.blocksFreed += st.st_blocks;
}


void LocalStore::optimiseStore(OptimiseStats & stats)
{
    PathSet paths = queryAllValidPaths();
    InodeHash inodeHash = loadInodeHash();

    for (auto& i : paths) {
        addTempRoot(i);
        if (!isValidPath(i)) continue; /* path was GC'ed, probably */
        startNest(nest, lvlChatty, std::format("hashing files in `{}'", i));
        optimisePath_(stats, i, inodeHash);
    }
}

void LocalStore::optimiseStore()
{
    OptimiseStats stats;

    optimiseStore(stats);

    printMsg(lvlError,
        std::format("{} freed by hard-linking {} files",
        showBytes(stats.bytesFreed),
        stats.filesLinked));
}

void LocalStore::optimisePath(const Path & path)
{
    OptimiseStats stats;
    InodeHash inodeHash;

    if (settings.autoOptimiseStore) optimisePath_(stats, path, inodeHash);
}


}
