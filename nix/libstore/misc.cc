#include "misc.hh"
#include <math.h>
#include "store-api.hh"
#include "local-store.hh"
#include "globals.hh"


namespace nix {


Derivation derivationFromPath(StoreAPI & store, const Path & drvPath)
{
    assertStorePath(drvPath);
    store.ensurePath(drvPath);
    return readDerivation(drvPath);
}


void computeFSClosure(StoreAPI & store, const Path & path,
    PathSet & paths, bool flipDirection, bool includeOutputs, bool includeDerivers)
{
    if (paths.find(path) != paths.end()) return;
    paths.insert(path);

    PathSet edges;

    if (flipDirection) {
        store.queryReferrers(path, edges);

        if (includeOutputs) {
            PathSet derivers = store.queryValidDerivers(path);
            for (auto& i : derivers)
                edges.insert(i);
        }

        if (includeDerivers && isDerivation(path)) {
            PathSet outputs = store.queryDerivationOutputs(path);
            for (auto& i : outputs)
                if (store.isValidPath(i) && store.queryDeriver(i) == path)
                    edges.insert(i);
        }

    } else {
        store.queryReferences(path, edges);

        if (includeOutputs && isDerivation(path)) {
            PathSet outputs = store.queryDerivationOutputs(path);
            for (auto& i : outputs)
                if (store.isValidPath(i)) edges.insert(i);
        }

        if (includeDerivers) {
            Path deriver = store.queryDeriver(path);
            if (store.isValidPath(deriver)) edges.insert(deriver);
        }
    }

    for (auto& i : edges)
        computeFSClosure(store, i, paths, flipDirection, includeOutputs, includeDerivers);
}


static void dfsVisit(StoreAPI & store, const PathSet & paths,
    const Path & path, PathSet & visited, Paths & sorted,
    PathSet & parents)
{
    if (parents.find(path) != parents.end())
        throw BuildError(format("cycle detected in the references of `%1%'") % path);

    if (visited.find(path) != visited.end()) return;
    visited.insert(path);
    parents.insert(path);

    PathSet references;
    if (store.isValidPath(path))
        store.queryReferences(path, references);

    for (auto& i : references)
        /* Don't traverse into paths that don't exist.  That can
           happen due to substitutes for non-existent paths. */
        if (i != path && paths.find(i) != paths.end())
            dfsVisit(store, paths, i, visited, sorted, parents);

    sorted.push_front(path);
    parents.erase(path);
}


Paths topoSortPaths(StoreAPI & store, const PathSet & paths)
{
    Paths sorted;
    PathSet visited, parents;
    for (const auto& i : paths)
        dfsVisit(store, paths, i, visited, sorted, parents);
    return sorted;
}

/* Max of LLONG_MAX is 8 EiB */
string showBytes(long long bytes)
{
    if (llabs(bytes > exp2l(60))) {
        return (format("%7.2f EiB") % (bytes / exp2l(60))).str();
    } else if (llabs(bytes > exp2l(50))) {
        return (format("%7.2f PiB") % (bytes / exp2l(50))).str();
    } else if (llabs(bytes > exp2l(40))) {
        return (format("%7.2f TiB") % (bytes / exp2l(40))).str();
    } else if (llabs(bytes > exp2l(30))) {
        return (format("%7.2f GiB") % (bytes / exp2l(30))).str();
    } else if (llabs(bytes > exp2l(20))) {
        return (format("%7.2f MiB") % (bytes / exp2l(20))).str();
    } else if (llabs(bytes > exp2l(10))) {
        return (format("%7.2f KiB") % (bytes / exp2l(10))).str();
    } else {
        return (format("%4f  bytes") % bytes).str();
    }
}


}
