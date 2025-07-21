#pragma once

#include "config.h"

#include <string>
#include <list>
#include <set>
#include <vector>
#include <string_view>


/* Before 4.7, gcc's std::exception uses empty throw() specifiers for
 * its (virtual) destructor and what() in c++11 mode, in violation of spec
 */
#ifdef __GNUC__
#if __GNUC__ < 4 || (__GNUC__ == 4 && __GNUC_MINOR__ < 7)
#define EXCEPTION_NEEDS_THROW_SPEC
#endif
#endif


namespace nix {


/* Inherit some names from other namespaces for convenience. */
using std::string;
using std::list;
using std::set;
using std::vector;


/* BaseError should generally not be caught, as it has Interrupted as
   a subclass. Catch Error instead. */
class BaseError : public std::exception
{
protected:
    string prefix_; // used for location traces etc.
    string err;
public:
    unsigned int status; // exit status
    BaseError(std::string_view fs, unsigned int status = 1);
#ifdef EXCEPTION_NEEDS_THROW_SPEC
    ~BaseError() throw () { };
    const char * what() const throw () { return err.c_str(); }
#else
    const char * what() const noexcept { return err.c_str(); }
#endif
    const string & msg() const { return err; }
    const string & prefix() const { return prefix_; }
    BaseError & addPrefix(std::string fs);
};

#define MakeError(newClass, superClass) \
    class newClass : public superClass                  \
    {                                                   \
    public:                                             \
        newClass(std::string_view fs, unsigned int status = 1) : superClass(fs, status) { }; \
    };

MakeError(Error, BaseError)

class SysError : public Error
{
public:
    int errNo;
    SysError(std::string_view fs);
};


using Strings = std::list<std::string>;
using StringSet = std::set<std::string>;


/* Paths are just strings. */
using Path = std::string;
using Paths = std::list<Path>;
using PathSet = std::set<Path>;


enum Verbosity {
    lvlError = 0,
    lvlInfo,
    lvlTalkative,
    lvlChatty,
    lvlDebug,
    lvlVomit
};


}
