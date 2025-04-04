# GNU Guix --- Functional package management for GNU
# Copyright © 2015 David Thompson <davet@gnu.org>
# Copyright © 2022, 2023 John Kehayias <john.kehayias@protonmail.com>
# Copyright © 2023, 2025 Ludovic Courtès <ludo@gnu.org>
#
# This file is part of GNU Guix.
#
# GNU Guix is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or (at
# your option) any later version.
#
# GNU Guix is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

#
# Test 'guix environment'.
#

set -e

guix environment --version

if ! guile -c '((@ (guix scripts environment) assert-container-features))'
then
    # User containers are not supported; skip this test.
    exit 77
fi

tmpdir="t-guix-environment-$$"
trap 'rm -r "$tmpdir"' EXIT

mkdir "$tmpdir"

# Make sure the exit value is preserved.
if guix environment --container --ad-hoc --bootstrap guile-bootstrap \
        -- guile -c '(exit 42)'
then
    false
else
    test $? = 42
fi

# Try '--root' and '--profile'.
root="$tmpdir/root"
guix environment -C --ad-hoc --bootstrap guile-bootstrap -r "$root" -- guile --version
guix environment -C -p "$root" --bootstrap -- guile --version
path1=$(guix environment -C -p "$root" --bootstrap -- guile -c '(display (getenv "PATH"))')
path2=$(guix environment -C --ad-hoc --bootstrap guile-bootstrap  -- guile -c '(display (getenv "PATH"))')
test "$path1" = "$path2"

# Make sure "localhost" resolves.
guix environment --container --ad-hoc --bootstrap guile-bootstrap \
     -- guile -c '(exit (pair? (getaddrinfo "localhost" "80")))'

# We should get ECONNREFUSED, not ENETUNREACH, which would indicate that "lo"
# is down.
guix environment --container --ad-hoc --bootstrap guile-bootstrap \
     -- guile -c "(exit (= ECONNREFUSED
  (catch 'system-error
    (lambda ()
      (let ((sock (socket AF_INET SOCK_STREAM 0)))
        (connect sock AF_INET INADDR_LOOPBACK 12345)))
    (lambda args
      (pk 'errno (system-error-errno args))))))"

# Make sure '--preserve' is honored.
result="`FOOBAR=42; export FOOBAR; guix environment -C --ad-hoc --bootstrap \
   guile-bootstrap -E ^FOO -- guile -c '(display (getenv \"FOOBAR\"))'`"
test "$result" = "42"

# By default, the UID inside the container should be the same as outside.
uid="`id -u`"
inner_uid="`guix environment -C --ad-hoc --bootstrap guile-bootstrap \
  -- guile -c '(display (getuid))'`"
test $inner_uid = $uid

# When '--user' is passed, the UID should be 1000.  (Note: Use a separate HOME
# so that we don't run into problems when the test directory is under /home.)
export tmpdir
inner_uid="`HOME=$tmpdir guix environment -C --ad-hoc --bootstrap guile-bootstrap \
  --user=gnu-guix -- guile -c '(display (getuid))'`"
test $inner_uid = 1000

if test "x$USER" = "x"; then USER="`id -un`"; fi

# Check whether /etc/passwd and /etc/group are valid.
guix environment -C --ad-hoc --bootstrap guile-bootstrap \
     -- guile -c "(exit (string=? \"$USER\" (passwd:name (getpwuid (getuid)))))"
guix environment -C --ad-hoc --bootstrap guile-bootstrap \
     -- guile -c '(exit (string? (group:name (getgrgid (getgid)))))'
guix environment -C --ad-hoc --bootstrap guile-bootstrap \
     -- guile -c '(use-modules (srfi srfi-1))
                  (exit (every group:name
                               (map getgrgid (vector->list (getgroups)))))'

# Make sure file-not-found errors in mounts are reported.
if guix environment --container --ad-hoc --bootstrap guile-bootstrap \
	--expose=/does-not-exist -- guile -c 1 2> "$tmpdir/error"
then
    false
else
    grep "/does-not-exist" "$tmpdir/error"
    grep "[Nn]o such file" "$tmpdir/error"
fi

# Make sure that the right directories are mapped.
mount_test_code="
(use-modules (ice-9 rdelim)
             (ice-9 match)
             (srfi srfi-1))

(define mappings
  (filter-map (lambda (line)
                (match (string-split line #\space)
                  ;; Empty line.
                  ((\"\") #f)
                  ;; Ignore the root file system.
                  ((_ \"/\" _ _ _ _)
                   #f)
                  ;; Ignore these types of file systems, except if they
                  ;; correspond to a parent file system.
                  ((_ mount (or \"tmpfs\" \"proc\" \"sysfs\" \"devtmpfs\"
                                \"devpts\" \"cgroup\" \"mqueue\") _ _ _)
                   (and (string-prefix? (getcwd) mount)
		        mount))
                  ((_ mount _ _ _ _)
                   mount)))
              (string-split (call-with-input-file \"/proc/mounts\" read-string)
                            #\newline)))

(for-each (lambda (mount)
            (display mount)
            (newline))
          mappings)"

guix environment --container --ad-hoc --bootstrap guile-bootstrap \
     -- guile -c "$mount_test_code" > $tmpdir/mounts

cat "$tmpdir/mounts"
test `wc -l < $tmpdir/mounts` -eq 4

current_dir="`cd $PWD; pwd -P`"
grep -e "$current_dir$" $tmpdir/mounts # current directory
grep $(guix build guile-bootstrap) $tmpdir/mounts
grep -e "$NIX_STORE_DIR/.*-bash" $tmpdir/mounts # bootstrap bash

rm $tmpdir/mounts

# Make sure 'GUIX_ENVIRONMENT' is set to '~/.guix-profile' when requested
# within a container.
(
  linktest='
(exit (and (string=? (getenv "GUIX_ENVIRONMENT")
                     (string-append (getenv "HOME") "/.guix-profile"))
           (string-prefix? "'"$NIX_STORE_DIR"'"
                           (readlink (string-append (getenv "HOME")
                                                    "/.guix-profile")))))'

  cd "$tmpdir" \
     && guix environment --bootstrap --container --link-profile \
             --ad-hoc guile-bootstrap --pure \
             -- guile -c "$linktest"
)

# Test that user can be mocked.
usertest='(exit (and (string=? (getenv "HOME") "/home/foognu")
                     (string=? (passwd:name (getpwuid 1000)) "foognu")
                     (file-exists? "/home/foognu/umock")))'
touch "$tmpdir/umock"
HOME="$tmpdir" guix environment --bootstrap --container --user=foognu \
     --ad-hoc guile-bootstrap --pure \
     --share="$tmpdir/umock" \
     -- guile -c "$usertest"

# if not sharing CWD, chdir home
(
  cd "$tmpdir" \
    && guix environment --bootstrap --container --no-cwd --user=foo  \
            --ad-hoc guile-bootstrap --pure \
            -- /bin/sh -c 'test $(pwd) == "/home/foo" -a ! -d '"$tmpdir"
)

# Check that the root file system is read-only by default...
guix environment --bootstrap --container --ad-hoc guile-bootstrap \
     -- guile -c '(mkdir "/whatever")' && false

# ... and can be made writable.
guix environment --bootstrap --container --ad-hoc guile-bootstrap	\
     --writable-root							\
     -- guile -c '(mkdir "/whatever")'

# Check the exit code.

abnormal_exit_code="
(use-modules (system foreign))
;; Purposely make Guile crash with a segfault. :)
(pointer->string (make-pointer 123) 123)"

if guix environment --bootstrap --container \
	--ad-hoc guile-bootstrap -- guile -c "$abnormal_exit_code"
then false;
else
    test $? -gt 127
fi

# Test the Filesystem Hierarchy Standard (FHS) container option, --emulate-fhs (-F)

# As this option requires a glibc package (glibc-for-fhs), try to run these
# tests with the user's global store to make it easier to build or download a
# substitute.
storedir="`guile -c '(use-modules (guix config))(display %storedir)'`"
localstatedir="`guile -c '(use-modules (guix config))(display %localstatedir)'`"
NIX_STORE_DIR="$storedir"
GUIX_DAEMON_SOCKET="$localstatedir/guix/daemon-socket/socket"
export NIX_STORE_DIR GUIX_DAEMON_SOCKET

if ! guile -c '(use-modules (guix)) (exit (false-if-exception (open-connection)))'
then
    exit 77
fi

# Test that the container has FHS specific files/directories.  Note that /bin
# exists in a non-FHS container as it will contain sh, a symlink to the bash
# package, so we don't test for it.
guix shell -C --emulate-fhs --bootstrap guile-bootstrap \
     -- guile -c '(exit (and (file-exists? "/etc/ld.so.cache")
                             (file-exists? "/lib")
                             (file-exists? "/sbin")
                             (file-exists? "/usr/bin")
                             (file-exists? "/usr/include")
                             (file-exists? "/usr/lib")
                             (file-exists? "/usr/libexec")
                             (file-exists? "/usr/sbin")
                             (file-exists? "/usr/share")))'

# Test that the ld cache was generated and can be successfully read.
guix shell -CF --bootstrap guile-bootstrap \
     -- guile -c '(execlp "ldconfig" "ldconfig" "-p")'

# Test that the package glibc-for-fhs is in the container even if there is the
# regular glibc package from another source.  See
# <https://issues.guix.gnu.org/58861>.
guix shell -CF --bootstrap guile-bootstrap glibc \
     -- guile -c '(exit (if (string-contains (readlink "/lib/libc.so")
                            "glibc-for-fhs")
                           0
                           1))'

# Test that $PATH inside the container includes the FHS directories.
guix shell -CF coreutils -- env | grep ^PATH=/bin:/usr/bin:/sbin:/usr/sbin.*

# Make sure '--preserve' is honored for $PATH, which the '--emulate-fhs'
# option modifies.  We can't (easily) check the whole $PATH as it will differ
# inside and outside the container, so just check our test $PATH is still
# present.  See <https://issues.guix.gnu.org/60566>.
PATH=/foo $(type -P guix) shell -CF -E ^PATH$ coreutils \
     -- env | grep ^PATH=.*:/foo

# '--symlink' works.
echo "TESTING SYMLINK IN CONTAINER"
guix shell --bootstrap guile-bootstrap --container \
     --symlink=/usr/bin/guile=bin/guile -- \
     /usr/bin/guile --version

# A dangling symlink causes the command to fail.
guix shell --bootstrap -CS /usr/bin/python=bin/python guile-bootstrap -- exit && false

# An invalid symlink spec causes the command to fail.
guix shell --bootstrap -CS bin/guile=/usr/bin/guile guile-bootstrap -- exit && false

# Check whether '--nesting' works.
guix build hello -d
env="$(type -P pre-inst-env)"
guix shell -C -D guix -- "$env" guix build hello -d && false # cannot work
hello_drv="$(guix build hello -d)"
hello_drv_nested="$(cd "$(dirname env)" && guix shell --bootstrap -E GUIX_BUILD_OPTIONS -CW -D guix -- "$env" guix build hello -d)"
test "$hello_drv" = "$hello_drv_nested"
