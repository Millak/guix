# GNU Guix --- Functional package management for GNU
# Copyright © 2023 Ludovic Courtès <ludo@gnu.org>
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
# Test the 'guix refresh' command-line utility.
#

guix refresh --version

manifest="t-guix-refresh-manifest-$$.scm"
module_dir="t-guix-refresh-modules-$$"
trap 'rm -f "$manifest"; rm -rf "$module_dir"' EXIT

# Tell the 'test' updater what to simulate.
export GUIX_TEST_UPDATER_TARGETS
idutils_version="$(guix package -A ^idutils$ | cut -f2)"
GUIX_TEST_UPDATER_TARGETS='
  (("guile" "3" (("12.5" "file:///dev/null")
                 ("1.6.4" "file:///dev/null")
                 ("3.13.3" "file:///dev/null")))
   ("libreoffice" "" (("1.0" "file:///dev/null")))
   ("idutils" "" (("'$idutils_version'" "file:///dev/null")))
   ("the-test-package" "" (("5.5" "file://'$PWD/$module_dir'/source"
                                   ("grep" "sed" "libreoffice")))))'

# No newer version available.
guix refresh -t test idutils	# XXX: should return non-zero?
case "$(guix refresh -t test idutils 2>&1)" in
    *"$idutils_version"*"already the latest version"*) true;;
    *) false;;
esac

# No-op when updating to same version.
case "$(guix refresh -t test -u idutils \
--target-version=$idutils_version 2>&1)" in
    *downgrading*) false;;
    *updating*) false;;
    *) true;;
esac

guix refresh -t test libreoffice # XXX: should return non-zero?
case "$(guix refresh -t test libreoffice 2>&1)" in
    *"greater than the latest known version"*"1.0"*) true;;
    *) false;;
esac

# Various ways to specify packages.
cat > "$manifest" <<EOF
(specifications->manifest (list "guile@3.0"))
EOF
default_IFS="$IFS"
IFS=_
for spec in "guile"					\
	    "guile@3.0"					\
	    "-e_(@ (gnu packages guile) guile-3.0)"	\
	    "-m_$manifest"				\
	    "-r_guile"					\
	    "-s_core"
do
    guix refresh -t test $spec
    case "$(guix refresh -t test $spec 2>&1)" in
	*"would be upgraded"*"12.5"*)
	    true;;
	*)
	    false;;
    esac
done
IFS="$default_IFS"

# Actually updating.
mkdir "$module_dir"
echo hello > "$module_dir/source"
cat > "$module_dir/sample.scm"<<EOF
(define-module (sample)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (gnu packages base))

(define-public my-thing
  (package
    (inherit hello)
    (name "the-test-package")
    (version "4.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/hello/hello-" version
                                  ".tar.gz"))
              (sha256
               (base32
                "086vqwk2wl8zfs47sq2xpjc9k066ilmb8z6dn0q6ymwjzlm196cd"))))
    (inputs (list coreutils tar))
    (properties '((updater-ignored-inputs . ("libreoffice"))))))
EOF
guix refresh -t test -L "$module_dir" the-test-package
guix refresh -t test -L "$module_dir" the-test-package -u \
     --keyring="$module_dir/keyring.kbx"  # so we don't create $HOME/.config
grep 'version "5.5"' "$module_dir/sample.scm"
grep "$(guix hash -H sha256 -f nix-base32 "$module_dir/source")" "$module_dir/sample.scm"
grep '(inputs (list grep sed))' "$module_dir/sample.scm"

# Specifying a target version.
guix refresh -t test guile=2.0.0 # XXX: should return non-zero?
case "$(guix refresh -t test guile=2.0.0 2>&1)" in
    *"failed to find"*"2.0.0"*) true;;
    *) false;;
esac

guix refresh -t test guile --target-version=2.0.0 # XXX: should return non-zero?
case "$(guix refresh -t test guile --target-version=2.0.0 2>&1)" in
    *"failed to find"*"2.0.0"*) true;;
    *) false;;
esac

# Partial target version => select the newest release prefixed by it.
guix refresh -t test guile --target-version=3 # XXX: should return non-zero?
case "$(guix refresh -t test guile --target-version=3 2>&1)" in
    *"would be upgraded"*"3.13.3"*) true;;
    *) false;;
esac

for spec in "guile=1.6.4" "guile@3=1.6.4"
do
    guix refresh -t test "$spec"
    case "$(guix refresh -t test "$spec" 2>&1)" in
	*"would be downgraded"*"1.6.4"*) true;;
	*) false;;
    esac
done

# Listing updaters.  This should work whether or not networking is available.
guix refresh --list-updaters
