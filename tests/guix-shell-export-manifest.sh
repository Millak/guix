# GNU Guix --- Functional package management for GNU
# Copyright © 2022 Ludovic Courtès <ludo@gnu.org>
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
# Test 'guix shell --export-manifest'.
#

guix shell --version

tmpdir="t-guix-manifest-$$"
trap 'rm -r "$tmpdir"' EXIT
mkdir "$tmpdir"

manifest="$tmpdir/manifest.scm"

# Basics.
guix shell --export-manifest guile-bootstrap > "$manifest"
test "$(guix build -m "$manifest")" = "$(guix build guile-bootstrap)"

guix shell -m "$manifest" --bootstrap -- \
     "$SHELL" -c 'guix package --export-manifest -p "$GUIX_ENVIRONMENT"' > \
     "$manifest.second"
for m in "$manifest" "$manifest.second"
do
    grep -v '^;' < "$m" > "$m.new" # filter out comments
    mv "$m.new" "$m"
done

cat "$manifest"
cat "$manifest.second"

cmp "$manifest" "$manifest.second"

# Combining manifests.
guix shell --export-manifest -m "$manifest" gash gash-utils \
     > "$manifest.second"
guix build -m "$manifest.second" -d | \
    grep "$(guix build guile-bootstrap -d)"
guix build -m "$manifest.second" -d | \
    grep "$(guix build gash -d)"

# Package transformation option.
guix shell --export-manifest guile guix --with-latest=guile-json > "$manifest"
grep 'options->transformation' "$manifest"
grep '(with-latest . "guile-json")' "$manifest"

# Development manifest.
guix shell --export-manifest -D guile git > "$manifest"
grep 'package->development-manifest' "$manifest"
grep '"guile"' "$manifest"
guix build -m "$manifest" -d | \
    grep "$(guix build -e '(@@ (gnu packages commencement) gcc-final)' -d)"
guix build -m "$manifest" -d | \
    grep "$(guix build git -d)"

# Test various combinations to make sure generated code uses interfaces
# correctly.
for options in					\
    "coreutils grep sed"			\
    "gsl openblas gcc-toolchain --tune"		\
    "guile -m $manifest.previous"		\
    "git:send-email gdb guile:debug"		\
    "git -D coreutils"
do
    guix shell --export-manifest $options > "$manifest"
    cat "$manifest"
    guix shell -m "$manifest" -n
    mv "$manifest" "$manifest.previous"
done
