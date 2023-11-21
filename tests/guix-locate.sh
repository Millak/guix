# GNU Guix --- Functional package management for GNU
# Copyright © 2023 Antoine R. Dumont <antoine.romain.dumont@gmail.com>
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
# Test the 'guix locate' command-line utility.
#

set -x

RUN_EXPENSIVE_TESTS="${RUN_EXPENSIVE_TESTS:-false}"

tmpdir="guix-index-$$"
# In the following tests, we use two different databases, one for each
# indexation method.
tmpdb_manifests="$tmpdir/manifests/db.sqlite"
tmpdb_store="$tmpdir/store/db.sqlite"
trap 'rm -rf "$tmpdir" "$tmpdb_store" "$tmpdb_manifests"' EXIT

guix locate --version

# Preparing db locations for both indexation methods.
mkdir -p "$(dirname "$tmpdb_manifests")" "$(dirname "$tmpdb_store")"

cmd_manifests="guix locate --database=$tmpdb_manifests --method=manifests"
cmd_store="guix locate --database=$tmpdb_store --method=store"

# Lookup without any db should fail.
guix locate --database="$tmpdb_manifests" guile && false
guix locate --database="$tmpdb_store" guile && false

# Lookup without anything in db should yield no results because the indexer
# didn't stumble upon any profile.
test -z "$(guix locate --database="$tmpdb_manifests" guile)"

# Install a package.
guix package --bootstrap --install guile-bootstrap \
     --profile="$tmpdir/profile"

# Look for 'guile'.
$cmd_manifests --update
$cmd_manifests guile | grep "$(guix build guile-bootstrap)/bin/guile"
$cmd_manifests boot-9.scm | grep ^guile-bootstrap

# Using a glob pattern.
$cmd_manifests -g '*.scm' | grep "^guile-bootstrap.*boot-9"

# Statistics.
$cmd_manifests --stats

if $RUN_EXPENSIVE_TESTS
then
    $cmd_store --update
    $cmd_store guile
    $cmd_store guile | grep "$(guix build guile-bootstrap)/bin/guile"
    $cmd_store boot-9.scm | grep ^guile-bootstrap
fi

# The command below is an error: "no files to search for"...
guix locate && false

# ... but this one is fine.
guix locate --clear
