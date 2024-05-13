# GNU Guix --- Functional package management for GNU
# Copyright © 2012, 2015-2016, 2024 Ludovic Courtès <ludo@gnu.org>
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

# Define some files/folders needed for the tests.
output="t-download-$$"
test_git_repo="$(mktemp -d)"
output_dir="t-archive-dir-$$"
trap 'rm -rf "$test_git_repo" ; rm -f "$output" ; rm -rf "$output_dir"' EXIT

#
# Test the `guix download' command-line utility.
#

guix download --version

# Make sure it fails here.
guix download http://does.not/exist && false

guix download unknown://some/where && false

guix download /does-not-exist && false

# This one should succeed.
guix download "file://$abs_top_srcdir/README"

# And this one, without the URI scheme.
guix download "$abs_top_srcdir/README"

# This one too, even if it cannot talk to the daemon.
GUIX_DAEMON_SOCKET="/nowhere" guix download -o "$output" \
		  "file://$abs_top_srcdir/README"
cmp "$output" "$abs_top_srcdir/README"

# This one should fail.
guix download "file:///does-not-exist" "file://$abs_top_srcdir/README" && false

# Test git support with local repository.
# First, create a dummy git repo in the temporary directory.
(
    # Avoid interference with user config.
    GIT_CONFIG_GLOBAL=/dev/null
    GIT_CONFIG_SYSTEM=/dev/null
    export GIT_CONFIG_SYSTEM GIT_CONFIG_GLOBAL

    cd $test_git_repo
    git init
    touch test
    git config user.name "User"
    git config user.email "user@domain"
    git add test
    git commit -m "Commit"
    git tag -a -m "v1" v1
)

# Extract commit number.
commit=$((cd $test_git_repo && git log) | head -n 1 | cut -f2 -d' ')

# We expect that guix hash is working properly or at least that the output of
# 'guix download' is consistent with 'guix hash'.
expected_hash=$(guix hash -rx $test_git_repo)

# Test the different options
for option in "" "--commit=$commit" "--commit=v1" "--branch=master"
do
    command_output="$(guix download --git $option "file://$test_git_repo")"
    computed_hash="$(echo $command_output | cut -f2 -d' ')"
    store_path="$(echo $command_output | cut -f1 -d' ')"
    [ "$expected_hash" = "$computed_hash" ]
    diff -r -x ".git" $test_git_repo $store_path
done

# Should fail.
guix download --git --branch=non_existent "file://$test_git_repo" && false

# Same but download to file instead of store.
guix download --git "file://$test_git_repo" -o $output_dir
diff -r -x ".git" $test_git_repo $output_dir

exit 0
