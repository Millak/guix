# GNU Guix --- Functional package management for GNU
# Copyright © 2023 Maxim Cournoyer <maxim@guixotic.coop>
# Copyright © 2023-2024, 2026 Ludovic Courtès <ludo@gnu.org>
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
# Test the 'guix time-machine' command-line utility.
#

channels_file="channels-test-$$.scm"
trap "rm -f $channels_file" EXIT

cat > "$channels_file" <<EOF
(system "echo rm -rf /")
%default-channels
EOF

# This must fail: 'system' is not among the available bindings in the
# evaluation sandbox.
guix time-machine -C "$channels_file" && echo false

cat > "$channels_file" <<EOF
(use-modules (system foreign))
%default-channels
EOF

# Likewise, 'use-modules' is not available.
guix time-machine -C "$channels_file" && echo false

if [ -d "$abs_top_srcdir/.git" ] \
   || guile -c '(getaddrinfo "www.gnu.org" "80" AI_NUMERICSERV)' 2> /dev/null
then
    guix time-machine --version
else
    echo "This test requires networking or a local Git checkout; skipping." >&2
    exit 77
fi

if [ -d "$abs_top_srcdir/.git" ]
then
    # Note: No "file://" prefix because that makes cloning much more expensive
    # for some reason.
    EXTRA_OPTIONS="--url=$abs_top_srcdir"
else
    EXTRA_OPTIONS=""
fi

# Visiting a commit older than v0.16.0 must fail (this test is expensive
# because it clones the whole repository).
guix time-machine -q --commit=v0.15.0 $EXTRA_OPTIONS -- describe && false

true
