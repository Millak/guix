#!/bin/sh
# GNU Guix --- Functional package management for GNU
# Copyright © 2025 Hilton Chain <hako@ultrarare.space>
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

# Usage: ./etc/teams/rust/unpack-new-crates.sh <DIR>
# Then inspect DIR with your tool of choice.
#
# For example:
# ./etc/teams/rust/unpack-new-crates.sh tmp-rust-crates &&
#     emacs --eval '(find-dired "tmp-rust-crates" "")'
#
# Note this uses ‘git diff’, only unstaged one are unpacked.

UNPACK_DIR="$1"
CHANGES="$(git diff gnu/packages/rust-crates.scm | grep -E '^\+\(define rust-' | cut -f2 -d' ')"

rm -rf "$UNPACK_DIR"
mkdir -p "$UNPACK_DIR"

for crate in $CHANGES
do
    built="$(./pre-inst-env guix build -e "(@@ (gnu packages rust-crates) "$crate")" -v0 2>/dev/null)"
    if [[ -n $built ]]; then
        if [[ -d $built ]]; then
            cp -r "$built" "$UNPACK_DIR"
        else
            tar xf "$built" -C "$UNPACK_DIR"
        fi
    fi
done
