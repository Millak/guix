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

FILE=gnu/packages/rust-crates.scm
PATTERN='^(define rust-'

grep "$PATTERN" $FILE | cut -d' ' -f2 | while IFS= read -r crate
do
    if [ "$(grep -wc "$crate" $FILE)" -eq 1 ]; then
        echo "\
(begin
  (use-modules (guix utils))
  (let ((source-properties
         (find-definition-location \"$FILE\" '$crate #:define-prefix 'define)))
    (and=> source-properties delete-expression)))" |
            guix repl -t machine
    fi
done

# Delete extra newlines.
sed --in-place ':a;N;$!ba;s/\n\n\+/\n\n/g' $FILE
