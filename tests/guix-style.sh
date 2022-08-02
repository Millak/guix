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
# Test 'guix style'.
#

set -e

guix style --version

tmpdir="guix-style-$$"
trap 'rm -r "$tmpdir"' EXIT

tmpfile="$tmpdir/os.scm"
mkdir "$tmpdir"
cat > "$tmpfile" <<EOF
;;; This is a header with three semicolons.
;;;

(define-module (foo bar)
  #:use-module (guix)
  #:use-module (gnu))

;; One blank line and a page break.


;; And now, the OS.
(operating-system
  (host-name "komputilo")
  (locale "eo_EO.UTF-8")

  ;; User accounts.
  (users (cons (user-account
                 (name "alice")
                 (comment "Bob's sister")
                 (group "users")

                 ;; Groups fit on one line.
                 (supplementary-groups '("wheel" "audio" "video")))
               %base-user-accounts))

  ;; The services.
  (services
   (cons (service mcron-service-type) %base-services)))
EOF

cp "$tmpfile" "$tmpfile.bak"

initial_hash="$(guix hash "$tmpfile")"

guix style -f "$tmpfile"
if ! test "$initial_hash" = "$(guix hash "$tmpfile")"
then
    cat "$tmpfile"
    diff -u "$tmpfile.bak" "$tmpfile"
    false
fi

# Introduce random changes and try again.
sed -i "$tmpfile" -e's/ +/ /g'
! test "$initial_hash" = "$(guix hash "$tmpfile")"

guix style -f "$tmpfile"
test "$initial_hash" = "$(guix hash "$tmpfile")"
