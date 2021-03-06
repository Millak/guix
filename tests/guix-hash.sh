# GNU Guix --- Functional package management for GNU
# Copyright © 2013, 2014, 2016, 2020, 2021 Ludovic Courtès <ludo@gnu.org>
# Copyright © 2016 Jan Nieuwenhuizen <janneke@gnu.org>
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
# Test the `guix hash' command-line utility.
#

guix hash --version

tmpdir="guix-hash-$$"
trap 'rm -rf "$tmpdir"' EXIT

test `guix hash /dev/null` = 0mdqa9w1p6cmli6976v4wi0sw9r4p5prkj7lzfd1877wk11c9c73
test `echo -n | guix hash -` = 0mdqa9w1p6cmli6976v4wi0sw9r4p5prkj7lzfd1877wk11c9c73
test `guix hash -f nix-base32 /dev/null` = 0mdqa9w1p6cmli6976v4wi0sw9r4p5prkj7lzfd1877wk11c9c73
test `guix hash -f hex /dev/null` = e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855
test `guix hash -f base32 /dev/null` = 4oymiquy7qobjgx36tejs35zeqt24qpemsnzgtfeswmrw6csxbkq
test `guix hash -H sha512 -f hex /dev/null` = cf83e1357eefb8bdf1542850d66d8007d620e4050b5715dc83f4a921d36ce9ce47d0d13c5d85f2b0ff8318d2877eec2f63b931bd47417a81a538327af927da3e
test `guix hash -H sha1 -f base64 /dev/null` = "2jmj7l5rSw0yVb/vlWAYkK/YBwk="

! guix hash -H abcd1234 /dev/null

mkdir "$tmpdir"
echo -n executable > "$tmpdir/exe"
chmod +x "$tmpdir/exe"
( cd "$tmpdir" ; ln -s exe symlink )
mkdir "$tmpdir/subdir"

test `guix hash -r "$tmpdir"` = 10k1lw41wyrjf9mxydi0is5nkpynlsvgslinics4ppir13g7d74p
test `guix hash -r "$tmpdir" -H sha512` = 301ra58c2vahczzxiyfin41mpyb0ljh4dh9zn3ijvwviaw1j40sfzw5skh9x945da88n3785ggifzig7acd6k72h0mpsc20m1f66m9n

# Without '-r', this should fail.
! guix hash "$tmpdir"

# This should fail because /dev/null is a character device, which
# the archive format doesn't support.
! guix hash -r /dev/null

# Adding a .git directory
mkdir "$tmpdir/.git"
touch "$tmpdir/.git/foo"

# ...changes the hash
test `guix hash -r $tmpdir` = 0a50z04zyzf7pidwxv0nwbj82pgzbrhdy9562kncnvkcfvb48m59

# ...but remains the same when using `-x'
test `guix hash -r $tmpdir -x` = 10k1lw41wyrjf9mxydi0is5nkpynlsvgslinics4ppir13g7d74p

# Without '-r', this should fail.
! guix hash "$tmpdir"

