# GNU Guix --- Functional package management for GNU
# Copyright © 2023 Maxim Cournoyer <maxim.cournoyer@gmail.com>
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

guix time-machine --version

# Visiting a commit older than v1.0.0 fails.
! guix time-machine --commit=v0.15.0

exit 0
