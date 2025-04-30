# GNU Guix --- Functional package management for GNU
# Copyright © 2018 Julien Lepiller <julien@lepiller.eu>
# Copyright © 2021 Maxim Cournoyer <maxim.cournoyer@gmail.com>
# Copyright © 2024 Janneke Nieuwenhuizen <janneke@gnu.org>
# Copyright © 2024 gemmaro <gemmaro.dev@gmail.com>
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

DOC_PO_FILES =					\
  %D%/guix-manual.de.po				\
  %D%/guix-manual.es.po				\
  %D%/guix-manual.fr.po				\
  %D%/guix-manual.pt_BR.po			\
  %D%/guix-manual.ru.po				\
  %D%/guix-manual.zh_CN.po

DOC_COOKBOOK_PO_FILES =				\
  %D%/guix-cookbook.de.po			\
  %D%/guix-cookbook.es.po			\
  %D%/guix-cookbook.fr.po			\
  %D%/guix-cookbook.ko.po			\
  %D%/guix-cookbook.pt_BR.po			\
  %D%/guix-cookbook.ru.po			\
  %D%/guix-cookbook.sk.po			\
  %D%/guix-cookbook.sv.po

EXTRA_DIST = \
  %D%/po4a.cfg \
  %D%/guix-manual.pot \
  %D%/guix-cookbook.pot \
  $(DOC_PO_FILES) \
  $(DOC_COOKBOOK_PO_FILES)

%D%/%.pot: $(srcdir)/doc/%.texi
	$(AM_V_PO4A)$(PO4A) --no-translations -M UTF-8			\
	    --package-version "$(VERSION)"				\
	    --variable master="$<"					\
	    --variable pot="$@-t"					\
	    --variable po=/dev/null					\
	    --variable localized=/dev/null				\
	    $(POT_OPTIONS) %D%/po4a.cfg
	date="$$(git log --pretty=format:%ci -n 1 -- $< 2>/dev/null	\
		|| echo $(SOURCE_DATE_EPOCH))"				\
	sed -ri -e "s,^(.POT-Creation-Date: )[^\]*,\1$$date," "$@-t"
	mv "$@-t" "$@"

%D%/guix-manual.pot: %D%/guix.pot %D%/contributing.pot
	msgcat $^ > "$@-t"
	date="$$(git log --pretty=format:%ci -n 1 -- $< 2>/dev/null	\
		|| echo $(SOURCE_DATE_EPOCH))"				\
	sed -ri "s,^(.POT-Creation-Date: )[^\]*,\1$$date," "$@-t"
	mv "$@-t" "$@"

doc-pot-update: %D%/guix-manual.pot %D%/guix-cookbook.pot
.PHONY: doc-pot-update
