# GNU Guix --- Functional package management for GNU
# Copyright © 2025 Nicolas Graves <ngraves@ngraves.fr>
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

import importlib.util
import json


def pytest_addoption(parser):
    """Add stub options to be ignored by pytest.

    More precisely, inject all options provided in .pytest_guix_options.json,
    except options whose plugin is indeed installed.

    For example, if the json file records --cov:
      if the pytest_cov module is installed, its --cov will be used.
      otherwise, --cov is ignored (read by this parser, but nothing is done
      with it).

    This allows to remove development packages, which are not required at build
    time while at the same time avoiding the need to adjust test options in
    pyproject.toml or other configuration files.
    """
    with open(".pytest_guix_options.json", "r") as options_file:
        plugin_options = json.load(options_file)
        group = parser.getgroup(
            "guix", "Options ignored by the Guix pyproject-build-system"
        )

        # Only add options for plugins that are not present.
        for key, options in plugin_options.items():
            if importlib.util.find_spec(f"pytest_{key}") is None:
                # Plugin not found, add stub options
                for option in options:
                    group.addoption(option, action="append", nargs="?")
