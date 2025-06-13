# GNU Guix --- Functional package management for GNU
# Copyright © 2017, 2018 Nikita <nikita@n0.is>
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

# FIXME: Fish doesn't support GNU-style long options.
# Completions in this file add ‘=’ suffixes for visual indication, but they
# will complete ‘--load-path= ’ instead of ‘--load-path=’, for example.

function __fish_guix_needs_command
    set cmd (commandline -opc)
    if [ (count $cmd) -eq 1 ]
        return 0
    else
        set -l skip_next 1
        # Skip first word because it's "guix"
        for c in $cmd[2..-1]
            test $skip_next -eq 0
            and set skip_next 1
            and continue
            switch $c
                # General options that can still take a command
                case "=*"
                    continue
                    # case --asdf
                    #     set skip_next 0
                    #     continue
                    # these behave like commands and everything after them is ignored
                case "--help" "--version"
                    return 1
                    # We assume that any other token that's not an argument to a general option is a command
                case "*"
                    echo $c
                    return 1
            end
        end
        return 0
    end
    return 1
end

function __fish_guix_using_command
    set -l cmd (__fish_guix_needs_command)
    test -z "$cmd"
    and return 1
    contains -- $cmd $argv
    and return 0
end

function __fish_guix_complete_subcommand
    # Support nested ‘guix shell’, ‘guix deploy’, ‘guix time-machine’ etc.
    set -l time_machine
    set -l tokens (commandline -opc)
    while true
        set -l index (contains -i -- "--" $tokens)
        if [ -z "$index" ]
            break
        end
        set time_machine (contains -i -- "time-machine" $tokens[1..$index])
        set -e tokens[1..$index]
    end
    if [ -n "$time_machine" ]
        complete -C "guix $tokens $(commandline -ct)"
    else
        complete -C "$tokens $(commandline -ct)"
    end
end

set -l guix_substitute_urls "https://bordeaux.guix.gnu.org https://ci.guix.gnu.org"

complete -f -c guix -s h -l help    -d "display help message"
complete -f -c guix -s V -l version -d "display version information"


# Generate option lists and simple completions, which may take too long to
# compute at run time.
#
# echo set -l guix_build_systems \\
# for command in $(command guix build --list-systems | string match -rg '^   - ([a-z0-9-_]+)')
#     echo "    $command \\"
# end
# echo set -l guix_build_targets \\
# for command in $(command guix build --list-targets | string match -rg '^   - ([a-z0-9-_]+)')
#     echo "    $command \\"
# end
# echo set -l guix_describe_formats \\
# for command in $(guix describe --list-formats | string match -rg '^  - ([a-z0-9-_]+)')
#     echo "    $command \\"
# end
# echo set -l guix_graph_backends \\
# for command in $(guix graph --list-backends | string match -rg '^  - ([a-z0-9-_]+)')
#     echo "    $command \\"
# end
# echo set -l guix_graph_types \\
# for command in $(guix graph --list-types | string match -rg '^  - ([a-z0-9-_]+)')
#     echo "    $command \\"
# end
# echo set -l guix_image_types \\
# for type in $(guix system --list-image-types | string match -rg "^   - ([a-z0-9-_]+)")
#     echo "    $type \\"
# end
# echo set -l guix_importers \\
# for importer in $(command guix import --help | string match -rg '^   ([a-z0-9-_]+)')
#     echo "    $importer \\"
# end
# echo set -l guix_lint_checker \\
# for checker in $(guix lint --list-checkers | string match -rg "^- ([a-z0-9-_]+):")
#     echo "    $checker \\"
# end
# echo set -l guix_pack_formats \\
# for format in $(guix pack --list-formats | string match -rg "^  ([a-z0-9-_]+)")
#     echo "    $format \\"
# end
# echo set -l guix_processes_formats \\
# for format in $(guix processes --list-formats | string match -rg '^  - ([a-z0-9-_]*)')
#     echo "    $format \\"
# end
# echo set -l guix_refresh_updaters \\
# for updater in $(guix refresh --list-updaters | string match -rg "^  - ([a-z0-9-_]+):")
#     echo "    $updater \\"
# end
# echo set -l guix_repl_types \\
# for type in $(guix repl --list-types)
#     echo "    $type \\"
# end
# echo set -l guix_style_stylings \\
# for styling in $(guix style --list-stylings | string match -rg "^- ([a-z0-9-_]+):")
#     echo "    $styling \\"
# end
# echo set -l guix_commands \\
# for command in $(command guix --help | string match -rg '^    ([a-z0-9-]+)')
#     echo "    $command \\"
# end
# for command in $(command guix --help | string match -rg '^    ([a-z0-9-]+)')
#     echo "complete -f -c guix -n \"__fish_guix_needs_command\" -a \"$command\" -d \"$(guix --help | string match -rg "^    $command +(.*)")\""
# end
# echo set -l guix_system_commands \\
# for command in $(command guix system --help | string match -rg '^   ([a-z0-9-]+)')
#     echo "    $command \\"
# end
# for command in $(command guix system --help | string match -rg '^   ([a-z0-9-]+)')
#     echo "complete -f -c guix -n \"__fish_guix_using_command system; and not __fish_seen_subcommand_from \$guix_system_commands\" -a \"$command\" -d \"$(guix system --help | string match -rg "^   $command +(.*)")\""
# end
# echo set -l guix_home_commands \\
# for command in $(command guix home --help | string match -rg '^   ([a-z0-9-]+)')
#     echo "    $command \\"
# end
# for command in $(command guix home --help | string match -rg '^   ([a-z0-9-]+)')
#     echo "complete -f -c guix -n \"__fish_guix_using_command home; and not __fish_seen_subcommand_from \$guix_home_commands\" -a \"$command\" -d \"$(echo $(guix home --help | string match -rg "^   $command +(.*)"))\""
# end

# Generated output
set -l guix_build_systems \
    aarch64-linux \
    armhf-linux \
    i586-gnu \
    i686-linux \
    mips64el-linux \
    powerpc-linux \
    powerpc64le-linux \
    riscv64-linux \
    x86_64-gnu \
    x86_64-linux
set -l guix_build_targets \
    aarch64-linux-gnu \
    arm-linux-gnueabihf \
    avr \
    i586-pc-gnu \
    i686-linux-gnu \
    i686-w64-mingw32 \
    loongarch64-linux-gnu \
    mips64el-linux-gnu \
    or1k-elf \
    powerpc-linux-gnu \
    powerpc64-linux-gnu \
    powerpc64le-linux-gnu \
    riscv64-linux-gnu \
    x86_64-linux-gnu \
    x86_64-linux-gnux32 \
    x86_64-pc-gnu \
    x86_64-w64-mingw32 \
    xtensa-ath9k-elf
set -l guix_describe_formats \
    human \
    channels \
    channels-sans-intro \
    json \
    recutils
set -l guix_graph_backends \
    graphviz \
    d3js \
    cypher \
    graphml
set -l guix_graph_types \
    package \
    reverse-package \
    bag \
    bag-with-origins \
    bag-emerged \
    reverse-bag \
    derivation \
    references \
    referrers \
    module
set -l guix_image_types \
    docker \
    efi-raw \
    efi32-raw \
    hurd-qcow2 \
    hurd-raw \
    hurd64-qcow2 \
    hurd64-raw \
    iso9660 \
    mbr-hybrid-raw \
    mbr-raw \
    novena-raw \
    orangepi-r1-plus-lts-rk3328-raw \
    pine64-raw \
    pinebook-pro-raw \
    qcow2 \
    raw-with-offset \
    rock64-raw \
    tarball \
    uncompressed-iso9660 \
    unmatched-raw \
    visionfive2-raw \
    wsl2
set -l guix_importers \
    composer \
    cpan \
    cran \
    crate \
    egg \
    elm \
    elpa \
    gem \
    gnu \
    go \
    hackage \
    hexpm \
    json \
    luanti \
    npm-binary \
    nuget \
    opam \
    pypi \
    stackage \
    texlive
set -l guix_lint_checker \
    name \
    tests-true \
    compiler-for-target \
    description \
    inputs-should-be-native \
    inputs-should-not-be-input \
    inputs-should-be-minimal \
    input-labels \
    wrapper-inputs \
    license \
    optional-tests \
    mirror-url \
    source-file-name \
    source-unstable-tarball \
    misplaced-flags \
    derivation \
    profile-collisions \
    patch-file-names \
    patch-headers \
    formatting \
    synopsis \
    gnu-description \
    home-page \
    source \
    github-url \
    cve \
    refresh \
    archival \
    haskell-stackage
set -l guix_pack_formats \
    tarball \
    squashfs \
    docker \
    deb \
    rpm \
    appimage
set -l guix_processes_formats \
    recutils \
    normalized
set -l guix_refresh_updaters \
    bioconductor \
    composer \
    cpan \
    cran \
    crate \
    egg \
    elpa \
    gem \
    github \
    gnome \
    gnu \
    gnu-ftp \
    hackage \
    hexpm \
    kde \
    launchpad \
    luanti \
    opam \
    pypi \
    savannah \
    sourceforge \
    stackage \
    test \
    texlive \
    xorg \
    generic-git \
    generic-html
set -l guix_repl_types \
    guile \
    machine
set -l guix_style_stylings \
    format \
    inputs \
    arguments
set -l guix_commands \
    deploy \
    describe \
    gc \
    home \
    install \
    locate \
    package \
    pull \
    remove \
    search \
    show \
    system \
    time-machine \
    upgrade \
    weather \
    container \
    environment \
    pack \
    shell \
    build \
    challenge \
    download \
    edit \
    graph \
    hash \
    import \
    lint \
    publish \
    refresh \
    size \
    style \
    archive \
    copy \
    git \
    offload \
    processes \
    repl
complete -f -c guix -n "__fish_guix_needs_command" -a "deploy" -d "deploy operating systems on a set of machines"
complete -f -c guix -n "__fish_guix_needs_command" -a "describe" -d "describe the channel revisions currently used"
complete -f -c guix -n "__fish_guix_needs_command" -a "gc" -d "invoke the garbage collector"
complete -f -c guix -n "__fish_guix_needs_command" -a "home" -d "build and deploy home environments"
complete -f -c guix -n "__fish_guix_needs_command" -a "install" -d "install packages"
complete -f -c guix -n "__fish_guix_needs_command" -a "locate" -d "search for packages providing a given file"
complete -f -c guix -n "__fish_guix_needs_command" -a "package" -d "manage packages and profiles"
complete -f -c guix -n "__fish_guix_needs_command" -a "pull" -d "pull the latest revision of Guix"
complete -f -c guix -n "__fish_guix_needs_command" -a "remove" -d "remove installed packages"
complete -f -c guix -n "__fish_guix_needs_command" -a "search" -d "search for packages"
complete -f -c guix -n "__fish_guix_needs_command" -a "show" -d "show information about packages"
complete -f -c guix -n "__fish_guix_needs_command" -a "system" -d "build and deploy full operating systems"
complete -f -c guix -n "__fish_guix_needs_command" -a "time-machine" -d "run commands from a different revision"
complete -f -c guix -n "__fish_guix_needs_command" -a "upgrade" -d "upgrade packages to their latest version"
complete -f -c guix -n "__fish_guix_needs_command" -a "weather" -d "report on the availability of pre-built package binaries"
complete -f -c guix -n "__fish_guix_needs_command" -a "container" -d "run code in containers created by 'guix environment -C'"
complete -f -c guix -n "__fish_guix_needs_command" -a "environment" -d "spawn one-off software environments (deprecated)"
complete -f -c guix -n "__fish_guix_needs_command" -a "pack" -d "create application bundles"
complete -f -c guix -n "__fish_guix_needs_command" -a "shell" -d "spawn one-off software environments"
complete -f -c guix -n "__fish_guix_needs_command" -a "build" -d "build packages or derivations without installing them"
complete -f -c guix -n "__fish_guix_needs_command" -a "challenge" -d "challenge substitute servers, comparing their binaries"
complete -f -c guix -n "__fish_guix_needs_command" -a "download" -d "download a file to the store and print its hash"
complete -f -c guix -n "__fish_guix_needs_command" -a "edit" -d "view and edit package definitions"
complete -f -c guix -n "__fish_guix_needs_command" -a "graph" -d "view and query package dependency graphs"
complete -f -c guix -n "__fish_guix_needs_command" -a "hash" -d "compute the cryptographic hash of a file"
complete -f -c guix -n "__fish_guix_needs_command" -a "import" -d "import a package definition from an external repository"
complete -f -c guix -n "__fish_guix_needs_command" -a "lint" -d "validate package definitions"
complete -f -c guix -n "__fish_guix_needs_command" -a "publish" -d "publish build results over HTTP"
complete -f -c guix -n "__fish_guix_needs_command" -a "refresh" -d "update existing package definitions"
complete -f -c guix -n "__fish_guix_needs_command" -a "size" -d "profile the on-disk size of packages"
complete -f -c guix -n "__fish_guix_needs_command" -a "style" -d "update the style of package definitions"
complete -f -c guix -n "__fish_guix_needs_command" -a "archive" -d "manipulate, export, and import normalized archives (nars)"
complete -f -c guix -n "__fish_guix_needs_command" -a "copy" -d "copy store items remotely over SSH"
complete -f -c guix -n "__fish_guix_needs_command" -a "git" -d "operate on Git repositories"
complete -f -c guix -n "__fish_guix_needs_command" -a "offload" -d "set up and operate build offloading"
complete -f -c guix -n "__fish_guix_needs_command" -a "processes" -d "list currently running sessions"
complete -f -c guix -n "__fish_guix_needs_command" -a "repl" -d "read-eval-print loop (REPL) for interactive programming"
set -l guix_system_commands \
    search \
    edit \
    reconfigure \
    roll-back \
    describe \
    list-generations \
    switch-generation \
    delete-generations \
    build \
    container \
    vm \
    image \
    docker-image \
    init \
    installer \
    extension-graph \
    shepherd-graph
complete -f -c guix -n "__fish_guix_using_command system; and not __fish_seen_subcommand_from $guix_system_commands" -a "search" -d "search for existing service types"
complete -f -c guix -n "__fish_guix_using_command system; and not __fish_seen_subcommand_from $guix_system_commands" -a "edit" -d "edit the definition of an existing service type"
complete -f -c guix -n "__fish_guix_using_command system; and not __fish_seen_subcommand_from $guix_system_commands" -a "reconfigure" -d "switch to a new operating system configuration"
complete -f -c guix -n "__fish_guix_using_command system; and not __fish_seen_subcommand_from $guix_system_commands" -a "roll-back" -d "switch to the previous operating system configuration"
complete -f -c guix -n "__fish_guix_using_command system; and not __fish_seen_subcommand_from $guix_system_commands" -a "describe" -d "describe the current system"
complete -f -c guix -n "__fish_guix_using_command system; and not __fish_seen_subcommand_from $guix_system_commands" -a "list-generations" -d "list the system generations"
complete -f -c guix -n "__fish_guix_using_command system; and not __fish_seen_subcommand_from $guix_system_commands" -a "switch-generation" -d "switch to an existing operating system configuration"
complete -f -c guix -n "__fish_guix_using_command system; and not __fish_seen_subcommand_from $guix_system_commands" -a "delete-generations" -d "delete old system generations"
complete -f -c guix -n "__fish_guix_using_command system; and not __fish_seen_subcommand_from $guix_system_commands" -a "build" -d "build the operating system without installing anything"
complete -f -c guix -n "__fish_guix_using_command system; and not __fish_seen_subcommand_from $guix_system_commands" -a "container" -d "build a container that shares the host's store"
complete -f -c guix -n "__fish_guix_using_command system; and not __fish_seen_subcommand_from $guix_system_commands" -a "vm" -d "build a virtual machine image that shares the host's store"
complete -f -c guix -n "__fish_guix_using_command system; and not __fish_seen_subcommand_from $guix_system_commands" -a "image" -d "build a Guix System image"
complete -f -c guix -n "__fish_guix_using_command system; and not __fish_seen_subcommand_from $guix_system_commands" -a "docker-image" -d "build a Docker image"
complete -f -c guix -n "__fish_guix_using_command system; and not __fish_seen_subcommand_from $guix_system_commands" -a "init" -d "initialize a root file system to run GNU"
complete -f -c guix -n "__fish_guix_using_command system; and not __fish_seen_subcommand_from $guix_system_commands" -a "installer" -d "run the graphical installer"
complete -f -c guix -n "__fish_guix_using_command system; and not __fish_seen_subcommand_from $guix_system_commands" -a "extension-graph" -d "emit the service extension graph in Dot format"
complete -f -c guix -n "__fish_guix_using_command system; and not __fish_seen_subcommand_from $guix_system_commands" -a "shepherd-graph" -d "emit the graph of shepherd services in Dot format"
set -l guix_home_commands \
    search \
    edit \
    container \
    reconfigure \
    roll-back \
    describe \
    list-generations \
    switch-generation \
    delete-generations \
    build \
    import \
    extension-graph \
    shepherd-graph
complete -f -c guix -n "__fish_guix_using_command home; and not __fish_seen_subcommand_from $guix_home_commands" -a "search" -d "search for existing service types"
complete -f -c guix -n "__fish_guix_using_command home; and not __fish_seen_subcommand_from $guix_home_commands" -a "edit" -d "edit the definition of an existing service type"
complete -f -c guix -n "__fish_guix_using_command home; and not __fish_seen_subcommand_from $guix_home_commands" -a "container" -d "run the home environment configuration in a container"
complete -f -c guix -n "__fish_guix_using_command home; and not __fish_seen_subcommand_from $guix_home_commands" -a "reconfigure" -d "switch to a new home environment configuration"
complete -f -c guix -n "__fish_guix_using_command home; and not __fish_seen_subcommand_from $guix_home_commands" -a "roll-back" -d "switch to the previous home environment configuration"
complete -f -c guix -n "__fish_guix_using_command home; and not __fish_seen_subcommand_from $guix_home_commands" -a "describe" -d "describe the current home environment"
complete -f -c guix -n "__fish_guix_using_command home; and not __fish_seen_subcommand_from $guix_home_commands" -a "list-generations" -d "list the home environment generations"
complete -f -c guix -n "__fish_guix_using_command home; and not __fish_seen_subcommand_from $guix_home_commands" -a "switch-generation" -d "switch to an existing home environment configuration"
complete -f -c guix -n "__fish_guix_using_command home; and not __fish_seen_subcommand_from $guix_home_commands" -a "delete-generations" -d "delete old home environment generations"
complete -f -c guix -n "__fish_guix_using_command home; and not __fish_seen_subcommand_from $guix_home_commands" -a "build" -d "build the home environment without installing anything"
complete -f -c guix -n "__fish_guix_using_command home; and not __fish_seen_subcommand_from $guix_home_commands" -a "import" -d "generates a home environment definition from dotfiles"
complete -f -c guix -n "__fish_guix_using_command home; and not __fish_seen_subcommand_from $guix_home_commands" -a "extension-graph" -d "emit the service extension graph"
complete -f -c guix -n "__fish_guix_using_command home; and not __fish_seen_subcommand_from $guix_home_commands" -a "shepherd-graph" -d "emit the graph of shepherd services"


# Standard build options
set -l guix_commands_with_build_options \
    archive \
    build \
    copy \
    deploy \
    environment \
    home \
    install \
    pack \
    package \
    pull \
    system \
    time-machine \
    upgrade

complete -x -c guix -n "__fish_guix_using_command $guix_commands_with_build_options" -s L -l load-path=        -d "prepend DIR to the package module search path" -a "(__fish_complete_directories)"
complete -f -c guix -n "__fish_guix_using_command $guix_commands_with_build_options" -s K -l keep-failed       -d "keep build tree of failed builds"
complete -f -c guix -n "__fish_guix_using_command $guix_commands_with_build_options" -s k -l keep-going        -d "keep going when some of the derivations fail"
complete -x -c guix -n "__fish_guix_using_command $guix_commands_with_build_options"      -l rounds=           -d "build N times in a row to detect non-determinism"
complete -f -c guix -n "__fish_guix_using_command $guix_commands_with_build_options"      -l fallback          -d "fall back to building when the substituter fails"
complete -f -c guix -n "__fish_guix_using_command $guix_commands_with_build_options"      -l no-substitutes    -d "build instead of resorting to pre-built substitutes"
complete -x -c guix -n "__fish_guix_using_command $guix_commands_with_build_options"      -l substitute-urls=  -d "fetch substitute from URLS if they are authorized" -a "$guix_substitute_urls"
complete -f -c guix -n "__fish_guix_using_command $guix_commands_with_build_options"      -l no-grafts         -d "do not graft packages"
complete -f -c guix -n "__fish_guix_using_command $guix_commands_with_build_options"      -l no-offload        -d "do not attempt to offload builds"
complete -x -c guix -n "__fish_guix_using_command $guix_commands_with_build_options"      -l max-silent-time=  -d "mark the build as failed after SECONDS of silence"
complete -x -c guix -n "__fish_guix_using_command $guix_commands_with_build_options"      -l timeout=          -d "mark the build as failed after SECONDS of activity"
complete -x -c guix -n "__fish_guix_using_command $guix_commands_with_build_options"      -l debug=            -d "produce debugging output at LEVEL"
complete -x -c guix -n "__fish_guix_using_command $guix_commands_with_build_options" -s c -l cores=            -d "allow the use of up to N CPU cores for the build"
complete -x -c guix -n "__fish_guix_using_command $guix_commands_with_build_options" -s M -l max-jobs=         -d "allow at most N build jobs"


# Cross build options.
set -l guix_commands_with_cross_build_options \
    archive \
    build \
    pack \
    system

complete -f -c guix -n "__fish_guix_using_command $guix_commands_with_cross_build_options" -l list-targets -d "list available targets"
complete -x -c guix -n "__fish_guix_using_command $guix_commands_with_cross_build_options" -l target=      -d "cross-build for TRIPLET" -a "$guix_build_targets"


# Native build options.
set -l guix_commands_with_native_build_options \
    archive \
    build \
    environment \
    graph \
    pack \
    pull \
    shell \
    size \
    system \
    weather

complete -f -c guix -n "__fish_guix_using_command $guix_commands_with_native_build_options" -l list-systems -d "list available systems"
complete -x -c guix -n "__fish_guix_using_command $guix_commands_with_native_build_options" -l system=      -d "attempt to build for SYSTEM" -a "$guix_build_systems"


# Transformation options.
set -l guix_commands_with_transformation_options \
    build \
    environment \
    graph \
    install \
    pack \
    package \
    shell \
    upgrade

complete -x -c guix -n "__fish_guix_using_command $guix_commands_with_transformation_options" -l with-source=         -d "use SOURCE when building the corresponding package"
complete -x -c guix -n "__fish_guix_using_command $guix_commands_with_transformation_options" -l with-input=          -d "replace dependency PACKAGE by REPLACEMENT"
complete -x -c guix -n "__fish_guix_using_command $guix_commands_with_transformation_options" -l with-graft=          -d "graft REPLACEMENT on packages that refer to PACKAGE"
complete -x -c guix -n "__fish_guix_using_command $guix_commands_with_transformation_options" -l with-branch=         -d "build PACKAGE from the latest commit of BRANCH"
complete -x -c guix -n "__fish_guix_using_command $guix_commands_with_transformation_options" -l with-commit=         -d "build PACKAGE from COMMIT"
complete -x -c guix -n "__fish_guix_using_command $guix_commands_with_transformation_options" -l with-git-url=        -d "build PACKAGE from the repository at URL"
complete -x -c guix -n "__fish_guix_using_command $guix_commands_with_transformation_options" -l with-c-toolchain=    -d "add FILE to the list of patches of PACKAGE"
complete -f -c guix -n "__fish_guix_using_command $guix_commands_with_transformation_options" -l tune                 -d "tune relevant packages for CPU--e.g., \"skylake\"" -a "native generic"
complete -x -c guix -n "__fish_guix_using_command $guix_commands_with_transformation_options" -l with-debug-info=     -d "append FLAG to the configure flags of PACKAGE"
complete -x -c guix -n "__fish_guix_using_command $guix_commands_with_transformation_options" -l without-tests=       -d "use the latest upstream release of PACKAGE"
complete -x -c guix -n "__fish_guix_using_command $guix_commands_with_transformation_options" -l with-configure-flag= -d "use the given upstream VERSION of PACKAGE"
complete -x -c guix -n "__fish_guix_using_command $guix_commands_with_transformation_options" -l with-patch=          -d "build PACKAGE and its dependents with TOOLCHAIN"
complete -x -c guix -n "__fish_guix_using_command $guix_commands_with_transformation_options" -l with-latest=         -d "build PACKAGE and preserve its debug info"
complete -x -c guix -n "__fish_guix_using_command $guix_commands_with_transformation_options" -l with-version=        -d "build PACKAGE without running its tests"
complete -x -c guix -n "__fish_guix_using_command $guix_commands_with_transformation_options" -l help-transform       -d "list package transformation options not shown here"


# Environment options.
set -l guix_commands_with_environment_options \
    environment \
    shell

complete -x -c guix -n "__fish_guix_using_command $guix_commands_with_environment_options" -s e -l expression=   -d "create environment for the package that EXPR evaluates to"
complete -r -c guix -n "__fish_guix_using_command $guix_commands_with_environment_options" -s m -l manifest=     -d "create environment with the manifest from FILE"
complete -x -c guix -n "__fish_guix_using_command $guix_commands_with_environment_options" -s p -l profile=      -d "create environment from profile at PATH" -a "(__fish_complete_directories)"
complete -f -c guix -n "__fish_guix_using_command $guix_commands_with_environment_options"      -l check         -d "check if the shell clobbers environment variables"
complete -f -c guix -n "__fish_guix_using_command $guix_commands_with_environment_options"      -l pure          -d "unset existing environment variables"
complete -x -c guix -n "__fish_guix_using_command $guix_commands_with_environment_options" -s E -l preserve=     -d "preserve environment variables that match REGEXP"
complete -f -c guix -n "__fish_guix_using_command $guix_commands_with_environment_options"      -l search-paths  -d "display needed environment variable definitions"
complete -r -c guix -n "__fish_guix_using_command $guix_commands_with_environment_options" -s r -l root=         -d "make FILE a symlink to the result, and register it as a garbage collector root"
complete -f -c guix -n "__fish_guix_using_command $guix_commands_with_environment_options" -s C -l container     -d "run command within an isolated container"
complete -f -c guix -n "__fish_guix_using_command $guix_commands_with_environment_options" -s N -l network       -d "allow containers to access the network"
complete -f -c guix -n "__fish_guix_using_command $guix_commands_with_environment_options" -s P -l link-profile  -d "link environment profile to ~/.guix-profile within an isolated container"
complete -f -c guix -n "__fish_guix_using_command $guix_commands_with_environment_options" -s W -l nesting       -d "make Guix available within the container"
complete -x -c guix -n "__fish_guix_using_command $guix_commands_with_environment_options" -s u -l user=         -d "instead of copying the name and home of the current user into an isolated container, use the name USER with home directory /home/USER" -a "(__fish_complete_users)"
complete -f -c guix -n "__fish_guix_using_command $guix_commands_with_environment_options"      -l no-cwd        -d "do not share current working directory with an isolated container"
complete -f -c guix -n "__fish_guix_using_command $guix_commands_with_environment_options"      -l writable-root -d "make the container's root file system writable"
complete -r -c guix -n "__fish_guix_using_command $guix_commands_with_environment_options"      -l share=        -d "for containers, share writable host file system according to SPEC"
complete -r -c guix -n "__fish_guix_using_command $guix_commands_with_environment_options"      -l expose=       -d "for containers, expose read-only host file system according to SPEC"
complete -r -c guix -n "__fish_guix_using_command $guix_commands_with_environment_options" -s S -l symlink=      -d "for containers, add symlinks to the profile according to SPEC, e.g. \"/usr/bin/env=bin/env\"."
complete -x -c guix -n "__fish_guix_using_command $guix_commands_with_environment_options" -s v -l verbosity=    -d "use the given verbosity LEVEL"
complete -f -c guix -n "__fish_guix_using_command $guix_commands_with_environment_options"      -l bootstrap     -d "use bootstrap binaries to build the environment"


set -l guix_command archive
complete -f -c guix -n "__fish_guix_using_command $guix_command"      -l export        -d "export the specified files/packages to stdout"
complete -f -c guix -n "__fish_guix_using_command $guix_command" -s r -l recursive     -d "combined with '--export', include dependencies"
complete -f -c guix -n "__fish_guix_using_command $guix_command"      -l import        -d "import from the archive passed on stdin"
complete -f -c guix -n "__fish_guix_using_command $guix_command"      -l missing       -d "print the files from stdin that are missing"
complete -x -c guix -n "__fish_guix_using_command $guix_command" -s x -l extract=      -d "extract the archive on stdin to DIR" -a "(__fish_complete_directories)"
complete -f -c guix -n "__fish_guix_using_command $guix_command" -s t -l list          -d "list the files in the archive on stdin"
complete -f -c guix -n "__fish_guix_using_command $guix_command"      -l generate-key  -d "generate a key pair with the given parameters" -a "PARAMETERS"
complete -f -c guix -n "__fish_guix_using_command $guix_command"      -l authorize     -d "authorize imports signed by the public key on stdin"
complete -x -c guix -n "__fish_guix_using_command $guix_command" -s e -l expression=   -d "build the package or derivation EXPR evaluates to"
complete -f -c guix -n "__fish_guix_using_command $guix_command" -s S -l source        -d "build the packages' source derivations"
complete -x -c guix -n "__fish_guix_using_command $guix_command" -s v -l verbosity=    -d "use the given verbosity LEVEL"


set -l guix_command build
complete -x -c guix -n "__fish_guix_using_command $guix_command" -s e -l expression= -d "build the package or derivation EXPR evaluates to"
complete -r -c guix -n "__fish_guix_using_command $guix_command" -s f -l file=       -d "build the package or derivation that the code within FILE evaluates to"
complete -r -c guix -n "__fish_guix_using_command $guix_command" -s m -l manifest=   -d "build the packages that the manifest given in FILE evaluates to"
complete -f -c guix -n "__fish_guix_using_command $guix_command" -s D -l development -d "build the inputs of the following package"
complete -f -c guix -n "__fish_guix_using_command $guix_command" -s P -l dependents  -d "build dependents of the following package, up to depth N" -a "N"
complete -f -c guix -n "__fish_guix_using_command $guix_command" -s S -l source      -d "build the packages' source derivations"
complete -f -c guix -n "__fish_guix_using_command $guix_command"      -l sources     -d "build source derivations" -a "package all transitive"
complete -f -c guix -n "__fish_guix_using_command $guix_command" -s d -l derivations -d "return the derivation paths of the given packages"
complete -f -c guix -n "__fish_guix_using_command $guix_command"      -l check       -d "rebuild items to check for non-determinism issues"
complete -f -c guix -n "__fish_guix_using_command $guix_command"      -l repair      -d "repair the specified items"
complete -r -c guix -n "__fish_guix_using_command $guix_command" -s r -l root=       -d "make FILE a symlink to the result, and register it as a garbage collector root"
complete -x -c guix -n "__fish_guix_using_command $guix_command" -s v -l verbosity=  -d "use the given verbosity LEVEL"
complete -f -c guix -n "__fish_guix_using_command $guix_command" -s q -l quiet       -d "do not show the build log"
complete -f -c guix -n "__fish_guix_using_command $guix_command"      -l log-file    -d "return the log file names for the given derivations"


set -l guix_command challenge
complete -x -c guix -n "__fish_guix_using_command $guix_command"      -l substitute-urls= -d "compare build results with those at URLS" -a "$guix_substitute_urls"
complete -f -c guix -n "__fish_guix_using_command $guix_command" -s v -l verbose          -d "show details about successful comparisons"
complete -x -c guix -n "__fish_guix_using_command $guix_command"      -l diff=            -d "show differences according to MODE"


set -l guix_command container
complete -x -c guix -n "__fish_guix_using_command $guix_command" -d "execute a command inside of an existing container" -a "exec"


set -l guix_command copy
complete -x -c guix -n "__fish_guix_using_command $guix_command"      -l to=        -d "send ITEMS to HOST" -a "(__fish_print_hostnames)"
complete -x -c guix -n "__fish_guix_using_command $guix_command"      -l from=      -d "receive ITEMS from HOST" -a "(__fish_print_hostnames)"
complete -x -c guix -n "__fish_guix_using_command $guix_command" -s v -l verbosity= -d "use the given verbosity LEVEL"


set -l guix_command deploy
complete -x -c guix -n "__fish_guix_using_command $guix_command" -s e -l expression= -d "deploy the list of machines EXPR evaluates to"
complete -f -c guix -n "__fish_guix_using_command $guix_command" -s r -l roll-back   -d "switch to the previous operating system configuration"
complete -f -c guix -n "__fish_guix_using_command $guix_command" -s x -l execute     -d "execute the following command on all the machines"
complete -x -c guix -n "__fish_guix_using_command $guix_command" -s v -l verbosity=  -d "use the given verbosity LEVEL"
# guix deploy --execute --
complete -f -c guix -n "__fish_guix_using_command $guix_command; and __fish_seen_subcommand_from --execute -x; and not __fish_seen_subcommand_from --" -a "--"
complete -x -c guix -n "__fish_guix_using_command $guix_command; and __fish_seen_subcommand_from --execute -x; and __fish_seen_subcommand_from --"     -a "(__fish_guix_complete_subcommand)"


set -l guix_command describe
complete -x -c guix -n "__fish_guix_using_command $guix_command" -s f -l format=      -d "display information in the given FORMAT" -a "$guix_describe_formats"
complete -f -c guix -n "__fish_guix_using_command $guix_command"      -l list-formats -d "display available formats"
complete -x -c guix -n "__fish_guix_using_command $guix_command" -s p -l profile=     -d "display information about PROFILE" -a "(__fish_complete_directories)"


set -l guix_command download
complete -x -c guix -n "__fish_guix_using_command $guix_command" -s f -l format=               -d "write the hash in the given format" -a "base64 nix-base32 base32 base16 hex hexadecimal"
complete -x -c guix -n "__fish_guix_using_command $guix_command" -s H -l hash=                 -d "use the given hash ALGORITHM"
complete -x -c guix -n "__fish_guix_using_command $guix_command"      -l no-check-certificate= -d "do not validate the certificate of HTTPS servers"
complete -r -c guix -n "__fish_guix_using_command $guix_command" -s o -l output=               -d "download to FILE"
complete -f -c guix -n "__fish_guix_using_command $guix_command" -s g -l git                   -d "download the default branch's latest commit of the Git repository at URL"
complete -x -c guix -n "__fish_guix_using_command $guix_command"      -l commit=               -d "download the given commit or tag of the Git repository at URL"
complete -x -c guix -n "__fish_guix_using_command $guix_command"      -l branch=               -d "download the given branch of the Git repository at URL"
complete -f -c guix -n "__fish_guix_using_command $guix_command" -s r -l recursive             -d "download a Git repository recursively"


set -l guix_command edit
complete -x -c guix -n "__fish_guix_using_command $guix_command" -s L -l load-path= -d "prepend DIR to the package module search path" -a "(__fish_complete_directories)"


set -l guix_command environment
complete -r -c guix -n "__fish_guix_using_command $guix_command" -s l -l load-file= -d "create environment for the package that the code within FILE evaluates to"
complete -f -c guix -n "__fish_guix_using_command $guix_command"      -l ad-hoc     -d "include all specified packages in the environment instead of only their inputs"


set -l guix_command gc
complete -x -c guix -n "__fish_guix_using_command $guix_command" -s C -l collect-garbage=   -d "collect at least MIN bytes of garbage"
complete -x -c guix -n "__fish_guix_using_command $guix_command" -s F -l free-space=        -d "attempt to reach FREE available space in the store"
complete -f -c guix -n "__fish_guix_using_command $guix_command" -s d -l delete-generations -d "delete profile generations matching PATTERN" -a "PATTERN"
complete -f -c guix -n "__fish_guix_using_command $guix_command" -s D -l delete             -d "attempt to delete PATHS"
complete -f -c guix -n "__fish_guix_using_command $guix_command"      -l list-roots         -d "list the user's garbage collector roots"
complete -f -c guix -n "__fish_guix_using_command $guix_command"      -l list-busy          -d "list store items used by running processes"
complete -f -c guix -n "__fish_guix_using_command $guix_command"      -l optimize           -d "optimize the store by deduplicating identical files"
complete -f -c guix -n "__fish_guix_using_command $guix_command"      -l list-dead          -d "list dead paths"
complete -f -c guix -n "__fish_guix_using_command $guix_command"      -l list-live          -d "list live paths"
complete -f -c guix -n "__fish_guix_using_command $guix_command"      -l references         -d "list the references of PATHS"
complete -f -c guix -n "__fish_guix_using_command $guix_command" -s R -l requisites         -d "list the requisites of PATHS"
complete -f -c guix -n "__fish_guix_using_command $guix_command"      -l referrers          -d "list the referrers of PATHS"
complete -f -c guix -n "__fish_guix_using_command $guix_command"      -l derivers           -d "list the derivers of PATHS"
complete -f -c guix -n "__fish_guix_using_command $guix_command"      -l verify             -d "verify the integrity of the store" -a "repair contents repair,contents"
complete -f -c guix -n "__fish_guix_using_command $guix_command"      -l list-failures      -d "list cached build failures"
complete -f -c guix -n "__fish_guix_using_command $guix_command"      -l clear-failures     -d "remove PATHS from the set of cached failures"
complete -f -c guix -n "__fish_guix_using_command $guix_command"      -l vacuum-database    -d "repack the sqlite database tracking the store using less space"


set -l guix_command git
complete -x -c guix -n "__fish_guix_using_command $guix_command; and not __fish_seen_subcommand_from authenticate"  -a "authenticate"             -d "verify commit signatures and authorizations"
complete -x -c guix -n "__fish_guix_using_command $guix_command; and __fish_seen_subcommand_from authenticate" -s r -l repository=                -d "open the Git repository at DIRECTORY" -a "(__fish_complete_directories)"
complete -x -c guix -n "__fish_guix_using_command $guix_command; and __fish_seen_subcommand_from authenticate" -s k -l keyring=                   -d "load keyring from REFERENCE, a Git branch"
complete -x -c guix -n "__fish_guix_using_command $guix_command; and __fish_seen_subcommand_from authenticate"      -l end=                       -d "authenticate revisions up to COMMIT"
complete -f -c guix -n "__fish_guix_using_command $guix_command; and __fish_seen_subcommand_from authenticate"      -l stats                      -d "display commit signing statistics upon completion"
complete -x -c guix -n "__fish_guix_using_command $guix_command; and __fish_seen_subcommand_from authenticate"      -l cache-key=                 -d "cache authenticated commits under KEY"
complete -r -c guix -n "__fish_guix_using_command $guix_command; and __fish_seen_subcommand_from authenticate"      -l historical-authorizations= -d "read historical authorizations from FILE"


set -l guix_command graph
complete -x -c guix -n "__fish_guix_using_command $guix_command" -s b -l backend=      -d "produce a graph with the given backend TYPE" -a "$guix_graph_backends"
complete -f -c guix -n "__fish_guix_using_command $guix_command"      -l list-backends -d "list the available graph backends"
complete -x -c guix -n "__fish_guix_using_command $guix_command" -s t -l type=         -d "represent nodes of the given TYPE" -a "$guix_graph_types"
complete -f -c guix -n "__fish_guix_using_command $guix_command"      -l list-types    -d "list the available graph types"
complete -x -c guix -n "__fish_guix_using_command $guix_command" -s M -l max-depth=    -d "limit to nodes within distance DEPTH"
complete -f -c guix -n "__fish_guix_using_command $guix_command"      -l path          -d "display the shortest path between the given nodes"
complete -x -c guix -n "__fish_guix_using_command $guix_command" -s e -l expression=   -d "consider the package EXPR evaluates to"
complete -x -c guix -n "__fish_guix_using_command $guix_command" -s L -l load-path=    -d "prepend DIR to the package module search path" -a "(__fish_complete_directories)"


set -l guix_command hash
complete -f -c guix -n "__fish_guix_using_command $guix_command" -s x -l exclude-vcs  -d "exclude version control directories"
complete -x -c guix -n "__fish_guix_using_command $guix_command" -s H -l hash=        -d "use the given hash ALGORITHM"
complete -x -c guix -n "__fish_guix_using_command $guix_command" -s f -l format=      -d "write the hash in the given format" -a "base64 nix-base32 base32 base16 hex hexadecimal"
complete -x -c guix -n "__fish_guix_using_command $guix_command" -s S -l serializers= -d "compute the hash on FILE according to TYPE serialization"


set -l guix_command home
complete -x -c guix -n "__fish_guix_using_command $guix_command" -s e -l expression=      -d "consider the home-environment EXPR evaluates to instead of reading FILE, when applicable"
complete -f -c guix -n "__fish_guix_using_command $guix_command"      -l allow-downgrades -d "for 'reconfigure', allow downgrades to earlier channel revisions"
complete -f -c guix -n "__fish_guix_using_command $guix_command" -s N -l network          -d "allow containers to access the network"
complete -r -c guix -n "__fish_guix_using_command $guix_command"      -l share=           -d "for containers, share writable host file system according to SPEC"
complete -r -c guix -n "__fish_guix_using_command $guix_command"      -l expose=          -d "for containers, expose read-only host file system according to SPEC"
complete -x -c guix -n "__fish_guix_using_command $guix_command" -s v -l verbosity=       -d "use the given verbosity LEVEL"
complete -x -c guix -n "__fish_guix_using_command $guix_command"      -l graph-backend=   -d "use BACKEND for 'extension-graph' and 'shepherd-graph'" -a "$guix_graph_backends"
complete -f -c guix -n "__fish_guix_using_command $guix_command" -s I -l list-installed   -d "for 'describe' or 'list-generations', list installed packages matching REGEXP" -a "REGEXP"


set -l guix_command import
complete -r -c guix -n "__fish_guix_using_command $guix_command; and not __fish_seen_subcommand_from $guix_importers" -s i -l insert= -d "insert packages into file alphabetically"
complete -x -c guix -n "__fish_guix_using_command $guix_command; and not __fish_seen_subcommand_from $guix_importers" -a "$guix_importers"

set -l guix_importer composer
complete -f -c guix -n "__fish_seen_subcommand_from $guix_command; and __fish_seen_subcommand_from $guix_importer" -s r -l recursive -d "generate package expressions for all Composer packages that are not yet in Guix"

set -l guix_importer cpan
complete -f -c guix -n "__fish_seen_subcommand_from $guix_command; and __fish_seen_subcommand_from $guix_importer" -s r -l recursive -d "import missing packages recursively"

set -l guix_importer cran
complete -x -c guix -n "__fish_seen_subcommand_from $guix_command; and __fish_seen_subcommand_from $guix_importer" -s a -l archive=        -d "specify the archive repository"
complete -f -c guix -n "__fish_seen_subcommand_from $guix_command; and __fish_seen_subcommand_from $guix_importer" -s r -l recursive       -d "import packages recursively"
complete -x -c guix -n "__fish_seen_subcommand_from $guix_command; and __fish_seen_subcommand_from $guix_importer" -s s -l style=          -d "choose output style" -a "specification variable"
complete -x -c guix -n "__fish_seen_subcommand_from $guix_command; and __fish_seen_subcommand_from $guix_importer" -s p -l license-prefix= -d "add custom prefix to licenses"

set -l guix_importer crate
complete -f -c guix -n "__fish_seen_subcommand_from $guix_command; and __fish_seen_subcommand_from $guix_importer"      -l allow-yanked -d "allow importing yanked crates if no alternative satisfying the version requirement is found"
complete -x -c guix -n "__fish_seen_subcommand_from $guix_command; and __fish_seen_subcommand_from $guix_importer" -s f -l lockfile=    -d "import dependencies from FILE, a 'Cargo.lock' file"

set -l guix_importer egg
complete -f -c guix -s r -l recursive -n "__fish_seen_subcommand_from $guix_command; and __fish_seen_subcommand_from $guix_importer" -d "import packages recursively"

set -l guix_importer elm
complete -f -c guix -s r -l recursive -n "__fish_seen_subcommand_from $guix_command; and __fish_seen_subcommand_from $guix_importer" -d "import packages recursively"

set -l guix_importer elpa
complete -x -c guix -n "__fish_seen_subcommand_from $guix_command; and __fish_seen_subcommand_from $guix_importer" -s a -l archive=      -d "specify the archive repository"
complete -f -c guix -n "__fish_seen_subcommand_from $guix_command; and __fish_seen_subcommand_from $guix_importer" -s l -l list-archives -d "list ELPA repositories supported by the importer"
complete -f -c guix -n "__fish_seen_subcommand_from $guix_command; and __fish_seen_subcommand_from $guix_importer" -s r -l recursive     -d "generate package expressions for all Emacs packages that are not yet in Guix"

set -l guix_importer gem
complete -f -c guix -n "__fish_seen_subcommand_from $guix_command; and __fish_seen_subcommand_from $guix_importer" -s r -l recursive     -d "generate package expressions for all Gem packages that are not yet in Guix"

set -l guix_importer gnu
complete -x -c guix -n "__fish_seen_subcommand_from $guix_command; and __fish_seen_subcommand_from $guix_importer"      -l key-download= -d "handle missing OpenPGP keys according to POLICY" -a "auto always never interactive"

set -l guix_importer go
complete -f -c guix -n "__fish_seen_subcommand_from $guix_command; and __fish_seen_subcommand_from $guix_importer" -s r -l recursive    -d "generate package expressions for all Go modules that are not yet in Guix"
complete -x -c guix -n "__fish_seen_subcommand_from $guix_command; and __fish_seen_subcommand_from $guix_importer" -s p -l goproxy=     -d "specify which goproxy server to use"
complete -f -c guix -n "__fish_seen_subcommand_from $guix_command; and __fish_seen_subcommand_from $guix_importer"      -l pin-versions -d "use the exact versions of a module's dependencies"

set -l guix_importer hackage
complete -x -c guix -n "__fish_seen_subcommand_from $guix_command; and __fish_seen_subcommand_from $guix_importer" -s e -l cabal-environment=   -d "specify environment for Cabal evaluation"
complete -f -c guix -n "__fish_seen_subcommand_from $guix_command; and __fish_seen_subcommand_from $guix_importer" -s r -l recursive            -d "import packages recursively"
complete -f -c guix -n "__fish_seen_subcommand_from $guix_command; and __fish_seen_subcommand_from $guix_importer" -s s -l stdin                -d "read from standard input"
complete -f -c guix -n "__fish_seen_subcommand_from $guix_command; and __fish_seen_subcommand_from $guix_importer" -s t -l no-test-dependencies -d "don't include test-only dependencies"

set -l guix_importer hackage
complete -f -c guix -n "__fish_seen_subcommand_from $guix_command; and __fish_seen_subcommand_from $guix_importer" -s r -l recursive -d "import packages recursively"

set -l guix_importer luanti
complete -f -c guix -n "__fish_seen_subcommand_from $guix_command; and __fish_seen_subcommand_from $guix_importer" -s r -l recursive -d "import packages recursively"
complete -x -c guix -n "__fish_seen_subcommand_from $guix_command; and __fish_seen_subcommand_from $guix_importer"      -l sort=     -d "when choosing between multiple implementations, choose the one with the highest value for KEY" -a "score downloads"

set -l guix_importer npm-binary
complete -f -c guix -n "__fish_seen_subcommand_from $guix_command; and __fish_seen_subcommand_from $guix_importer" -s r -l recursive -d "import packages recursively"

set -l guix_importer nuget
complete -x -c guix -n "__fish_seen_subcommand_from $guix_command; and __fish_seen_subcommand_from $guix_importer" -s a -l archive=        -d "specify the archive repository"
complete -f -c guix -n "__fish_seen_subcommand_from $guix_command; and __fish_seen_subcommand_from $guix_importer" -s r -l recursive       -d "import packages recursively"
complete -x -c guix -n "__fish_seen_subcommand_from $guix_command; and __fish_seen_subcommand_from $guix_importer" -s s -l style=          -d "choose output style" -a "specification variable"
complete -x -c guix -n "__fish_seen_subcommand_from $guix_command; and __fish_seen_subcommand_from $guix_importer" -s p -l license-prefix= -d "add custom prefix to licenses"


set -l guix_importer opam
complete -f -c guix -n "__fish_seen_subcommand_from $guix_command; and __fish_seen_subcommand_from $guix_importer" -s r -l recursive -d "import packages recursively"
complete -x -c guix -n "__fish_seen_subcommand_from $guix_command; and __fish_seen_subcommand_from $guix_importer"      -l repo=     -d "import packages from REPOSITORY (name, URL, or file name); can be used more than once"

set -l guix_importer pypi
complete -f -c guix -n "__fish_seen_subcommand_from $guix_command; and __fish_seen_subcommand_from $guix_importer" -s r -l recursive -d "import packages recursively"

set -l guix_importer stackage
complete -x -c guix -n "__fish_seen_subcommand_from $guix_command; and __fish_seen_subcommand_from $guix_importer" -s l -l lts-version=         -d "specify the LTS version to use"
complete -f -c guix -n "__fish_seen_subcommand_from $guix_command; and __fish_seen_subcommand_from $guix_importer" -s r -l recursive            -d "import packages recursively"
complete -f -c guix -n "__fish_seen_subcommand_from $guix_command; and __fish_seen_subcommand_from $guix_importer" -s t -l no-test-dependencies -d "don't include test-only dependencies"


set -l guix_command install
complete -x -c guix -n "__fish_guix_using_command $guix_command" -s p -l profile=   -d "use PROFILE instead of the user's default profile" -a "(__fish_complete_directories)"
complete -x -c guix -n "__fish_guix_using_command $guix_command" -s v -l verbosity= -d "use the given verbosity LEVEL"


set -l guix_command lint
complete -x -c guix -n "__fish_guix_using_command $guix_command" -s c -l checkers=     -d "only run the specified checkers" -a "$guix_lint_checker"
complete -x -c guix -n "__fish_guix_using_command $guix_command" -s x -l exclude=      -d "exclude the specified checkers" -a "$guix_lint_checker"
complete -f -c guix -n "__fish_guix_using_command $guix_command" -s n -l no-network    -d "only run checkers that do not access the network"
complete -x -c guix -n "__fish_guix_using_command $guix_command" -s e -l expression=   -d "consider the package EXPR evaluates to"
complete -x -c guix -n "__fish_guix_using_command $guix_command" -s L -l load-path=    -d "prepend DIR to the package module search path" -a "(__fish_complete_directories)"
complete -f -c guix -n "__fish_guix_using_command $guix_command" -s l -l list-checkers -d "display the list of available lint checkers"


set -l guix_command locate
complete -f -c guix -n "__fish_guix_using_command $guix_command" -s g -l glob      -d "interpret FILE as a glob pattern"
complete -f -c guix -n "__fish_guix_using_command $guix_command"      -l stats     -d "display database statistics"
complete -f -c guix -n "__fish_guix_using_command $guix_command" -s u -l update    -d "force a database update"
complete -f -c guix -n "__fish_guix_using_command $guix_command"      -l clear     -d "clear the database"
complete -r -c guix -n "__fish_guix_using_command $guix_command"      -l database= -d "store the database in FILE"
complete -x -c guix -n "__fish_guix_using_command $guix_command"      -l method=   -d "use METHOD to select packages to index" -a "manifests store"


set -l guix_command pack
complete -r -c guix -n "__fish_guix_using_command $guix_command"      -l file=           -d "build a pack the code within FILE evaluates to"
complete -x -c guix -n "__fish_guix_using_command $guix_command" -s f -l format=         -d "build a pack in the given FORMAT" -a "$guix_pack_formats"
complete -f -c guix -n "__fish_guix_using_command $guix_command"      -l list-formats    -d "list the formats available"
complete -f -c guix -n "__fish_guix_using_command $guix_command" -s R -l relocatable     -d "produce relocatable executables"
complete -x -c guix -n "__fish_guix_using_command $guix_command" -s e -l expression=     -d "consider the package EXPR evaluates to"
complete -x -c guix -n "__fish_guix_using_command $guix_command" -s C -l compression=    -d "compress using TOOL" -a "gzip lzip xz bzip2 zstd none"
complete -r -c guix -n "__fish_guix_using_command $guix_command" -s S -l symlink=        -d "create symlinks to the profile according to SPEC"
complete -r -c guix -n "__fish_guix_using_command $guix_command" -s m -l manifest=       -d "create a pack with the manifest from FILE"
complete -x -c guix -n "__fish_guix_using_command $guix_command"      -l entry-point=    -d "use PROGRAM as the entry point of the pack"
complete -f -c guix -n "__fish_guix_using_command $guix_command"      -l save-provenance -d "save provenance information"
complete -f -c guix -n "__fish_guix_using_command $guix_command"      -l localstatedir   -d "include /var/guix in the resulting pack"
complete -x -c guix -n "__fish_guix_using_command $guix_command"      -l profile-name=   -d "populate /var/guix/profiles/.../NAME"
complete -r -c guix -n "__fish_guix_using_command $guix_command" -s r -l root=           -d "make FILE a symlink to the result, and register it as a garbage collector root"
complete -f -c guix -n "__fish_guix_using_command $guix_command" -s d -l derivation      -d "return the derivation of the pack"
complete -x -c guix -n "__fish_guix_using_command $guix_command" -s v -l verbosity=      -d "use the given verbosity LEVEL"
complete -f -c guix -n "__fish_guix_using_command $guix_command"      -l bootstrap       -d "use the bootstrap binaries to build the pack"


set -l guix_command package
complete -x -c guix -n "__fish_guix_using_command $guix_command" -s i -l install                 -d "install PACKAGEs"
complete -x -c guix -n "__fish_guix_using_command $guix_command" -s e -l install-from-expression -d "install the package EXP evaluates to"
complete -r -c guix -n "__fish_guix_using_command $guix_command" -s f -l install-from-file=      -d "install the package that the code within FILE evaluates to"
complete -x -c guix -n "__fish_guix_using_command $guix_command" -s r -l remove                  -d "remove PACKAGEs"
complete -f -c guix -n "__fish_guix_using_command $guix_command" -s u -l upgrade                 -d "upgrade all the installed packages matching REGEXP" -a "REGEXP"
complete -r -c guix -n "__fish_guix_using_command $guix_command" -s m -l manifest=               -d "create a new profile generation with the manifest from FILE"
complete -f -c guix -n "__fish_guix_using_command $guix_command"      -l do-not-upgrade          -d "do not upgrade any packages matching REGEXP" -a "REGEXP"
complete -f -c guix -n "__fish_guix_using_command $guix_command"      -l roll-back               -d "roll back to the previous generation"
complete -f -c guix -n "__fish_guix_using_command $guix_command"      -l search-paths            -d "display needed environment variable definitions" -a "exact prefix suffix"
complete -f -c guix -n "__fish_guix_using_command $guix_command" -s l -l list-generations        -d "list generations matching PATTERN" -a "PATTERN"
complete -f -c guix -n "__fish_guix_using_command $guix_command" -s d -l delete-generations      -d "delete generations matching PATTERN" -a "PATTERN"
complete -x -c guix -n "__fish_guix_using_command $guix_command" -s S -l switch-generation=      -d "switch to a generation matching PATTERN"
complete -f -c guix -n "__fish_guix_using_command $guix_command"      -l export-manifest         -d "print a manifest for the chosen profile"
complete -f -c guix -n "__fish_guix_using_command $guix_command"      -l export-channels         -d "print channels for the chosen profile"
complete -x -c guix -n "__fish_guix_using_command $guix_command" -s p -l profile=                -d "use PROFILE instead of the user's default profile" -a "(__fish_complete_directories)"
complete -f -c guix -n "__fish_guix_using_command $guix_command"      -l list-profiles           -d "list the user's profiles"
complete -f -c guix -n "__fish_guix_using_command $guix_command"      -l allow-collisions        -d "do not treat collisions in the profile as an error"
complete -f -c guix -n "__fish_guix_using_command $guix_command"      -l bootstrap               -d "use the bootstrap Guile to build the profile"
complete -x -c guix -n "__fish_guix_using_command $guix_command" -s v -l verbosity=              -d "use the given verbosity LEVEL"
complete -x -c guix -n "__fish_guix_using_command $guix_command" -s s -l search=                 -d "search in synopsis and description using REGEXP"
complete -f -c guix -n "__fish_guix_using_command $guix_command" -s I -l list-installed          -d "list installed packages matching REGEXP" -a "REGEXP"
complete -f -c guix -n "__fish_guix_using_command $guix_command" -s A -l list-available          -d "list available packages matching REGEXP" -a "REGEXP"
complete -x -c guix -n "__fish_guix_using_command $guix_command"      -l show=                   -d "show details about PACKAGE"


set -l guix_command processes
complete -x -c guix -n "__fish_guix_using_command $guix_command" -s f -l format=      -d "display results as normalized record sets" -a "$guix_processes_formats"
complete -f -c guix -n "__fish_guix_using_command $guix_command"      -l list-formats -d "display available formats"


set -l guix_command publish
complete -x -c guix -n "__fish_guix_using_command $guix_command" -s p -l port=                   -d "listen on PORT"
complete -x -c guix -n "__fish_guix_using_command $guix_command"      -l listen=                 -d "listen on the network interface for HOST"
complete -x -c guix -n "__fish_guix_using_command $guix_command" -s u -l user=                   -d "change privileges to USER as soon as possible" -a "(__fish_complete_users)"
complete -f -c guix -n "__fish_guix_using_command $guix_command" -s a -l advertise               -d "advertise on the local network"
complete -f -c guix -n "__fish_guix_using_command $guix_command" -s C -l compression             -d "compress archives with METHOD at LEVEL" -a "METHOD:LEVEL"
complete -x -c guix -n "__fish_guix_using_command $guix_command" -s c -l cache=                  -d "cache published items to DIRECTORY" -a "(__fish_complete_directories)"
complete -x -c guix -n "__fish_guix_using_command $guix_command"      -l cache-bypass-threshold= -d "serve store items below SIZE even when not cached"
complete -x -c guix -n "__fish_guix_using_command $guix_command"      -l workers=                -d "use N workers to bake items"
complete -x -c guix -n "__fish_guix_using_command $guix_command"      -l ttl=                    -d "announce narinfos can be cached for TTL seconds"
complete -x -c guix -n "__fish_guix_using_command $guix_command"      -l negative-ttl=           -d "announce missing narinfos can be cached for TTL seconds"
complete -x -c guix -n "__fish_guix_using_command $guix_command"      -l nar-path=               -d "use PATH as the prefix for nar URLs"
complete -r -c guix -n "__fish_guix_using_command $guix_command"      -l public-key=             -d "use FILE as the public key for signatures"
complete -r -c guix -n "__fish_guix_using_command $guix_command"      -l private-key=            -d "use FILE as the private key for signatures"
complete -f -c guix -n "__fish_guix_using_command $guix_command" -s r -l repl                    -d "spawn REPL server on PORT" -a "PORT"


set -l guix_command pull
complete -r -c guix -n "__fish_guix_using_command $guix_command" -s C -l channels=              -d "deploy the channels defined in FILE"
complete -f -c guix -n "__fish_guix_using_command $guix_command" -s q -l no-channel-files       -d "inhibit loading of user and system 'channels.scm'"
complete -x -c guix -n "__fish_guix_using_command $guix_command"      -l url=                   -d "download \"guix\" channel from the Git repository at URL"
complete -x -c guix -n "__fish_guix_using_command $guix_command"      -l commit=                -d "download the specified \"guix\" channel COMMIT"
complete -x -c guix -n "__fish_guix_using_command $guix_command"      -l branch=                -d "download the tip of the specified \"guix\" channel BRANCH"
complete -f -c guix -n "__fish_guix_using_command $guix_command"      -l allow-downgrades       -d "allow downgrades to earlier channel revisions"
complete -f -c guix -n "__fish_guix_using_command $guix_command"      -l disable-authentication -d "disable channel authentication"
complete -f -c guix -n "__fish_guix_using_command $guix_command"      -l no-check-certificate   -d "do not validate the certificate of HTTPS servers"
complete -f -c guix -n "__fish_guix_using_command $guix_command" -s N -l news                   -d "display news compared to the previous generation"
complete -f -c guix -n "__fish_guix_using_command $guix_command" -s l -l list-generations       -d "list generations matching PATTERN" -a "PATTERN"
complete -f -c guix -n "__fish_guix_using_command $guix_command"      -l details                -d "show details when listing generations"
complete -f -c guix -n "__fish_guix_using_command $guix_command"      -l roll-back              -d "roll back to the previous generation"
complete -f -c guix -n "__fish_guix_using_command $guix_command" -s d -l delete-generations     -d "delete generations matching PATTERN" -a "PATTERN"
complete -x -c guix -n "__fish_guix_using_command $guix_command" -s S -l switch-generation=     -d "switch to a generation matching PATTERN"
complete -x -c guix -n "__fish_guix_using_command $guix_command" -s p -l profile=               -d "use PROFILE instead of ~/.config/guix/current" -a "(__fish_complete_directories)"
complete -x -c guix -n "__fish_guix_using_command $guix_command" -s v -l verbosity=             -d "use the given verbosity LEVEL"
complete -f -c guix -n "__fish_guix_using_command $guix_command"      -l bootstrap              -d "use the bootstrap Guile to build the new Guix"


set -l guix_command refresh
complete -x -c guix -s e -l expression=     -n "__fish_guix_using_command $guix_command" -d "consider the package EXPR evaluates to"
complete -f -c guix -s u -l update          -n "__fish_guix_using_command $guix_command" -d "update source files in place"
complete -x -c guix -s s -l select=         -n "__fish_guix_using_command $guix_command" -d "select all the packages in SUBSET" -a "core non-core module:NAME"
complete -r -c guix -s m -l manifest=       -n "__fish_guix_using_command $guix_command" -d "select all the packages from the manifest in FILE"
complete -x -c guix      -l target-version= -n "__fish_guix_using_command $guix_command" -d "update the package or packages to VERSION, VERSION may be partially specified, e.g. as 6 or 6.4 instead of 6.4.3"
complete -x -c guix -s t -l type=           -n "__fish_guix_using_command $guix_command" -d "restrict to updates from the specified updaters" -a "$guix_refresh_updaters"
complete -f -c guix      -l list-updaters   -n "__fish_guix_using_command $guix_command" -d "list available updaters and exit"
complete -f -c guix -s l -l list-dependent  -n "__fish_guix_using_command $guix_command" -d "list top-level dependent packages that would need to be rebuilt as a result of upgrading PACKAGE..."
complete -f -c guix -s r -l recursive       -n "__fish_guix_using_command $guix_command" -d "check the PACKAGE and its inputs for upgrades"
complete -f -c guix -s T -l list-transitive -n "__fish_guix_using_command $guix_command" -d "list all the packages that PACKAGE depends on"
complete -r -c guix      -l keyring=        -n "__fish_guix_using_command $guix_command" -d "use FILE as the keyring of upstream OpenPGP keys"
complete -x -c guix      -l key-server=     -n "__fish_guix_using_command $guix_command" -d "use HOST as the OpenPGP key server"
complete -x -c guix      -l gpg=            -n "__fish_guix_using_command $guix_command" -d "use COMMAND as the GnuPG 2.x command"
complete -x -c guix      -l key-download=   -n "__fish_guix_using_command $guix_command" -d "handle missing OpenPGP keys according to POLICY" -a "auto always never interactive"
complete -x -c guix -s L -l load-path=      -n "__fish_guix_using_command $guix_command" -d "prepend DIR to the package module search path" -a "(__fish_complete_directories)"


set -l guix_command remove
complete -x -c guix -n "__fish_guix_using_command $guix_command" -s p -l profile=   -d "use PROFILE instead of the user's default profile" -a "(__fish_complete_directories)"
complete -x -c guix -n "__fish_guix_using_command $guix_command" -s v -l verbosity= -d "use the given verbosity LEVEL"


set -l guix_command repl
complete -f -c guix -n "__fish_guix_using_command $guix_command"      -l list-types  -d "display REPL types and exit"
complete -x -c guix -n "__fish_guix_using_command $guix_command" -s t -l type=       -d "start a REPL of the given TYPE" -a "$guix_repl_types"
complete -x -c guix -n "__fish_guix_using_command $guix_command"      -l listen=     -d "listen to ENDPOINT instead of standard input"
complete -f -c guix -n "__fish_guix_using_command $guix_command" -s q                -d "inhibit loading of ~/.guile"
complete -f -c guix -n "__fish_guix_using_command $guix_command" -s i -l interactive -d "launch REPL after evaluating FILE"
complete -x -c guix -n "__fish_guix_using_command $guix_command" -s L -l load-path=  -d "prepend DIR to the package module search path" -a "(__fish_complete_directories)"


set -l guix_command search
complete -x -c guix -n "__fish_guix_using_command $guix_command" -s L -l load-path= -d "prepend DIR to the package module search path" -a "(__fish_complete_directories)"


set -l guix_command shell
complete -f -c guix -n "__fish_guix_using_command $guix_command" -s D -l development     -d "include the development inputs of the next package"
complete -r -c guix -n "__fish_guix_using_command $guix_command" -s f -l file=           -d "add to the environment the package FILE evaluates to"
complete -f -c guix -n "__fish_guix_using_command $guix_command" -s q                    -d "inhibit loading of 'guix.scm' and 'manifest.scm'"
complete -f -c guix -n "__fish_guix_using_command $guix_command"      -l rebuild-cache   -d "rebuild cached environment, if any"
complete -f -c guix -n "__fish_guix_using_command $guix_command"      -l export-manifest -d "print a manifest for the given options"
complete -f -c guix -n "__fish_guix_using_command $guix_command" -s F -l emulate-fhs     -d "for containers, emulate the Filesystem Hierarchy Standard (FHS)"
# guix shell --
complete -f -c guix -n "__fish_guix_using_command $guix_command; and not __fish_seen_subcommand_from --" -a "--"
complete -x -c guix -n "__fish_guix_using_command $guix_command; and __fish_seen_subcommand_from --"     -a "(__fish_guix_complete_subcommand)"


set -l guix_command show
complete -x -c guix -n "__fish_guix_using_command $guix_command" -s L -l load-path= -d "prepend DIR to the package module search path" -a "(__fish_complete_directories)"


set -l guix_command size
complete -x -c guix -n "__fish_guix_using_command $guix_command"      -l substitute-urls= -d "fetch substitute from URLS if they are authorized" -a "$guix_substitute_urls"
complete -x -c guix -n "__fish_guix_using_command $guix_command"      -l sort=            -d "sort according to KEY" -a "closure self"
complete -r -c guix -n "__fish_guix_using_command $guix_command" -s m -l map-file=        -d "write to FILE a graphical map of disk usage"
complete -x -c guix -n "__fish_guix_using_command $guix_command" -s L -l load-path=       -d "prepend DIR to the package module search path" -a "(__fish_complete_directories)"


set -l guix_command style
complete -x -c guix -n "__fish_guix_using_command $guix_command" -s S -l styling=              -d "apply RULE, a styling rule" -a "$guix_style_stylings"
complete -f -c guix -n "__fish_guix_using_command $guix_command"      -l list-stylings         -d "display the list of available style rules"
complete -f -c guix -n "__fish_guix_using_command $guix_command" -s n -l dry-run               -d "display files that would be edited but do nothing"
complete -x -c guix -n "__fish_guix_using_command $guix_command" -s L -l load-path=            -d "prepend DIR to the package module search path" -a "(__fish_complete_directories)"
complete -x -c guix -n "__fish_guix_using_command $guix_command" -s e -l expression=           -d "consider the package EXPR evaluates to"
complete -x -c guix -n "__fish_guix_using_command $guix_command"      -l input-simplification= -d "follow POLICY for package input simplification" -a "silent safe always"
complete -f -c guix -n "__fish_guix_using_command $guix_command" -s f -l whole-file            -d "format the entire contents of the given file(s)"
complete -f -c guix -n "__fish_guix_using_command $guix_command" -s A -l alphabetical-sort     -d "place the contents in alphabetical order as well"


set -l guix_command system
complete -f -c guix -n "__fish_guix_using_command $guix_command" -s d -l derivation       -d "return the derivation of the given system"
complete -x -c guix -n "__fish_guix_using_command $guix_command" -s e -l expression=      -d "consider the operating-system EXPR evaluates to instead of reading FILE, when applicable"
complete -f -c guix -n "__fish_guix_using_command $guix_command"      -l allow-downgrades -d "for 'reconfigure', allow downgrades to earlier channel revisions"
complete -x -c guix -n "__fish_guix_using_command $guix_command"      -l on-error=        -d "apply STRATEGY when an error occurs while reading FILE" -a "nothing-special backtrace debug"
complete -f -c guix -n "__fish_guix_using_command $guix_command"      -l list-image-types -d "list available image types"
complete -x -c guix -n "__fish_guix_using_command $guix_command" -s t -l image-type=      -d "for 'image', produce an image of TYPE" -a "$guix_image_types"
complete -x -c guix -n "__fish_guix_using_command $guix_command"      -l image-size=      -d "for 'image', produce an image of SIZE"
complete -f -c guix -n "__fish_guix_using_command $guix_command"      -l no-bootloader    -d "for 'init', do not install a bootloader"
complete -f -c guix -n "__fish_guix_using_command $guix_command"      -l no-kexec         -d "for 'reconfigure', do not load system for kexec reboot"
complete -f -c guix -n "__fish_guix_using_command $guix_command"      -l volatile         -d "for 'image', make the root file system volatile"
complete -f -c guix -n "__fish_guix_using_command $guix_command"      -l persistent       -d "for 'vm', make the root file system persistent"
complete -x -c guix -n "__fish_guix_using_command $guix_command"      -l label=           -d "for 'image', label disk image with LABEL"
complete -f -c guix -n "__fish_guix_using_command $guix_command"      -l save-provenance  -d "save provenance information"
complete -r -c guix -n "__fish_guix_using_command $guix_command"      -l share=           -d "for 'vm' and 'container', share host file system with read/write access according to SPEC"
complete -r -c guix -n "__fish_guix_using_command $guix_command"      -l expose=          -d "for 'vm' and 'container', expose host file system directory as read-only according to SPEC"
complete -f -c guix -n "__fish_guix_using_command $guix_command" -s N -l network          -d "for 'container', allow containers to access the network"
complete -r -c guix -n "__fish_guix_using_command $guix_command" -s r -l root=            -d "for 'vm', 'image', 'container' and 'build', make FILE a symlink to the result, and register it as a garbage collector root"
complete -f -c guix -n "__fish_guix_using_command $guix_command"      -l full-boot        -d "for 'vm', make a full boot sequence"
complete -f -c guix -n "__fish_guix_using_command $guix_command"      -l no-graphic       -d "for 'vm', use the tty that we are started in for IO"
complete -f -c guix -n "__fish_guix_using_command $guix_command"      -l skip-checks      -d "skip file system and initrd module safety checks"
complete -x -c guix -n "__fish_guix_using_command $guix_command" -s v -l verbosity=       -d "use the given verbosity LEVEL"
complete -x -c guix -n "__fish_guix_using_command $guix_command"      -l graph-backend=   -d "use BACKEND for 'extension-graph' and 'shepherd-graph'" -a "$guix_graph_backends"
complete -f -c guix -n "__fish_guix_using_command $guix_command" -s I -l list-installed   -d "for 'describe' and 'list-generations', list installed packages matching REGEXP" -a "REGEXP"


set -l guix_command time-machine
complete -r -c guix -n "__fish_guix_using_command $guix_command" -s C -l channels=              -d "deploy the channels defined in FILE"
complete -f -c guix -n "__fish_guix_using_command $guix_command" -s q -l no-channel-files       -d "inhibit loading of user and system 'channels.scm'"
complete -x -c guix -n "__fish_guix_using_command $guix_command"      -l url=                   -d "use the Git repository at URL"
complete -x -c guix -n "__fish_guix_using_command $guix_command"      -l commit=                -d "use the specified COMMIT"
complete -x -c guix -n "__fish_guix_using_command $guix_command"      -l branch=                -d "use the tip of the specified BRANCH"
complete -f -c guix -n "__fish_guix_using_command $guix_command"      -l disable-authentication -d "disable channel authentication"
complete -f -c guix -n "__fish_guix_using_command $guix_command"      -l no-check-certificate   -d "do not validate the certificate of HTTPS servers"
# guix time-machine --
complete -x -c guix -n "__fish_guix_using_command $guix_command; and not __fish_seen_subcommand_from --" -a "--"
complete -x -c guix -n "__fish_guix_using_command $guix_command; and __fish_seen_subcommand_from --"     -a "(__fish_guix_complete_subcommand)"


set -l guix_command upgrade
complete -x -c guix -n "__fish_guix_using_command $guix_command" -s p -l profile=       -d "use PROFILE instead of the user's default profile" -a "(__fish_complete_directories)"
complete -x -c guix -n "__fish_guix_using_command $guix_command" -s v -l verbosity=     -d "use the given verbosity LEVEL"
complete -f -c guix -n "__fish_guix_using_command $guix_command"      -l do-not-upgrade -d "do not upgrade any packages matching REGEXP" -a "REGEXP"


set -l guix_command weather
complete -x -c guix -n "__fish_guix_using_command $guix_command"      -l substitute-urls= -d "check for available substitutes at URLS" -a "$guix_substitute_urls"
complete -r -c guix -n "__fish_guix_using_command $guix_command" -s m -l manifest=        -d "look up substitutes for packages specified in MANIFEST"
complete -x -c guix -n "__fish_guix_using_command $guix_command" -s e -l expression=      -d "look up substitutes for the package EXPR evaluates to"
complete -f -c guix -n "__fish_guix_using_command $guix_command" -s c -l coverage         -d "show substitute coverage for packages with at least COUNT dependents" -a "COUNT"
complete -f -c guix -n "__fish_guix_using_command $guix_command"      -l display-missing  -d "display the list of missing substitutes"
