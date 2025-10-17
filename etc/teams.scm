#!/bin/sh
# -*- mode: scheme; -*-
# Extra care is taken here to ensure this script can run in most environments,
# since it is invoked by 'git send-email'.
pre_inst_env_maybe=
command -v guix > /dev/null || pre_inst_env_maybe=./pre-inst-env
exec $pre_inst_env_maybe guix repl -- "$0" "$@"
!#

;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2022-2024 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2022 Mathieu Othacehe <othacehe@gnu.org>
;;; Copyright © 2022, 2023, 2025 Maxim Cournoyer <maxim@guixotic.coop>
;;; Copyright © 2022 Simon Tournier <zimon.toutoune@gmail.com>
;;; Copyright © 2025 Jelle Licht <jlicht@fsfe.org>
;;; Copyright © 2025 Cayetano Santos <csantosb@inventati.org>
;;; Copyright © 2025 Ludovic Courtès <ludo@gnu.org>
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This code defines development teams and team members, as well as their
;; scope.

;;; Code:

(define-module (teams)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (srfi srfi-71)
  #:use-module (ice-9 format)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:use-module (guix ui)
  #:use-module (git)
  #:use-module (json)
  #:use-module (web client)
  #:use-module (web request)
  #:use-module (web response)
  #:use-module (rnrs bytevectors)
  #:use-module (guix base64))

(define-record-type <regexp*>
  (%make-regexp* pat flag rx)
  regexp*?
  (pat regexp*-pattern)
  (flag regexp*-flag)
  (rx regexp*-rx))

;;; Work around regexp implementation.
;;; This record allows to track the regexp pattern and then display it.
(define* (make-regexp* pat #:optional (flag regexp/extended))
  "Alternative to `make-regexp' producing annotated <regexp*> objects."
  (%make-regexp* pat flag (make-regexp pat flag)))

(define (regexp*-exec rx* str)
  "Execute the RX* regexp, a <regexp*> object."
  (regexp-exec (regexp*-rx rx*) str))

(define-record-type <team>
  (make-team id name description members scope)
  team?
  (id          team-id)
  (name        team-name)
  (description team-description)
  (members     team-members set-team-members!)
  (scope       team-scope))

(define-record-type <person>
  (make-person name email account)
  person?
  (name    person-name)
  (email   person-email)
  (account person-codeberg-account))

(define* (person name #:optional email account)
  (make-person name email account))

(define* (team id #:key name description (members '())
               (scope '()))
  (make-team id
             (or name (symbol->string id))
             description
             members
             scope))

(define %teams
  (make-hash-table))

(define-syntax define-team
  (lambda (x)
    (syntax-case x ()
      ((_ id value)
       #`(begin
           (define-public id value)
           (hash-set! %teams 'id id))))))

(define-syntax-rule (define-member person teams ...)
  (let ((p person))
    (for-each (lambda (team-id)
                (let ((team
                       (hash-ref %teams team-id
                                 (lambda ()
                                   (error (format #false
                                                  "Unknown team ~a for ~a~%"
                                                  team-id p))))))
                  (set-team-members!
                   team (cons p (team-members team)))))
              (quote (teams ...)))))


;;;
;;; Forgejo support.
;;;

;; Forgejo team.  This corresponds to both the 'Team' and 'CreateTeamOption'
;; structures in Forgejo.
(define-json-mapping <forgejo-team>
  forgejo-team forgejo-team?
  json->forgejo-team <=> forgejo-team->json
  (name                forgejo-team-name)
  (id                  forgejo-team-id)           ;integer
  (description         forgejo-team-description)
  (all-repositories?   forgejo-team-all-repositories?
                       "includes_all_repositories")
  (can-create-org-repository? forgejo-team-can-create-org-repository?
                              "can_create_org_repo")
  (permission          forgejo-team-permission
                       "permission" string->symbol symbol->string)
  ;; A 'units' field exists but is deprecated in favor of 'units_map'.
  (unit-map            forgejo-team-unit-map
                       "units_map" json->unit-map unit-map->json))

(define (unit-map->json lst)
  (map (match-lambda
         ((unit . permission)
          (cons unit (symbol->string permission))))
       lst))

(define (json->unit-map lst)
  (map (match-lambda
         ((unit . permission)
          (cons unit (string->symbol permission))))
       lst))

(define %default-forgejo-team-units
  '("repo.code" "repo.issues" "repo.pulls" "repo.releases"
    "repo.wiki" "repo.ext_wiki" "repo.ext_issues" "repo.projects"
    "repo.packages" "repo.actions"))

(define %default-forgejo-team-unit-map
  ;; Everything (including "repo.code") is read-only by default, except a few
  ;; units.
  (map (match-lambda
         ("repo.pulls" (cons "repo.pulls" 'write))
         ("repo.issues" (cons "repo.issues" 'write))
         ("repo.wiki" (cons "repo.wiki" 'write))
         (unit (cons unit 'read)))
       %default-forgejo-team-units))

;; Forgejo user, as returned by 'forgejo-team-members'.
(define-json-mapping <forgejo-user>
  forgejo-user forgejo-user?
  json->forgejo-user <=> forgejo-user->json
  (id           forgejo-user-id)                  ;integer
  (active?      forgejo-user-active? "active")    ;boolean
  (login        forgejo-user-login)               ;string
  (full-name    forgejo-user-full-name "full_name") ;string
  ;; Various fields omitted.
  )

(define (forgejo-http-headers token)
  "Return the HTTP headers for basic authorization with TOKEN."
  `((content-type . (application/json (charset . "UTF-8")))
    ;; The "Auth Basic" scheme needs a base64-encoded colon-separated user and
    ;; token values. Forgejo doesn't seem to care for the user part but the
    ;; colon seems to be necessary for the token value to get extracted.
    (authorization . (basic . ,(base64-encode
                                (string->utf8
                                 (string-append ":" token)))))))

;; Error with a Forgejo request.
(define-condition-type &forgejo-error &error
  forgejo-error?
  (url       forgejo-error-url)
  (method    forgejo-error-method)
  (response  forgejo-error-response))

(define %codeberg-organization
  ;; Name of the organization at codeberg.org.
  "guix")

(define* (codeberg-url items #:key (parameters '()))
  "Construct a Codeberg API URL with the path components ITEMS and query
PARAMETERS."
  (define query
    (match parameters
      (() "")
      (((keys . values) ...)
       (string-append "?" (string-join
                           (map (lambda (key value)
                                  (string-append key "=" value)) ;XXX: hackish
                                keys values)
                           "&")))))

  (string-append "https://codeberg.org/api/v1/"
                 (string-join items "/")
                 query))

(define-syntax process-url-components
  (syntax-rules (&)
    "Helper macro to construct a Codeberg URL."
    ((_ components ... & parameters)
     (codeberg-url (list components ...)
                   #:parameters parameters))
    ((_ components ...)
     (codeberg-url (list components ...)))))

(define-syntax define-forgejo-request
  (syntax-rules (=>)
    "Define a procedure that performs a Forgejo request."
    ((_ (proc parameters ...)
        docstring
        (verb components ...)
        body
        => code
        deserialize)
     (define (proc token parameters ...)
       docstring
       (let* ((url (process-url-components components ...))
              (response port (http-request url
                                           #:method 'verb
                                           #:streaming? #t
                                           #:headers (forgejo-http-headers token)
                                           #:body body)))
         (if (= code (response-code response))
             (let ((value (deserialize port)))
               (when port (close-port port))
               value)
             (begin
               (when port (close-port port))
               (raise (condition (&forgejo-error (url url)
                                                 (method 'verb)
                                                 (response response)))))))))
    ((_ (proc parameters ...)
        docstring
        (method components ...)
        => code
        deserialize)
     (define-forgejo-request (proc parameters ...)
       docstring
       (method components ...)
       ""
       => code
       deserialize))
    ((_ (proc parameters ...)
        docstring
        (method components ...)
        => code)
     (define-forgejo-request (proc parameters ...)
       docstring
       (method components ...)
       ""
       => code
       (const *unspecified*)))))

;; API documentation at <https://codeberg.org/api/swagger>.

(define-forgejo-request (organization-teams organization)
  "Return the list of teams of ORGANIZATION."
  (GET "orgs" organization "teams"
       & '(("limit" . "100")))                    ;get up to 100 teams
  => 200
  (lambda (port)
    (map json->forgejo-team (vector->list (json->scm port)))))

(define-forgejo-request (create-team organization team)
  "Create TEAM, a Forgejo team, under ORGANIZATION."
  (POST "orgs" organization "teams")
  (forgejo-team->json team)
  => 201
  json->forgejo-team)

(define-forgejo-request (edit-team team)
  "Update TEAM, a Forgejo team."
  (PATCH "teams" (number->string (forgejo-team-id team)))
  (forgejo-team->json team)
  => 200
  json->forgejo-team)

(define-forgejo-request (delete-team team)
  "Delete TEAM, a Forgejo team."
  (DELETE "teams" (number->string (forgejo-team-id team)))
  => 204)

(define-forgejo-request (forgejo-team-members team)
  "Return the list of account names of the members of TEAM, a Forgejo team."
  (GET "teams" (number->string (forgejo-team-id team)) "members"
       & '(("limit" . "100")))                    ;get up to 100 members
  => 200
  (lambda (port)
    (set-port-encoding! port "UTF-8")
    (map json->forgejo-user (vector->list (json->scm port)))))

(define-forgejo-request (add-team-member team user)
  "Add USER (a string) to TEAM, a Forgejo team."
  (PUT "teams" (number->string (forgejo-team-id team))
       "members" user)
  => 204)

(define-forgejo-request (repository-teams owner repository)
  "Return the list of teams assigned to REPOSITORY of OWNER."
  (GET "repos" owner repository "teams"
       & '(("limit" . "100")))                    ;get up to 100 teams
  => 200
  (lambda (port)
    (map json->forgejo-team (vector->list (json->scm port)))))

(define-forgejo-request (add-repository-team owner repository team-name)
  "Add TEAM-NAME as a team of OWNER's REPOSITORY."
  (PUT "repos" owner repository "teams" team-name)
  => 204)

(define (team->forgejo-team team)
  "Return a Forgejo team derived from TEAM, a <team> record."
  (forgejo-team (team-id->forgejo-id (team-id team))
                #f
                (or (team-description team) "")
                #f                                ;all-repositories?
                #f                                ;can-create-org-repository?
                'read                             ;permission
                %default-forgejo-team-unit-map))

(define* (update-team token forgejo-team team
                      #:key (log-port (current-error-port)))
  "Update FORGEJO-TEAM on the server so that it matches TEAM."
  (format log-port "updating team '~a'~%"
          (forgejo-team-name forgejo-team))

  ;; Update metadata: description, permissions, etc.
  (edit-team token forgejo-team)

  ;; Update the list of members so it matches those of TEAM.
  (let* ((current   (map forgejo-user-login
                         (forgejo-team-members token forgejo-team)))
         (target    (filter-map person-codeberg-account
                                (team-members team)))
         (to-add    (lset-difference string=? target current))
         (to-remove (lset-difference string=? current target)))
    (for-each (lambda (user)
                (format log-port "adding '~a' to team '~a'~%"
                        user (forgejo-team-name forgejo-team))
                (add-team-member token forgejo-team user))
              to-add)
    (for-each (lambda (user)
                (format log-port "removing '~a' from team '~a'~%"
                        user (forgejo-team-name forgejo-team)))
              to-remove)))

(define* (synchronize-team token team
                           #:key
                           (current-teams
                            (organization-teams token
                                                %codeberg-organization))
                           (log-port (current-error-port)))
  "Synchronize TEAM, a <team> record, so that its metadata and list of members
are accurate on Codeberg, either by creating it or by updating it if it
already exists.  Lookup team IDs among CURRENT-TEAMS."
  (let ((forgejo-team
         (find (let ((name (team-id->forgejo-id (team-id team))))
                 (lambda (candidate)
                   (string=? (forgejo-team-name candidate) name)))
               current-teams)))
    (if forgejo-team
        (update-team token forgejo-team team
                     #:log-port log-port)
        (let ((forgejo-team
               (create-team token %codeberg-organization
                            (or forgejo-team
                                (team->forgejo-team team)))))
          (format log-port "created team '~a'~%"
                  (forgejo-team-name forgejo-team))
          (let ((members (filter-map person-codeberg-account
                                     (team-members team))))
            (for-each (lambda (member)
                        (add-team-member token forgejo-team member))
                      members)
            (format log-port "added ~a members to team '~a'~%"
                    (length members)
                    (forgejo-team-name forgejo-team))
            forgejo-team)))))

(define (synchronize-teams token)
  "Push all the existing teams on Codeberg."
  (let ((teams (sort-teams
                (hash-map->list (lambda (_ value) value) %teams))))
    (format (current-error-port)
            "creating ~a teams in the '~a' organization at Codeberg...~%"
            (length teams) %codeberg-organization)

    ;; Arrange to compute the list of existing teams once and for all.
    (for-each (let ((teams (organization-teams token
                                               %codeberg-organization)))
                (lambda (team)
                  (synchronize-team token team
                                    #:current-teams teams)))
              teams)

    ;; Ensure all the teams are associated with the main repository.
    (let ((repository-teams (repository-teams token
                                              %codeberg-organization
                                              "guix")))
      (for-each (lambda (missing)
                  (format (current-error-port)
                          "adding team '~a' to '~a/guix'~%"
                          (team-id missing) %codeberg-organization)
                  (add-repository-team token %codeberg-organization "guix"
                                       (team-id->forgejo-id
                                        (team-id missing))))
                (let ((names (map forgejo-team-name repository-teams)))
                  (remove (lambda (team)
                            (member (team-id->forgejo-id (team-id team))
                                    names))
                          teams))))))



(define-team ai
  (team 'ai
        #:name "Artificial Intelligence"
        #:description "Packages related to AI, ML and LLM."
        #:scope (list "gnu/packages/machine-learning.scm")))

(define-team audio
  (team 'audio
        #:name "Audio team"
        #:description "Audio related packages."
        #:scope (list "gnu/packages/audio.scm"
                      "gnu/packages/fluidplug.scm"
                      "gnu/packages/xiph.scm")))

(define-team bootstrap
  (team 'bootstrap
        #:name "Bootstrap"
        #:description "Full-source bootstrap: stage0, Mes, Gash, etc."
        #:scope (list "gnu/packages/commencement.scm"
                      "gnu/packages/mes.scm")))

(define-team c++
  (team 'c++
        #:name "C/C++ team"
        #:description
        "C and C++ libraries and tools."
        #:scope (list "gnu/build-system/cmake.scm"
                      "gnu/build/cmake-build-system.scm"
                      "gnu/packages/c.scm"
                      "gnu/packages/cmake.scm"
                      "gnu/packages/cpp.scm"
                      "gnu/packages/ninja.scm"
                      "gnu/packages/valgrind.scm")))

(define-team core
  (team 'core
        #:name "Core / Tools / Internals"
        #:scope
        (list "guix/avahi.scm"
              "guix/base16.scm"
              "guix/base32.scm"
              "guix/base64.scm"
              "guix/bzr-download.scm"
              "guix/cache.scm"
              "guix/channels.scm"
              "guix/ci.scm"
              "guix/colors.scm"
              "guix/combinators.scm"
              "guix/config.scm"
              "guix/cpio.scm"
              "guix/cpu.scm"
              "guix/cve.scm"
              "guix/cvs-download.scm"
              "guix/deprecation.scm"
              "guix/derivations.scm"
              "guix/describe.scm"
              "guix/diagnostics.scm"
              "guix/discovery.scm"
              "guix/docker.scm"
              "guix/download.scm"
              "guix/elf.scm"
              "guix/ftp-client.scm"
              "guix/gexp.scm"
              "guix/git-authenticate.scm"
              "guix/git-download.scm"
              "guix/git.scm"
              "guix/glob.scm"
              "guix/gnu-maintenance.scm"
              "guix/gnupg.scm"
              "guix/grafts.scm"
              "guix/graph.scm"
              "guix/hash.scm"
              "guix/hg-download.scm"
              "guix/http-client.scm"
              "guix/i18n.scm"
              "guix/inferior.scm"
              "guix/ipfs.scm"
              "guix/least-authority.scm"
              "guix/licenses.scm"
              "guix/lint.scm"
              "guix/man-db.scm"
              "guix/memoization.scm"
              "guix/modules.scm"
              "guix/monad-repl.scm"
              "guix/monads.scm"
              "guix/narinfo.scm"
              "guix/nar.scm"
              "guix/openpgp.scm"
              "guix/packages.scm"
              "guix/pki.scm"
              "guix/platform.scm"
              "guix/profiles.scm"
              "guix/profiling.scm"
              "guix/progress.scm"
              "guix/quirks.scm"
              "guix/read-print.scm"
              "guix/records.scm"
              "guix/remote.scm"
              "guix/repl.scm"
              "guix/search-paths.scm"
              "guix/self.scm"
              "guix/serialization.scm"
              "guix/sets.scm"
              "guix/ssh.scm"
              "guix/status.scm"
              "guix/store.scm"
              "guix/substitutes.scm"
              "guix/svn-download.scm"
              "guix/swh.scm"
              "guix/tests.scm"
              "guix/transformations.scm"
              "guix/ui.scm"
              "guix/upstream.scm"
              "guix/utils.scm"
              "guix/workers.scm"
              (make-regexp* "^guix/platforms/")
              (make-regexp* "^guix/scripts/")
              (make-regexp* "^guix/store/")
              (make-regexp* "^nix/"))))

(define-team core-packages
  (team 'core-packages
        #:name "Core packages"
        #:description "Core packages: the GNU tool chain, Guile, Coreutils, etc."
        #:scope (list "gnu/packages/base.scm"
                      "gnu/packages/bootstrap.scm"
                      "gnu/packages/commencement.scm"
                      "gnu/packages/cross-base.scm"
                      "gnu/packages/gcc.scm"
                      "gnu/packages/guile.scm"
                      "gnu/packages/ld-wrapper.in"
                      "gnu/packages/make-bootstrap.scm"
                      "gnu/packages/multiprecision.scm"
                      "guix/build/gnu-build-system.scm"
                      "guix/build/utils.scm"
                      "guix/build-system/gnu.scm")))

(define-team debian
  (team 'debian
        #:name "Debian team"
        #:description
        "Packages related to Debian and Debian derivatives."
        #:scope (list "gnu/packages/debian.scm")))

(define-team documentation
  (team 'documentation
        #:name "Documentation"
        #:description "Documentation: the manual and cookbook."
        #:scope (list (make-regexp* "\\.texi$")
                      "doc/build.scm"
                      "gnu/system/examples/bare-bones.tmpl"
                      "gnu/system/examples/lightweight-desktop.tmpl"
                      "gnu/system/examples/desktop.tmpl")))

(define-team electronics
  (team 'electronics
        #:name "Electronics team"
        #:description "Electronics and hardware related packages."
        #:scope (list "gnu/packages/electronics.scm"
                      "gnu/packages/libftdi.scm"
                      "gnu/packages/engineering.scm"
                      "gnu/packages/flashing-tools.scm")))

(define-team emacs
  (team 'emacs
        #:name "Emacs team"
        #:description "The extensible, customizable text editor and its
ecosystem."
        #:scope (list "gnu/packages/aux-files/emacs/guix-emacs.el"
                      "gnu/packages/aux-files/emacs/comp-integrity.el"
                      (make-regexp* "^gnu/packages/emacs(-.+|)\\.scm$")
                      "gnu/packages/tree-sitter.scm"
                      "guix/build/emacs-build-system.scm"
                      "guix/build/emacs-utils.scm"
                      "guix/build-system/emacs.scm"
                      "guix/import/elpa.scm"
                      "guix/scripts/import/elpa.scm"
                      "tests/import/elpa.scm")))

(define-team embedded
  (team 'embedded
        #:name "Embedded"
        #:scope (list "gnu/packages/bootloaders.scm"
                      "gnu/packages/firmware.scm")))

(define-team games
  (team 'games
        #:name "Games and Toys"
        #:description "Packaging programs for amusement."
        #:scope (list "gnu/packages/emulators.scm"
                      "gnu/packages/games.scm"
                      "gnu/packages/game-development.scm"
                      "gnu/packages/luanti.scm"
                      "gnu/packages/esolangs.scm" ; granted, rather niche
                      "gnu/packages/motti.scm"
                      "guix/build/luanti-build-system.scm")))

(define-team gnome
  (team 'gnome
        #:name "Gnome team"
        #:description
        "The Gnome desktop environment, along with core technologies such as
GLib/GIO, GTK, GStreamer and Webkit."
        #:scope (list (make-regexp* "etc/teams/gnome")
                      "gnu/packages/glib.scm"
                      "gnu/packages/gstreamer.scm"
                      "gnu/packages/gtk.scm"
                      "gnu/packages/gnome.scm"
                      "gnu/packages/gnome-xyz.scm"
                      "gnu/packages/webkit.scm"
                      "gnu/services/desktop.scm"
                      "guix/build/glib-or-gtk-build-system.scm"
                      "guix/build/meson-build-system.scm")))

(define-team go
  (team 'go
        #:name "Go team"
        #:scope (list "gnu/packages/configuration-management.scm"
                      (make-regexp* "gnu/packages/golang(-.+|)\\.scm$")
                      "gnu/packages/syncthing.scm"
                      "gnu/packages/terraform.scm"
                      "guix/build-system/go.scm"
                      "guix/build/go-build-system.scm"
                      "guix/import/go.scm"
                      "guix/scripts/import/go.scm"
                      "tests/import/go.scm")))

(define-team haskell
  (team 'haskell
        #:name "Haskell team"
        #:description
        "GHC, Hugs, Haskell packages, the \"hackage\" and \"stackage\" importers, and
the haskell-build-system."
        #:scope
        (list "gnu/packages/dhall.scm"
              ;; Match haskell.scm and haskell-*.scm.
              (make-regexp* "^gnu/packages/haskell(-.+|)\\.scm$")
              "gnu/packages/purescript.scm"
              "guix/build/haskell-build-system.scm"
              "guix/build-system/haskell.scm"
              "guix/import/cabal.scm"
              "guix/import/hackage.scm"
              "guix/import/stackage.scm"
              "guix/scripts/import/hackage.scm")))

(define-team home
  (team 'home
        #:name "Team for \"Guix Home\""
        #:scope (list (make-regexp* "^(gnu|guix/scripts)/home(\\.scm$|/)")
                      "tests/guix-home.sh"
                      "tests/home-import.scm"
                      "tests/home-services.scm")))

(define-team hpc
  (team 'hpc
        #:name "Hpc team"
        #:description "High performance computing related packages."
        #:scope (list "gnu/packages/mpi.scm"
                      "gnu/packages/rocm.scm"
                      "gnu/packages/sycl.scm"
                      "gnu/packages/tbb.scm"
                      "gnu/packages/vulkan.scm")))

(define-team hurd
  (team 'hurd
        #:name "Team for the Hurd"
        #:description "GNU Hurd packages and operating system support."
        #:scope (list "gnu/system/hurd.scm"
                      "gnu/system/images/hurd.scm"
                      "gnu/build/hurd-boot.scm"
                      "gnu/services/hurd.scm"
                      "gnu/packages/hurd.scm")))

(define-team installer
  (team 'installer
        #:name "Installer script and system installer"
        #:scope (list (make-regexp* "^gnu/installer(\\.scm$|/)"))))

(define-team java
  (team 'java
        #:name "Java and Maven team"
        #:description
        "The JDK and JRE, the Maven build system, Java packages, the ant-build-system,
and the maven-build-system."
        #:scope
        (list ;; Match java.scm and java-*.scm.
              (make-regexp* "^gnu/packages/java(-.+|)\\.scm$")
              ;; Match maven.scm and maven-*.scm
              (make-regexp* "^gnu/packages/maven(-.+|)\\.scm$")
              "guix/build/ant-build-system.scm"
              "guix/build/java-utils.scm"
              "guix/build/maven-build-system.scm"
              ;; The maven directory
              (make-regexp* "^guix/build/maven/")
              "guix/build-system/ant.scm"
              "guix/build-system/maven.scm")))

(define-team javascript
  (team 'javascript
        #:name "JavaScript team"
        #:description
        "JavaScript/Node.js packages, the node build system."
        #:scope (list "gnu/packages/node-xyz.scm"
                      "gnu/packages/node.scm"
                      "guix/build-system/node.scm"
                      "guix/build/node-build-system.scm"
                      "guix/import/npm-binary.scm"
                      "guix/scripts/import/npm-binary.scm")))

(define-team julia
  (team 'julia
        #:name "Julia team"
        #:description
        "The Julia language, Julia packages, and the julia-build-system."
        #:scope (list (make-regexp* "^gnu/packages/julia(-.+|)\\.scm$")
                      "guix/build/julia-build-system.scm"
                      "guix/build-system/julia.scm")))

(define-team kde
  (team 'kde
        #:name "KDE team"
        #:description
        "The plasma desktop environment, and KDE Applications."
        #:scope (list (make-regexp* "^gnu/packages/(kde)(-.+|)\\.scm$"))))

(define-team kernel
  (team 'kernel
        #:name "Linux-libre kernel team"
        #:scope (list "gnu/build/linux-modules.scm"
                      (make-regexp* "^gnu/packages/aux-files/linux-libre.*$")
                      "gnu/packages/linux.scm"
                      "gnu/tests/linux-modules.scm"
                      "guix/build/linux-module-build-system.scm"
                      "guix/build-system/linux-module.scm")))

(define-team lisp
  (team 'lisp
        #:name "Lisp team"
        #:description
        "Common Lisp and similar languages, Common Lisp packages and the
asdf-build-system."
        #:scope (list (make-regexp* "^gnu/packages/lisp(-.+|)\\.scm$")
                      "guix/build/asdf-build-system.scm"
                      "guix/build/lisp-utils.scm"
                      "guix/build-system/asdf.scm")))

(define-team localization
  (team 'localization
        #:name "Localization (l10n) team"
        #:description
        "Localization of your system to specific languages."
        #:scope (list "gnu/packages/anthy.scm"
                      "gnu/packages/fcitx5.scm"
                      "gnu/packages/fonts.scm"
                      "gnu/packages/ibus.scm")))

(define-team lxqt
  (team 'lxqt
        #:name "LXQt team"
        #:description "LXQt desktop environment."
        #:scope (list "gnu/packages/lxqt.scm")))

(define-team mentors
  (team 'mentors
        #:name "Mentors"
        #:description
        "A group of mentors who chaperone contributions by newcomers."))

(define-team mozilla
  (team 'mozilla
        #:name "Mozilla"
        #:description
        "Taking care of Icedove and Web Browsers based on Mozilla Thunderbird
and Firefox."
        #:scope (list "gnu/build/icecat-extension.scm"
                      "gnu/packages/browser-extensions.scm"
                      "gnu/packages/gnuzilla.scm"
                      "gnu/packages/librewolf.scm"
                      "gnu/packages/tor-browsers.scm")))

(define-team ocaml
  (team 'ocaml
        #:name "OCaml and Dune team"
        #:description
        "The OCaml language, the Dune build system, OCaml packages, the \"opam\"
importer, and the ocaml-build-system."
        #:scope
        (list "gnu/packages/ocaml.scm"
              "gnu/packages/coq.scm"
              "guix/build/ocaml-build-system.scm"
              "guix/build/dune-build-system.scm"
              "guix/build-system/ocaml.scm"
              "guix/build-system/dune.scm"
              "guix/import/opam.scm"
              "guix/scripts/import/opam.scm"
              "tests/import/opam.scm")))

(define-team python
  (team 'python
        #:name "Python team"
        #:description
        "Python, Python packages, the \"pypi\" importer, and the python-build-system."
        #:scope
        (list "gnu/packages/django.scm"
              "gnu/packages/jupyter.scm"
              (make-regexp* "^gnu/packages/python(-.+|)\\.scm$")
              "gnu/packages/sphinx.scm"
              "gnu/packages/tryton.scm"
              "guix/build/pyproject-build-system.scm"
              "guix/build-system/pyproject.scm"
              "guix/build/python-build-system.scm"
              "guix/build-system/python.scm"
              "guix/import/pypi.scm"
              "guix/scripts/import/pypi.scm"
              "tests/import/pypi.scm")))

(define-team qt
  (team 'qt
        #:name "Qt team"
        #:description
        "The Qt toolkit/library and the qt-build-system,
as well as some packages using Qt."
        #:scope (list "gnu/packages/qt.scm"
                      "guix/build-system/qt.scm"
                      "guix/build/qt-build-system.scm"
                      "guix/build/qt-utils.scm")))

(define-team r
  (team 'r
        #:name "R team"
        #:description
        "The R language, CRAN and Bioconductor repositories, the \"cran\" importer,
and the r-build-system."
        #:scope (list "gnu/packages/bioconductor.scm"
                      "gnu/packages/cran.scm"
                      "guix/build/r-build-system.scm"
                      "guix/build-system/r.scm"
                      "guix/import/cran.scm"
                      "guix/scripts/import/cran.scm"
                      "tests/import/cran.scm")))

(define-team racket
  (team 'racket
        #:name "Racket team"
        #:description
        "The Racket language and Racket-based languages, Racket packages,
Racket's variant of Chez Scheme, and development of a Racket build system and
importer."
        #:scope (list "gnu/packages/chez.scm"
                      "gnu/packages/racket.scm")))

(define-team release
  (team 'release
        #:name "Release team"
        #:description
        "The current release team.  Members are expected to change with each
release."
        #:scope (list "NEWS"
                      "etc/manifests/release.scm")))

(define-team reproduciblebuilds
  (team 'reproduciblebuilds
        #:name "Reproducible Builds team"
        #:description
        "Reproducible Builds tooling and issues that affect any guix packages."
        #:scope (list "gnu/packages/diffoscope.scm")))

(define-team ruby
  (team 'ruby
        #:name "Ruby team"
        #:scope (list "gnu/packages/ruby.scm"
                      "guix/build/ruby-build-system.scm"
                      "guix/build-system/ruby.scm"
                      "guix/import/gem.scm"
                      "guix/scripts/import/gem.scm"
                      "tests/import/gem.scm")))

(define-team rust
  (team 'rust
        #:name "Rust"
        #:scope (list (make-regexp* "^gnu/packages/(crates|rust)(-.+|)\\.scm$")
                      "gnu/packages/sequoia.scm"
                      "guix/build/cargo-build-system.scm"
                      "guix/build/cargo-utils.scm"
                      "guix/build-system/cargo.scm"
                      "guix/import/crate.scm"
                      "guix/import/crate/cargo-lock.scm"
                      "guix/scripts/import/crate.scm"
                      "tests/import/crate.scm")))

(define-team science
  (team 'science
        #:name "Science team"
        #:description "The main science disciplines and fields related
packages (e.g. Astronomy, Chemistry, Math, Physics etc.)"
        #:scope (list (make-regexp* "^gnu/packages/fortran(-.+|)\\.scm$")
                      "gnu/packages/algebra.scm"
                      "gnu/packages/astronomy.scm"
                      "gnu/packages/chemistry.scm"
                      "gnu/packages/geo.scm"
                      "gnu/packages/graph.scm"
                      "gnu/packages/lean.scm"
                      "gnu/packages/maths.scm"
                      "gnu/packages/medical.scm"
                      "gnu/packages/sagemath.scm"
                      "gnu/packages/statistics.scm")))

(define-team sugar
  (team 'sugar
        #:name "Sugar team"
        #:description
        "Everything related to the Sugar Desktop and learning environment."
        #:scope (list "gnu/packages/sugar.scm")))

(define-team sysadmin
  (team 'sysadmin
        #:name "Sysadmin team"
        #:description
        "Networking, server clustering, high availability."
        #:scope (list "gnu/packages/admin.scm"
                      "gnu/packages/acl.scm"
                      "gnu/packages/adns.scm"
                      "gnu/packages/antivirus.scm"
                      "gnu/packages/apparmor.scm"
                      "gnu/packages/authentication.scm"
                      "gnu/packages/cluster.scm"
                      "gnu/packages/configuration-management"
                      "gnu/packages/databases.scm"
                      "gnu/packages/distributed.scm"
                      "gnu/packages/dns.scm"
                      "gnu/packages/high-availability.scm"
                      "gnu/packages/kerberos.scm"
                      "gnu/packages/logging.scm"
                      "gnu/packages/monitoring.scm"
                      "gnu/packages/nfs.scm"
                      "gnu/packages/openldap.scm"
                      "gnu/packages/openstack.scm"
                      "gnu/packages/prometheus.scm"
                      "gnu/packages/rdesktop.scm"
                      "gnu/packages/selinux.scm"
                      "gnu/packages/storage.scm"
                      "gnu/packages/task-runners.scm"
                      "gnu/packages/terraform.scm"
                      "gnu/packages/virtualization.scm"
                      "gnu/packages/vnc.scm")))

(define-team telephony
  (team 'telephony
        #:name "Telephony team"
        #:description
        "Telephony packages and services such as Jami, Linphone, etc."
        #:scope (list "gnu/build/jami-service.scm"
                      "gnu/packages/jami.scm"
                      "gnu/packages/linphone.scm"
                      "gnu/packages/telephony.scm"
                      "gnu/services/telephony.scm"
                      "gnu/tests/data/jami-dummy-account.dat"
                      "gnu/tests/telephony.scm"
                      "tests/services/telephony.scm")))

(define-team tex
  (team 'tex
        #:name "TeX team"
        #:description
        "TeX, LaTeX, XeLaTeX, LuaTeX, TeXLive, the texlive-build-system, and
the \"texlive\" importer."
        #:scope (list "gnu/packages/tex.scm"
                      "gnu/packages/texlive.scm"
                      "guix/build/texlive-build-system.scm"
                      "guix/build-system/texlive.scm"
                      "guix/import/texlive.scm"
                      "guix/scripts/import/texlive.scm"
                      "tests/import/texlive.scm")))

(define-team translations
  (team 'translations
        #:name "Translations"
        #:scope (list "etc/news.scm"
                      (make-regexp* "^po/"))))

(define-team xfce
  (team 'xfce
        #:name "Xfce team"
        #:description "Xfce desktop environment."
        #:scope (list "gnu/packages/xfce.scm")))

(define-team zig
  (team 'zig
        #:name "Zig team"
        #:description "Zig, Zig packages, and the zig-build system"
        #:scope (list "gnu/packages/zig.scm"
                      "gnu/packages/zig-xyz.scm"
                      "guix/build/zig-build-system.scm"
                      "guix/build-system/zig.scm")))


(define-member (person "Eric Bavier"
                       "bavier@posteo.net"
                       "bavier")
  science)

(define-member (person "Lars-Dominik Braun"
                       "lars@6xq.net"
                       "ldb")
  python haskell)

(define-member (person "Jonathan Brielmaier"
                       "jonathan.brielmaier@web.de"
                       "jonsger")
  mozilla)

(define-member (person "Ludovic Courtès"
                       "ludo@gnu.org"
                       "civodul")
  core core-packages hpc installer
  mentors)

(define-member (person "Andreas Enge"
                       "andreas@enge.fr"
                       "enge")
  bootstrap core-packages lxqt science tex)

(define-member (person "Tanguy Le Carrour"
                       "tanguy@bioneland.org"
                       "tanguybl")
  python home)

(define-member (person "Tobias Geerinckx-Rice"
                       "me@tobias.gr"
                       "nckx")
  mentors)

(define-member (person "Steve George"
                       "steve@futurile.net"
                       "futurile")
  rust)

(define-member (person "Leo Famulari"
                       "leo@famulari.name"
                       "lfam")
  kernel)

(define-member (person "Efraim Flashner"
                       "efraim@flashner.co.il"
                       "efraim")
  embedded bootstrap release rust)

(define-member (person "jgart"
                       "jgart@dismail.de"
                       "jgart")
  lisp mentors)

(define-member (person "Guillaume Le Vaillant"
                       "glv@posteo.net"
                       "glv")
  lisp)

(define-member (person "Julien Lepiller"
                       "julien@lepiller.eu"
                       "roptat")
  java ocaml translations)

(define-member (person "Philip McGrath"
                       "philip@philipmcgrath.com"
                       "LiberalArtist")
  racket)

(define-member (person "Mathieu Othacehe"
                       "othacehe@gnu.org"
                       "mothacehe")
  installer mentors)

(define-member (person "Florian Pelz"
                       "pelzflorian@pelzflorian.de"
                       "pelzflorian")
  translations)

(define-member (person "Liliana Marie Prikler"
                       "liliana.prikler@gmail.com"
                       "lilyp")
  emacs games gnome)

(define-member (person "Ricardo Wurmus"
                       "rekado@elephly.net"
                       "rekado")
  r sugar)

(define-member (person "Christopher Baines"
                       "guix@cbaines.net"
                       "cbaines")
  core mentors ruby)

(define-member (person "Andrew Tropin"
                       "andrew@trop.in"
                       "abcdw")
  home emacs)

(define-member (person "pukkamustard"
                       "pukkamustard@posteo.net"
                       "pukkamustard")
  ocaml)

(define-member (person "Josselin Poiret"
                       "dev@jpoiret.xyz")
  installer)

(define-member (person "("
                       "paren@disroot.org")
  )

(define-member (person "Simon Tournier"
                       "zimon.toutoune@gmail.com")
  julia core mentors r)

(define-member (person "宋文武"
                       "iyzsong@envs.net"
                       "iyzsong")
  games localization lxqt qt xfce)

(define-member (person "Vagrant Cascadian"
                       "vagrant@debian.org"
                       "vagrantc")
  debian embedded)

(define-member (person "Vagrant Cascadian"        ;XXX: duplicate
                       "vagrant@reproducible-builds.org"
                       "vagrantc")
  reproduciblebuilds)

(define-member (person "Maxim Cournoyer"
                       "maxim@guixotic.coop"
                       "apteryx")
  core documentation electronics gnome qt telephony)

(define-member (person "Katherine Cox-Buday"
                       "cox.katherine.e+guix@gmail.com")
  emacs go lisp)

(define-member (person "Munyoki Kilyungi"
                       "me@bonfacemunyoki.com"
                       "BonfaceKilz")
  python lisp)

(define-member (person "Gabriel Wicki"
                       "gabriel@erlikon.ch"
                       "gabber")
  audio documentation electronics embedded)

(define-member (person "Ekaitz Zarraga"
                       "ekaitz@elenq.tech"
                       "ekaitz-zarraga")
  bootstrap zig electronics)

(define-member (person "Divya Ranjan Pattanaik"
                       "divya@subvertising.org"
                       "divyaranjan")
  emacs rust haskell)

(define-member (person "Clément Lassieur"
                       "clement@lassieur.org"
                       "snape")
  mozilla)

(define-member (person "Sharlatan Hellseher"
                       "sharlatanus@gmail.com"
                       "Hellseher")
  go julia python science sysadmin)

(define-member (person "Vivien Kraus"
                       "vivien@planete-kraus.eu")
  gnome)

(define-member (person "Mark H Weaver"
                       "mhw@netris.org"
                       "mhw")
  mozilla)

(define-member (person "Adam Faiz"
                       "adam.faiz@disroot.org")
  games)

(define-member (person "Laurent Gatto"
                       "laurent.gatto@gmail.com")
  r)

(define-member (person "Nicolas Goaziou"
                       "guix@nicolasgoaziou.fr"
                       "ngz")
  tex)

(define-member (person "André Batista"
                       "nandre@riseup.net"
                       "madage")
  mozilla)

(define-member (person "Janneke Nieuwenhuizen"
                       "janneke@gnu.org"
                       "janneke")
  bootstrap core-packages home hurd installer)

(define-member (person "Ian Eure"
                       "ian@retrospec.tv"
                       "ieure")
  mozilla emacs)

(define-member (person "Zheng Junjie"
                       "z572@z572.online"
                       "Z572")
  core-packages qt kde)

(define-member (person "Sughosha"
                       "sughosha@disroot.org"
                       "SameExpert")
  audio kde)

(define-member (person "Jelle Licht"
                       "jlicht@fsfe.org"
                       "jlicht")
  javascript)

(define-member (person "Cayetano Santos"
                       "csantosb@inventati.org"
                       "csantosb")
  ai emacs electronics hpc)

(define-member (person "Greg Hogan"
                       "code@greghogan.com"
                       "greghogan")
  c++)

(define-member (person "Hilton Chain"
                       "hako@ultrarare.space"
                       "hako")
  emacs home localization mozilla rust zig)

(define-member (person "Noé Lopez"
                       "noelopez@free.fr"
                       "Baleine")
  gnome release)

(define-member (person "Ashvith Shetty"
                       "ashvithshetty0010@zohomail.in"
                       "Ashvith")
  games go javascript sysadmin xfce)

(define-member (person "Trevor Richards"
                       "trev@trevdev.ca")
  lisp emacs)

(define-member (person "Konrad Hinsen"
                       "guix@khinsen.fastmail.net"
                       "khinsen")
  lisp)

(define-member (person "Vinicius Monego"
                       "monego@posteo.net"
                       "monego")
  python science)

(define-member (person "Nicolas Graves"
                       "ngraves@ngraves.fr"
                       "nicolas-graves")
  javascript python ruby)

(define-member (person "Yelninei"
                       "yelninei@tutamail.com"
                       "Yelninei")
  hurd)

(define-member (person "Giacomo Leidi"
                       "goodoldpaul@autistici.org"
                       "fishinthecalculator")
  audio)

(define-member (person "Saku Laesvuori"
                       "saku@laesvuori.fi"
                       "slaesvuo")
  haskell)

(define-member (person "Rodion Goritskov"
                       "rodion@goritskov.com"
                       "rodion-goritskov")
  release)

(define-member (person "Rutherther"
                       "rutherther@ditigal.xyz"
                       "Rutherther")
  core release)

(define-member (person "Morgan Arnold"
                       "morgan.arnold@proton.me"
                       "mra")
  core)

(define-member (person "Danny Milosavljevic"
                       "dannym@friendly-machines.com"
                       "daym")
  bootstrap hpc rust science)


(define (find-team name)
  (or (hash-ref %teams (string->symbol name))
      (error (format #false
                     "no such team: ~a~%" name))))

(define (find-team-by-scope files)
  "Return the team(s) which scope matches at least one of the FILES, as list
of file names as string."
  (hash-fold
   (lambda (key team acc)
     (if (any (lambda (file)
                (any (match-lambda
                       ((? string? scope)
                        (string=? scope file))
                       ((? regexp*? scope)
                        (regexp*-exec scope file)))
                     (team-scope team)))
              files)
         (cons team acc)
         acc))
   '()
   %teams))

(define (cc . teams)
  "Return arguments for `git send-email' to notify the members of the given
TEAMS when a patch is received by Debbugs."
  (let ((members (append-map team-members teams)))
    (unless (null? members)
      (format #true "--add-header=\"X-Debbugs-Cc: ~{~a~^, ~}\""
              (map person-email (sort-members members))))))

(define (sort-members members)
  "Deduplicate and sort MEMBERS alphabetically by their name."
  (sort (delete-duplicates members equal?)
        (lambda (m1 m2)
          (string<? (person-name m1) (person-name m2)))))

(define (member->string member)
  "Return the 'email <name>' string representation of MEMBER."
  (let* ((name (person-name member))
         (quoted-name/maybe (if (string-contains name ",")
                                (string-append "\"" name "\"")
                                name)))
    (format #false "~a <~a>" quoted-name/maybe (person-email member))))

(define* (list-members team #:key (prefix ""))
  "Print the members of the given TEAM."
  (for-each (lambda (member)
              (format #t "~a~a~%" prefix (member->string member)))
            (sort-members (team-members team))))

(define (print-team team)
  "Print TEAM, a <team> record object."
  (format #t
          "\
id: ~a
name: ~a
description: ~a
~amembers:
"
          (team-id team)
          (team-name team)
          (or (and=> (team-description team)
                     (lambda (text)
                       (string->recutils
                        (fill-paragraph text (%text-width)
                                        (string-length "description: ")))))
              "<none>")
          (match (team-scope team)
            (() "")
            (scope (format #f "scope:~%~{+ ~a~^~%~}~%"
                           (sort (map (match-lambda
                                        ((? regexp*? rx)
                                         (regexp*-pattern rx))
                                        (item item))
                                      scope)
                                 string<?)))))
  (list-members team #:prefix "+ ")
  (newline))

(define (sort-teams teams)
  "Sort TEAMS, a list of <team> record objects."
  (sort teams
        (lambda (team1 team2)
          (string<? (symbol->string (team-id team1))
                    (symbol->string (team-id team2))))))

(define* (list-teams #:optional team-names)
  "Print all teams, their scope and their members."
  (for-each print-team
            (sort-teams
             (if team-names
                 (map find-team team-names)
                 (hash-map->list (lambda (_ value) value) %teams)))))


(define (diff-revisions rev-start rev-end)
  "Return the list of added, modified or removed files between REV-START
and REV-END, two git revision strings."
  (let* ((repository (repository-open (getcwd)))
         (commit1 (commit-lookup repository
                                 (object-id
                                  (revparse-single repository rev-start))))
         (commit2 (commit-lookup repository
                                 (object-id
                                  (revparse-single repository rev-end))))
         (diff (diff-tree-to-tree repository
                                  (commit-tree commit1)
                                  (commit-tree commit2)))
         (files '()))
    (diff-foreach
     diff
     (lambda (delta progress)
       (set! files
             (cons (diff-file-path (diff-delta-old-file delta)) files))
       0)
     (const 0)
     (const 0)
     (const 0))
    files))

(define (git-patch->commit-id file)
  "Parse the commit ID from FILE, a patch produced with git."
  (call-with-input-file file
    (lambda (port)
      (let loop ((line (read-line port)))
        (when (eof-object? line)
          (error "could not find 'from' commit in patch" file))
        (let ((m (string-match "^From ([0-9a-f]{40})" line)))
          (if m
           (match:substring m 1)
           (loop (read-line port))))))))

(define (git-patch->revisions file)
  "Return the start and end revisions of FILE, a patch file produced with git."
  (let* ((rev-end (git-patch->commit-id file))
         (rev-start (string-append rev-end "^")))
    (list rev-start rev-end)))

(define (patch->teams patch-file)
  "Return the name of the teams in scope for the changes in PATCH-FILE."
  (map (compose symbol->string team-id)
       (find-team-by-scope (apply diff-revisions
                                  (git-patch->revisions patch-file)))))

(define (team-id->forgejo-id id)
  "Return a name (string) suitable as a Forgejo team name."
  (define valid                                   ;"AlphaDashDot"
    (char-set-union char-set:ascii (char-set #\-) (char-set #\.)))

  (define (valid? chr)
    (char-set-contains? valid chr))

  (string-map (match-lambda
                (#\+ #\p)                         ;special case for "c++"
                ((? valid? chr) chr)
                (_ #\-))
              (symbol->string id)))

(define (team->codeowners-snippet team)
  (string-join (map (lambda (scope)
                      (format #f "~50a @guix/~a"
                              (if (regexp*? scope)
                                  (let ((regexp (regexp*-pattern scope)))
                                    ;; Caret may not match as expected in
                                    ;; 'CODEOWNERS' so drop it.
                                    (if (string-prefix? "^" regexp)
                                        (string-drop regexp 1)
                                        regexp))
                                  (regexp-quote scope))
                              (team-id->forgejo-id (team-id team))))
                    (team-scope team))
               "\n"
               'suffix))

(define (export-codeowners port)
  (let ((teams (sort-teams
                (hash-map->list (lambda (_ value) value) %teams))))
    (display "\
# This -*- conf -*- file was generated by './etc/teams.scm codeowners'.
#
# It describes the expected reviewers for a pull request based on the
# changed files.  Unlike what the name of the file suggests they don't
# own the code (ownership is collective in this house!) but merely have
# a good understanding of that area of the codebase and therefore are
# usually suited as a reviewer.\n\n"
             port)
    (for-each (lambda (team)
                (display (team->codeowners-snippet team) port)
                (newline port))
              teams)))


(define (main . args)
  (match args
    (("cc" . team-names)
     (apply cc (map find-team team-names)))
    (("cc-members" patch-file)
     (unless (file-exists? patch-file)
       (error "patch file does not exist:" patch-file))
     (apply main "cc-members" (git-patch->revisions patch-file)))
    (("cc-members" rev-start rev-end)
     (apply cc (find-team-by-scope
                (diff-revisions rev-start rev-end))))
    (("cc-members-header-cmd" patch-file)
     (let* ((teams (map find-team (patch->teams patch-file)))
            (members (sort-members (append-map team-members teams))))
       (unless (null? members)
         (format #true "X-Debbugs-Cc: ~{~a~^, ~}"
                 (map member->string members)))))
    (("cc-mentors-header-cmd" patch-file)
     (format #true "X-Debbugs-Cc: ~{~a~^, ~}"
             (map member->string
                  (sort-members (team-members (find-team "mentors"))))))
    (("get-maintainer" patch-file)
     (apply main "list-members" (patch->teams patch-file)))
    (("list-teams" . args)
     (list-teams))
    (("list-members" . team-names)
     (for-each
      (lambda (team-name)
        (list-members (find-team team-name)))
      team-names))
    (("show" . team-names)
     (list-teams team-names))
    (("codeowners")
     (export-codeowners (current-output-port)))
    (("sync-codeberg-teams" token)
     (synchronize-teams token))
    (anything
     (format (current-error-port)
             "Usage: etc/teams.scm <command> [<args>]

Commands:
  cc <team-name>
      get git send-email flags for cc-ing <team-name>
  cc-members <start> <end> | <patch>
      cc teams related to files changed between revisions or in a patch file
  cc-members-header-cmd <patch>
      cc-members variant for use with 'git send-email --header-cmd'
  cc-mentors-header-cmd <patch>
      command to use with 'git send-email --header-cmd' to notify mentors
  list-teams
      list teams and their members
  list-members <team-name>
      list members belonging to <team-name>
  get-maintainer <patch>
      compatibility mode with Linux get_maintainer.pl
  show <team-name>
      display <team-name> properties
  codeowners
      write a 'CODEOWNERS' file suitable for Codeberg on standard output
  sync-codeberg-teams <token>
      create or update the list of teams at Codeberg~%"))))

(apply main (cdr (command-line)))
