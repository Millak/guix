;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Eric Dvorsak <eric@dvorsak.fr>
;;; Copyright © 2015-2025 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017 Christopher Baines <mail@cbaines.net>
;;; Copyright © 2016, 2017 Danny Milosavljevic <dannym+a@scratchpost.org>
;;; Copyright © 2013, 2014, 2015, 2016, 2020 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2016, 2017, 2019-2023 Marius Bakke <marius@gnu.org>
;;; Copyright © 2015-2025 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017, 2021 Roel Janssen <roel@gnu.org>
;;; Copyright © 2016, 2017, 2020 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2016, 2017 Nikita <nikita@n0.is>
;;; Copyright © 2014, 2017, 2021 Eric Bavier <bavier@posteo.net>
;;; Copyright © 2014, 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015 Cyril Roelandt <tipecaml@gmail.com>
;;; Copyright © 2015, 2016, 2017, 2019 Leo Famulari <leo@famulari.name>
;;; Copyright © 2016, 2019, 2022 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2016–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2015, 2017 Ben Woodcroft <donttrustben@gmail.com>
;;; Copyright © 2015, 2016 Christine Lemmer-Webber <cwebber@dustycloud.org>
;;; Copyright © 2017 Adriano Peluso <catonano@gmail.com>
;;; Copyright © 2016 Dylan Jeffers <sapientech@sapientech@openmailbox.org>
;;; Copyright © 2016 David Craven <david@craven.ch>
;;; Copyright © 2017 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2015, 2016 David Thompson <davet@gnu.org>
;;; Copyright © 2017 Mark Meyer <mark@ofosos.org>
;;; Copyright © 2018 Tomáš Čech <sleep_walker@gnu.org>
;;; Copyright © 2018, 2019, 2021, 2024 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2018 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2018, 2020, 2021, 2022, 2023 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2019 Vagrant Cascadian <vagrant@debian.org>
;;; Copyright © 2019 Brendan Tildesley <mail@brendan.scot>
;;; Copyright © 2019 Pierre Langlois <pierre.langlois@gmx.com>
;;; Copyright © 2019, 2020 Tanguy Le Carrour <tanguy@bioneland.org>
;;; Copyright © 2020 Jakub Kądziołka <kuba@kadziolka.net>
;;; Copyright © 2020 Evan Straw <evan.straw99@gmail.com>
;;; Copyright © 2020 Alexandros Theodotou <alex@zrythm.org>
;;; Copyright © 2020 Holger Peters <holger.peters@posteo.de>
;;; Copyright © 2020 Noisytoot <noisytoot@gmail.com>
;;; Copyright © 2020 Edouard Klein <edk@beaver-labs.com>
;;; Copyright © 2020, 2021, 2022, 2023 Vinicius Monego <monego@posteo.net>
;;; Copyright © 2020 Konrad Hinsen <konrad.hinsen@fastmail.net>
;;; Copyright © 2020, 2022, 2024 Giacomo Leidi <goodoldpaul@autistici.org>
;;; Copyright © 2021 Ekaitz Zarraga <ekaitz@elenq.tech>
;;; Copyright © 2021 Greg Hogan <code@greghogan.com>
;;; Copyright © 2021 Maxime Devos <maximedevos@telenet.be>
;;; Copyright © 2021 Pradana Aumars <paumars@courrier.dev>
;;; Copyright © 2021, 2022, 2024 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2021, 2022, 2024 jgart <jgart@dismail.de>
;;; Copyright © 2021 Alice Brenon <alice.brenon@ens-lyon.fr>
;;; Copyright © 2021 Mekeor Melire <mekeor.melire@gmail.com>
;;; Copyright © 2022 John Kehayias <john.kehayias@protonmail.com>
;;; Copyright © 2022 Denis 'GNUtoo' Carikli <GNUtoo@cyberdimension.org>
;;; Copyright © 2022, 2023, 2025 Felix Gruber <felgru@posteo.net>
;;; Copyright © 2022 Peter Polidoro <peter@polidoro.io>
;;; Copyright © 2022 Antero Mejr <antero@mailbox.org>
;;; Copyright © 2022 Luis Henrique Gomes Higino <luishenriquegh2701@gmail.com>
;;; Copyright © 2022 Nicolas Graves <ngraves@ngraves.fr>
;;; Copyright © 2022 Tomasz Jeneralczyk <tj@schwi.pl>
;;; Copyright © 2022 msimonin <matthieu.simonin@inria.fr>
;;; Copyright © 2022 Michael Rohleder <mike@rohleder.de>
;;; Copyright © 2022 Baptiste Strazzulla <bstrazzull@hotmail.fr>
;;; Copyright © 2023 dan <i@dan.games>
;;; Copyright © 2023 John Kehayias <john.kehayias@protonmail.com>
;;; Copyright © 2023 Ivan Vilata-i-Balaguer <ivan@selidor.net>
;;; Copyright © 2024 Troy Figiel <troy@troyfigiel.com>
;;; Copyright © 2024, 2025 Sharlatan Hellseher <sharlatanus@gmail.com>
;;; Copyright © 2024 normally_js <normally_js@posteo.net>
;;; Copyright © 2024 Markku Korkeala <markku.korkeala@iki.fi>
;;; Copyright © 2024 Nguyễn Gia Phong <mcsinyx@disroot.org>
;;; Copyright © 2024 Zheng Junjie <873216071@qq.com>
;;; Copyright © 2024 Spencer King <spencer.king@geneoscopy.com>
;;; Copyright © 2024 Attila Lendvai <attila@lendvai.name>
;;; Copyright © 2025 Daniel Ziltener <dziltener@lyrion.ch>
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

(define-module (gnu packages python-web)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system python)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages crates-web)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages django)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages groff)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages node)
  #:use-module (gnu packages openstack)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-compression)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages rdf)
  #:use-module (gnu packages rpc)
  #:use-module (gnu packages rust-apps)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages time)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages)
  #:use-module (srfi srfi-1))

(define-public python-devpi-common
  (package
    (name "python-devpi-common")
    (version "4.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "devpi_common" version))
       (sha256
        (base32 "1k217b0fvsc7k2afw6wwlvqr1ksnv1vjzfm61g358vn98nd0lni3"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-lazy python-packaging-legacy
                             python-requests))
    (native-inputs
     (list python-pytest
           python-setuptools
           python-wheel))
    (home-page "https://github.com/devpi/devpi")
    (synopsis
     "Utilities jointly used by devpi-server, devpi-client and others")
    (description
     "This package provides utilities jointly used by devpi-server,
devpi-client and others.")
    (license license:expat)))

(define-public python-devpi-client
  (package
    (name "python-devpi-client")
    (version "7.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "devpi-client" version))
       (sha256
        (base32 "0ywx4ql8lsypb17n2miq39bmkrp232sdk8g6pskqp1y3b223chy1"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      ;; This wants to use pip install
      '(list "--ignore=testing/test_upload.py"
             ;; These complain about finding distutils.  It's not my fault,
             ;; mate, setuptools is right there, but you won't pick it!
             "--ignore=testing/test_list_remove.py"
             "--ignore=testing/test_main.py"
             "--ignore=testing/test_install.py"
             "--ignore=testing/test_functional.py"
             "--ignore=testing/test_test.py")
      #:phases
      '(modify-phases %standard-phases
         (add-after 'unpack 'patch-tests
           (lambda _
             ;; Do not replace the directory name of the store item.
             (substitute* "testing/conftest.py"
               (("\"devpi-server\", \"devpi-init\"")
                "\"/bin/devpi-server\", \"/bin/devpi-init\""))))
         (add-before 'check 'set-HOME
           ;; Some tests need this
           (lambda _ (setenv "HOME" "/tmp"))))))
    (propagated-inputs (list python-pypa-build
                             python-check-manifest
                             python-colorama
                             python-devpi-common
                             python-iniconfig
                             python-pkginfo
                             python-platformdirs
                             python-pluggy
                             python-pypitoken
                             python-tomli))
    (native-inputs
     (list python-devpi-server
           python-pytest
           python-setuptools
           python-virtualenv))
    (home-page "https://github.com/devpi/devpi")
    (synopsis "Devpi upload/install/... commands for Python developers")
    (description
     "The devpi command line tool is typically used in conjunction with
devpi-server.  It allows uploading, testing and installing packages from devpi
indexes.")
    (license license:expat)))

(define-public python-devpi-process
  (package
    (name "python-devpi-process")
    (version "1.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "devpi_process" version))
       (sha256
        (base32 "0v26i8bs0n8f2ikizwyvq77fw3l0nsyl4wj8yff0r3y351ydqbs1"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      '(modify-phases %standard-phases
         (add-after 'unpack 'relax-requirements
           (lambda _
             ;; Our package is too old, sorry.
             (substitute* "pyproject.toml"
               (("typing-extensions>=4.12.2")
                "typing-extensions>=4.10.0"))))
         (add-after 'unpack 'patch-tool-locations
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "src/devpi_process/__init__.py"
               (("_exe\\(\"devpi-init\"\\)")
                (string-append "\"" (search-input-file inputs "/bin/devpi-init") "\""))
               (("_exe\\(\"devpi-server\"\\)")
                (string-append "\"" (search-input-file inputs "/bin/devpi-server") "\""))
               (("_exe\\(\"devpi\"\\)")
                (string-append "\"" (search-input-file inputs "/bin/devpi") "\""))))))))
    (propagated-inputs (list python-devpi-client python-devpi-server
                             python-typing-extensions))
    (native-inputs
     (list nss-certs-for-test
           python-covdefaults
           python-hatchling
           python-hatch-vcs
           python-httpx
           python-pytest
           python-pytest-cov))
    (home-page "https://github.com/devpi/devpi")
    (synopsis "API to create and use a devpi server process")
    (description
     "This package provides a programmatic API to create and use a devpi
server process.")
    (license license:expat)))

(define-public python-devpi-server
  (package
    (name "python-devpi-server")
    (version "6.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "devpi-server" version))
       (sha256
        (base32 "08smfjhnvqj68fp45gzqm9axjcgnksc0z31v48llivnzlxwf8cmr"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      ;; These all fail with: "module 'py' has no attribute 'io'"
      '(list "--ignore=test_devpi_server/test_importexport.py"
             "--ignore=test_devpi_server/test_main.py"
             "--ignore=test_devpi_server/test_genconfig.py")))
    (propagated-inputs (list python-argon2-cffi
                             python-attrs
                             python-defusedxml
                             python-devpi-common
                             python-httpx
                             python-itsdangerous
                             python-lazy
                             python-legacy-cgi
                             python-passlib
                             python-platformdirs
                             python-pluggy
                             python-py
                             python-pyramid
                             python-repoze-lru
                             python-ruamel.yaml
                             python-strictyaml
                             python-waitress))
    (native-inputs
     (list python-execnet
           python-pytest
           python-pytest-timeout
           python-setuptools
           python-webtest
           python-wheel))
    (home-page "https://devpi.net")
    (synopsis "Pypi.org caching server")
    (description "This package implements a reliable private and pypi.org
caching server.")
    (license license:expat)))

(define-public python-domain-connect
  (package
    (name "python-domain-connect")
    (version "0.0.11")
    (source
     (origin
       (method git-fetch)               ;no tests in PyPI archive
       (uri (git-reference
             (url "https://github.com/Domain-Connect/domainconnect_python")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1dhqbx15h074g51mj73j9hlyvb11isjnj4s9ih5kbw1g4vf1q1jk"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; Check and sanity-check phases require /etc/resolv.conf, which is not
      ;; present in container.
      #:tests? #f
      #:phases #~(modify-phases %standard-phases (delete 'sanity-check))))
    (native-inputs
     (list python-setuptools
           python-wheel))
    (propagated-inputs
     (list python-cryptography
           python-dnspython
           python-future
           python-publicsuffix
           python-publicsuffixlist))
    (home-page "https://github.com/Domain-Connect/domainconnect_python")
    (synopsis "Client library for Domain Connect protocol")
    (description
     "This package provides a Service Provider functionality in both Sync and
Async mode for @url{https://domainconnect.org/, Domain Connect protocol}.")
    (license license:expat)))

(define-public python-domain-connect-dyndns
  (package
    (name "python-domain-connect-dyndns")
    (version "0.0.9")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "domain-connect-dyndns" version))
       (sha256
        (base32 "0srrblcb64bp7k5cqqivx4kykqdkmmzmspxwv66vix9k7wxdwqzx"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; Check and sanity-check phases require /etc/resolv.conf, which is not
      ;; present in container.
      #:tests? #f
      #:phases #~(modify-phases %standard-phases (delete 'sanity-check))))
    (native-inputs
     (list python-setuptools
           python-wheel))
    (propagated-inputs
     (list python-dnspython
           python-domain-connect
           python-requests
           python-validators))
    (home-page "https://github.com/Domain-Connect/DomainConnectDDNS-Python")
    (synopsis "Domain Connect Dynamic DNS in Python")
    ;; Project lacks meaningful description in README, see
    ;; <https://github.com/Domain-Connect/DomainConnectDDNS-Python/issues/43>.
    (description
     "Python client library for Dynamic DNS using
@url{https://www.domainconnect.org/, Domain Connect} protocol.")
    (license license:expat)))

(define-public python-eventlet
  (package
    (name "python-eventlet")
    (version "0.38.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "eventlet" version))
       (sha256
        (base32
         "1b7dhy3pyp3nfv0zzjrs7i3kam40cl1nh3acy2fd59ywy4x84ika"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      '(list "-k"
             (string-append
              "not TestGetaddrinfo"
              " and not TestProxyResolver"
              " and not test_noraise_dns_tcp"
              " and not test_raise_dns_tcp"
              " and not test_hosts_no_network"
              " and not test_import_rdtypes_then_eventlet"
              " and not test_patcher_existing_locks"
              " and not test_dns_methods_are_green"))
      #:phases
      '(modify-phases %standard-phases
         (add-after 'unpack 'avoid-OSError
           (lambda _
             ;; If eventlet tries to load greendns, an OSError is thrown when
             ;; getprotobyname is called.  Thankfully there is an environment
             ;; variable to disable the greendns import, so use it:
             (setenv "EVENTLET_NO_GREENDNS" "yes"))))))
    (native-inputs
     (list python-hatch-vcs
           python-hatchling
           python-pytest
           python-twine))
    (propagated-inputs
     (list python-dnspython
           python-greenlet
           python-monotonic))
    (home-page "https://eventlet.net")
    (synopsis "Concurrent networking library for Python")
    (description
     "Eventlet is a concurrent networking library for Python that
allows you to change how you run your code, not how you write it.
It uses @code{epoll} or @code{libevent} for highly scalable non-blocking I/O.
Coroutines ensure that the developer uses a blocking style of programming
that is similar to threading, but provide the benefits of non-blocking I/O.
The event dispatch is implicit, which means you can easily use @code{Eventlet}
from the Python interpreter, or as a small part of a larger application.")
    (license license:expat)))

(define-public python-hookdns
  (package
    (name "python-hookdns")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "hookdns" version))
       (sha256
        (base32 "087x12dy6slhyqwqblby2fpjdcy61yk3lqp3fplami0rmbn02fb7"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list "-k" (string-join
                    ;; Tests requiring networking.
                    (list
                     "not test_patch_contextmanager_with_another_hostname"
                     "test_patch_contextmanager_with_another_hostname_ipv6"
                     "test_patch_contextmanager_with_ipv4"
                     "test_patch_contextmanager_with_name"
                     "test_patch_contextmanager_with_public_fqdn_and_"
                     "test_patch_contextmanager_with_unknown_hostname"
                     "test_patch_decorator_with_another_hostname"
                     "test_patch_decorator_with_another_hostname_ipv6"
                     "test_patch_decorator_with_ipv4"
                     "test_patch_decorator_with_name"
                     "test_patch_decorator_with_public_fqdn_and_a_name_for_"
                     "test_patch_decorator_with_unknown_hostname"
                     "test_real_getaddrinfo_with_name_ipv6"
                     "test_real_getaddrinfo_with_public_fqdn_ipv4"
                     "test_real_getaddrinfo_with_public_fqdn_ipv6"
                     "test_real_gethostbyname_ex_with_public_fqdn"
                     "test_real_gethostbyname_with_public_fqdn"
                     "test_real_requests_ip"
                     "test_real_requests_name"
                     "test_real_requests_with_public_fqdn"
                     "test_reentrant")
                    " and not "))))
    (native-inputs
     (list python-pytest
           python-requests
           python-setuptools
           python-wheel))
    (home-page "https://github.com/cle-b/hookdns")
    (synopsis "DNS resolution customization library")
    (description
     "HookDNS implements functionality that allows for modifying name
resolution in a Python script without any changes to the hosts file or the use
of a fake DNS resolver.")
    (license license:asl2.0)))

(define-public python-huggingface-hub
  (package
    (name "python-huggingface-hub")
    (version "0.23.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/huggingface/huggingface_hub")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0hygxqcixkc1d9sr47j2km6z0p17aj4k1dzm4cvpddrvhrgqayq5"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      ;; We don't have sentence_transformers...
      '(list "--ignore=contrib/sentence_transformers/test_sentence_transformers.py"
             ;; ...nor do we have InquirerPy...
             "--ignore=tests/test_command_delete_cache.py"
             ;; ...or timm...
             "--ignore=contrib/timm/test_timm.py"
             ;; ...or spacy_huggingface_hub
             "--ignore=contrib/spacy/test_spacy.py"
             ;; These all require internet access
             "--ignore=tests/test_cache_no_symlinks.py"
             "--ignore=tests/test_cache_layout.py"
             "--ignore=tests/test_commit_scheduler.py"
             "--ignore=tests/test_file_download.py"
             "--ignore=tests/test_hf_api.py"
             "--ignore=tests/test_hf_file_system.py"
             "--ignore=tests/test_inference_api.py"
             "--ignore=tests/test_inference_async_client.py"
             "--ignore=tests/test_inference_client.py"
             "--ignore=tests/test_inference_text_generation.py"
             "--ignore=tests/test_login_utils.py"
             "--ignore=tests/test_offline_utils.py"
             "--ignore=tests/test_repocard.py"
             "--ignore=tests/test_repository.py"
             "--ignore=tests/test_snapshot_download.py"
             "--ignore=tests/test_utils_cache.py"
             "--ignore=tests/test_utils_git_credentials.py"
             "--ignore=tests/test_utils_http.py"
             "--ignore=tests/test_utils_pagination.py"
             "--ignore=tests/test_webhooks_server.py"
             "-k" (string-append
                   "not test_push_to_hub"
                   " and not test_from_pretrained_model_id_only"
                   " and not test_from_pretrained_model_id_and_revision"))
      #:phases
      '(modify-phases %standard-phases
         (add-before 'check 'pre-check
           ;; Some tests need to write to HOME.
           (lambda _ (setenv "HOME" "/tmp"))))))
    (propagated-inputs
     (list python-filelock
           python-fsspec
           python-packaging
           python-pyyaml
           python-requests
           python-tqdm
           python-typing-extensions))
    (native-inputs
     (list python-aiohttp
           python-fastapi
           python-jedi
           python-jinja2
           python-mypy
           python-numpy
           python-pillow
           python-pydantic-2
           python-pytest
           python-pytest-asyncio
           python-pytest-cov
           python-pytest-env
           python-pytest-rerunfailures
           python-pytest-vcr
           python-pytest-xdist
           python-setuptools
           python-types-requests
           python-types-toml
           python-types-urllib3
           python-typing-extensions
           python-urllib3
           python-wheel))
    (home-page "https://github.com/huggingface/huggingface_hub")
    (synopsis "Client library for accessing the huggingface.co hub")
    (description
     "This package provides a client library to download and publish models,
datasets and other repos on the @url{huggingface.co} hub.")
    (license license:asl2.0)))

(define-public python-lazr-restfulclient
  (package
    (name "python-lazr-restfulclient")
    (version "0.14.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "lazr.restfulclient" version))
       (sha256
        (base32 "11yhlqmdf2cqbdfzn8gdmzvmcivh4fflr18zf412sflvfjrdc3xz"))))
    (build-system python-build-system)
    ;; Disable the test suite to avoid the lazr.authentication requirement,
    ;; which requires the ancient 'oauth', a Python 2 only library.
    (arguments (list #:tests? #f))
    (propagated-inputs
     (list python-distro
           python-httplib2
           python-oauthlib
           python-pyparsing
           python-six
           python-wadllib))
    (home-page "https://launchpad.net/lazr.restfulclient")
    (synopsis "Web client Python library extending wadlib")
    (description "This package provides a programmable client library that
adds functionality on top of @code{wadlib}.")
    (license license:lgpl3+)))

(define-public python-launchpadlib
  (package
    (name "python-launchpadlib")
    (version "1.10.16")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "launchpadlib" version))
       (sha256
        (base32 "106aixwchwyb100wlf4cnj1vgsi2d7x40ps8xv8az27r6qwv3x0d"))))
    (build-system python-build-system)
    (arguments
     (list #:phases #~(modify-phases %standard-phases
                        (add-before 'check 'set-home
                          (lambda _
                            ;; Tests require a writable home.
                            (setenv "HOME" "/tmp"))))))
    (propagated-inputs
     (list python-httplib2
           python-keyring
           python-lazr-restfulclient
           python-lazr-uri))
    (native-inputs (list python-mock python-testresources python-wadllib))
    (home-page "https://help.launchpad.net/API/launchpadlib")
    (synopsis "Python client library for Launchpad's web service")
    (description "@code{launchpadlib} is a Python library that allows
scripting Launchpad via its the web service API.")
    (license license:lgpl3+)))

(define-public python-lazr-uri
  (package
    (name "python-lazr-uri")
    (version "1.0.6")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "lazr.uri" version))
       (sha256
        (base32 "0r44rw0bj5mayhqwfwj1dnrjgzj1lrh7ishiddd1vygnrczqa9jh"))))
    (build-system python-build-system)
    (native-inputs (list python-zope-testrunner))
    (home-page "https://launchpad.net/lazr.uri")
    (synopsis "Python URI manipulation library")
    (description "This Python package provides a self-contained, easily
reusable library for parsing, manipulating, and generating URIs.")
    (license license:lgpl3)))

(define-public python-legacy-cgi
  (package
    (name "python-legacy-cgi")
    (version "2.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "legacy_cgi" version))
       (sha256
        (base32 "0hksizyq7awmwh3337zy0rmj47x6b5igzam6ffj74gbwfjfakbgj"))))
    (build-system pyproject-build-system)
    (native-inputs (list python-poetry-core python-pytest))
    (home-page "https://github.com/jackrosenthal/legacy-cgi")
    (synopsis "Fork of the legacy standard library cgi and cgitb modules")
    (description
     "This is a fork of the standard library modules @code{cgi} and
@code{cgitb}.  They are slated to be removed from the Python standard library
in Python 3.13 by PEP-594.")
    (license license:psfl)))

(define-public python-portend
  (package
    (name "python-portend")
    (version "3.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "portend" version))
       (sha256
        (base32 "0wpvixicc3d0lack65554mvdrrckkn18p1xcgiv9v5cwq59a6l2j"))))
    (build-system pyproject-build-system)
    ;; Do not test pyproject.toml with python-pytest-checkdocs as it tries to
    ;; download dependencies.
    (arguments
     '(#:test-flags '("-k" "not project")))
    (propagated-inputs (list python-tempora))
    ;; TODO: Add python-pytest-ruff to native-inputs once it has been packaged.
    (native-inputs (list python-pytest
                         python-pytest-black
                         python-pytest-checkdocs
                         python-pytest-cov
                         python-pytest-enabler
                         python-pytest-mypy
                         python-setuptools
                         python-wheel))
    (home-page "https://github.com/jaraco/portend")
    (synopsis "Monitor TCP ports for bound or unbound states")
    (description
     "@code{python-portend} can be used to monitor TCP ports for bound or
unbound states.  For example, waiting for a port to be occupied or freed
within a set timeout.  @code{python-portend} can also be used directly from
the command line.")
    (license license:expat)))

(define-public python-prawcore
  (package
    (name "python-prawcore")
    (version "2.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/praw-dev/prawcore")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1y7gh7kk002b2h1ppkr1llb2gjfnby28zvx11j4ji0wm3r3rjh5l"))))
    (build-system pyproject-build-system)
    (arguments
     ;; XXX: These tests fail with an incomplete request response.
     (list #:test-flags
           #~'("-k" #$(string-append
                       "not test_revoke__access_token_with_refresh_set"
                       " and not test_revoke__access_token_without_refresh_set"
                       " and not test_revoke__refresh_token_with_access_set"
                       " and not test_refresh__with_scopes"
                       " and not test_request__patch"))))
    (native-inputs
     (list python-betamax
           python-betamax-matchers
           python-betamax-serializers
           python-flit-core
           python-mock
           python-pytest
           python-testfixtures))
    (propagated-inputs
     (list python-requests))
    (synopsis "Core component of PRAW")
    (description "PRAWcore is a low-level communication layer used by PRAW.")
    (home-page "https://praw.readthedocs.io/en/latest/")
    (license license:bsd-2)))

(define-public python-praw
  (package
    (name "python-praw")
    (version "7.8.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/praw-dev/praw")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "05qq43l4334cq8r8k731qnb45nq12vvfdxwbr6q84a1hafp7n4cg"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:test-flags
           #~(list "--ignore=tests/units/models"
                   "--ignore=tests/integration/models"
                   "--ignore=tests/unit/test_config.py"
                   "--ignore=tests/integration/test_reddit.py")))
    (native-inputs
     (list python-betamax
           python-betamax-matchers
           python-flit-core
           python-pytest))
    (propagated-inputs
     (list python-prawcore python-update-checker python-websocket-client))
    (synopsis "Python Reddit API Wrapper")
    (description "PRAW is a Python package that allows for simple access to
Reddit’s API.  It aims to be easy to use and internally follows all of Reddit’s
API rules.")
    (home-page "https://praw.readthedocs.io/en/latest/")
    (license license:bsd-2)))

(define-public python-frozenlist
  (package
    (name "python-frozenlist")
    ;; XXX: Any newer versions fail to build, check why.
    (version "1.3.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "frozenlist" version))
       (sha256
        (base32 "0sispcpras096fxrd0i35qs25fqy4r0x8v1d6f40pag845bwbg2q"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-pytest
           python-setuptools
           python-wheel))
    (home-page "https://github.com/aio-libs/frozenlist")
    (synopsis "List-like data structure for Python")
    (description "@code{frozenlist.FrozenList} is a list-like structure which
implements @code{collections.abc.MutableSequence}.  It can be made immutable
by calling @code{FrozenList.freeze}.")
    (license license:asl2.0)))

(define-public python-aiobotocore
  (package
    (name "python-aiobotocore")
    (version "2.17.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "aiobotocore" version))
       (sha256
        (base32 "1hlwgy1z6ln6bh7b2i9syv7q2bagjkjbws247gbgkgv5qlri6153"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; Too many tests fail seemingly because they need Internet access.
      #:tests? #false
      #:test-flags
      '(list
        ;; No module named 'tests'
        "--ignore=tests/test_config.py"
        ;; function uses no argument 's3_verify'
        "--ignore=tests/test_basic_s3.py"
        ;; function uses no argument 'signature_version'
        "--ignore=tests/test_dynamodb.py"
        ;; attempted relative import with no known parent package
        "--ignore=tests/test_stubber.py"
        ;; No module named 'pip'
        "--ignore=tests/test_version.py")))
    (propagated-inputs (list python-aiohttp
                             python-aioitertools
                             python-botocore
                             python-jmespath
                             python-multidict
                             python-dateutil
                             python-urllib3
                             python-wrapt))
    (native-inputs
     (list python-dill
           python-docutils
           python-moto
           python-pytest
           python-pytest-asyncio
           python-requests
           python-setuptools
           python-werkzeug
           python-wheel))
    (home-page "https://pypi.org/project/aiobotocore/")
    (synopsis "Async client for AWS services using botocore and aiohttp")
    (description "This package provides an async client for Amazon services
using botocore and aiohttp/asyncio.  This library is a mostly full featured
asynchronous version of botocore.")
    (license license:asl2.0)))

(define-public python-aiohappyeyeballs
  (package
    (name "python-aiohappyeyeballs")
    (version "2.4.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "aiohappyeyeballs" version))
       (sha256
        (base32 "0ibpfm552pflim399qq34cnd79xhz5qr5khqzhx1hqwwi23pvpaz"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-poetry-core
           python-pytest
           python-pytest-asyncio
           python-pytest-cov))
    (home-page "https://github.com/aio-libs/aiohappyeyeballs")
    (synopsis "Happy Eyeballs for asyncio")
    (description "This library exists to allow connecting with Happy
Eyeballs (RFC 8305) when you already have a list of @code{addrinfo} and not a
DNS name.")
    (license license:psfl)))

(define-public python-aioitertools
  (package
    (name "python-aioitertools")
    (version "0.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "aioitertools" version))
       (sha256
        (base32 "0syxv2r90d6410hc68jxhk610pdgx19n1n5rc7shaxxv9xdhbaf2"))))
    (build-system pyproject-build-system)
    ;; Curiously, no tests are collected although they exist.
    (arguments (list #:tests? #false))
    (propagated-inputs (list python-typing-extensions))
    (native-inputs
     (list python-coverage
           python-flake8
           python-flit-core
           python-mypy
           python-pytest
           python-pytest-asyncio
           python-sphinx))
    (home-page "https://pypi.org/project/aioitertools/")
    (synopsis "Itertools and builtins for AsyncIO and mixed iterables")
    (description
     "This package provides an implementation of itertools, builtins, and more
for AsyncIO and mixed-type iterables.")
    (license license:expat)))

(define-public python-aiosignal
  (package
    (name "python-aiosignal")
    (version "1.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "aiosignal" version))
       (sha256
        (base32 "1z4cnqww6j0xl6f3vx2r6kfv1hdny1pnlll7difvfj8nbvhrdkal"))))
    (build-system pyproject-build-system)
    (arguments (list #:test-flags #~(list "tests")))
    (propagated-inputs (list python-frozenlist))
    (native-inputs
     (list python-pytest
           python-pytest-asyncio
           python-pytest-cov
           python-setuptools
           python-wheel))
    (home-page "https://github.com/aio-libs/aiosignal")
    (synopsis "Callback manager for Python @code{asyncio} projects")
    (description "This Python module provides @code{Signal}, an abstraction to
register asynchronous callbacks.  The @code{Signal} abstraction can be used
for adding, removing and dropping callbacks.")
    (license license:asl2.0)))

(define-public python-aiohttp
  (package
    (name "python-aiohttp")
    (version "3.11.11")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "aiohttp" version))
       (sha256
        (base32 "0gihj076nm3863sqfnbh786621w1ad7lj7fq88d85wzbwvqwfjdv"))
       (snippet
        #~(begin
            (use-modules ((guix build utils)))
            ;; TODO: Unbundle the llhttp sources.
            ;; (delete-file-recursively "vendor")
            (delete-file "aiohttp/_http_parser.c")
            (delete-file "aiohttp/_http_writer.c")))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      '(list
        ;; This tests requires the 'proxy.py' module, not yet packaged.
        "--ignore=tests/test_proxy_functional.py"
        ;; These tests need brotli.
        "--ignore=tests/test_web_sendfile_functional.py"
        "--ignore=tests/test_web_functional.py"
        ;; These tests need pytest_codspeed
        "--ignore-glob=tests/test_benchmarks*"
        ;; These tests need python_on_whales
        "--ignore=tests/autobahn/test_autobahn.py"
        "-k"
        ;; These tests fail for unknown reasons (see:
        ;; https://github.com/aio-libs/aiohttp/issues/7130)
        (string-append
         "not test_no_warnings "
         "and not test_default_loop "
         "and not test_ctor_global_loop "
         "and not test_set_loop_default_loop "

         ;; Disable brotli tests, because we’re not providing that optional
         ;; library.
         "and not test_feed_eof_no_err_brotli "

         ;; Disable the following tests as they require
         ;; networking.
         "and not TestDeflateBuffer "
         "and not test_client_session_timeout_zero "
         "and not test_empty_body "
         "and not test_mark_formdata_as_processed[pyloop] "
         "and not test_receive_runtime_err[pyloop] "

         ;; assert 'gzip, deflate' == 'gzip, deflate, br'
         "and not test_headers[pyloop] "
         ;; ~/.. does not resolve
         "and not test_add_static_path_resolution"))
      #:phases
      '(modify-phases %standard-phases
         (add-after 'unpack 'fix-tests
           (lambda _
             ;; Make sure the timestamp of this file is > 1990, because a few
             ;; tests like test_static_file_if_modified_since_past_date depend
             ;; on it.
             (let ((late-90s (* 60 60 24 365 30)))
               (utime "tests/data.unknown_mime_type" late-90s late-90s))))
         (add-before 'build 'cythonize
           (lambda _
             ;; Adapted from the Makefile.
             (with-directory-excursion "aiohttp"
               (for-each
                (lambda (file)
                  (invoke "cython" "-3"
                          file "-I" "."))
                (find-files "." "_.*\\.pyx$"))))))))
    (propagated-inputs
     (list python-aiohappyeyeballs
           python-aiosignal
           python-async-timeout
           python-attrs
           python-frozenlist
           python-multidict
           python-propcache
           python-yarl))
    (native-inputs
     (list gunicorn-bootstrap
           python-cython-3
           python-freezegun
           python-pytest
           python-pytest-cov
           python-pytest-mock
           python-pytest-xdist
           python-re-assert
           python-setuptools
           python-wheel))
    (home-page "https://github.com/aio-libs/aiohttp/")
    (synopsis "Async HTTP client/server framework (asyncio)")
    (description "@code{aiohttp} is an asynchronous HTTP client/server
framework.

Its main features are:
@itemize
@item Supports both client and server side of HTTP protocol.
@item Supports both client and server Web-Sockets out-of-the-box without the
Callback Hell.
@item Web-server has middlewares and pluggable routing.
@end itemize")
    (license license:asl2.0)))

(define-public python-aiohttp-socks
  (package
    (name "python-aiohttp-socks")
    (version "0.10.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "aiohttp_socks" version))
       (sha256
        (base32
         "0s70jpcr9wc8ld2v1w309cz7r8mm2bipf6zbkdqqaa0z0pwf3wj9"))))
    (build-system pyproject-build-system)
    (arguments (list #:tests? #false)) ;none included
    (propagated-inputs
     (list python-aiohttp python-socks))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://github.com/romis2012/aiohttp-socks")
    (synopsis "SOCKS proxy connector for aiohttp")
    (description "This package provides a SOCKS proxy connector for
aiohttp.  It supports SOCKS4(a) and SOCKS5.")
    (license license:asl2.0)))

(define-public python-aiodns
  (package
    (name "python-aiodns")
    (version "3.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "aiodns" version))
       (sha256
        (base32
         "1mlcw14hxyzd2yg89gj1l84gfi8nbl7h32iw17myxz23wymxyswl"))))
    (build-system python-build-system)
    (propagated-inputs (list python-pycares))
    (arguments
     `(#:tests? #f))                    ;tests require internet access
    (home-page "https://github.com/saghul/aiodns")
    (synopsis "Simple DNS resolver for asyncio")
    (description "@code{aiodns} provides a simple way for doing
asynchronous DNS resolutions with a synchronous looking interface by
using @url{https://github.com/saghul/pycares,pycares}.")
    (license license:expat)))

(define-public python-aioquic
  (package
    (name "python-aioquic")
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "aioquic" version))
       (sha256
        (base32 "16bigrn5b46c7nmpzxhnlhh4y03hwc7dbd0mi5f8r53i7yxn64pr"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list "-k" (string-join
                    ;; AssertionError: AlertBadCertificate not raised
                    (list "not test_verify_subject_no_subjaltname"
                          ;; AttributeError: module
                          ;; 'service_identity.cryptography' has no attribute
                          ;; 'extract_patterns'
                          "test_verify_subject_with_subjaltname"
                          ;; AttributeError: module
                          ;; 'service_identity.cryptography' has no attribute
                          ;; 'extract_patterns'
                          "test_verify_subject_with_subjaltname_ipaddress")
                    " and not "))))
    (native-inputs
     (list nss-certs-for-test
           python-pytest
           python-setuptools
           python-wheel))
    (inputs
     (list openssl))
    (propagated-inputs
     (list python-certifi
           python-pylsqpack
           python-pyopenssl
           python-service-identity))
    (home-page "https://github.com/aiortc/aioquic")
    (synopsis "QUIC and HTTP3 implementation in Python")
    (description
     "@code{aioquic} is a library for the QUIC network protocol in Python.
It features a minimal TLS 1.3 implementation, a QUIC stack and an HTTP/3 stack.")
    (license license:bsd-3)))

(define-public python-aiorpcx
  (package
    (name "python-aiorpcx")
    (version "0.22.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "aiorpcX" version))
       (sha256
        (base32
         "0lx54bcinp44fmr8q4bbffsqbkg8kdcwykf9i5jj0bj3sfzgf9k0"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-attrs))
    (home-page "https://github.com/kyuupichan/aiorpcX")
    (synopsis "Generic asyncio RPC implementation")
    (description
     "The aiorpcX library is a generic asyncio implementation of RPC suitable
for an application that is a client, server or both.

The package includes a module with full coverage of JSON RPC versions 1.0 and
2.0, JSON RPC protocol auto-detection, and arbitrary message framing.  It also
comes with a SOCKS proxy client.")
    (license (list license:expat license:bsd-2))))

(define-public python-aiorpcx-0.18
  (package
    (inherit python-aiorpcx)
    (version "0.18.7")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "aiorpcX" version))
       (sha256
        (base32
         "1rswrspv27x33xa5bnhrkjqzhv0sknv5kd7pl1vidw9d2z4rx2l0"))))))

(define-public python-aiostream
  (package
    (name "python-aiostream")
    (version "0.4.5")
    (source (origin
              ;; Tests not included in pypi release.
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/vxgmichel/aiostream")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0l2ijb7yk0820wiyf6zrzx8fhzf8925an7r06mcaw3in1mr6rssq"))))
    (build-system python-build-system)
    (native-inputs
     (list python-pytest
           python-pytest-asyncio
           python-pytest-cov
           python-pytest-runner
           python-setuptools
           python-wheel))
    (home-page "https://github.com/vxgmichel/aiostream")
    (synopsis "Generator-based operators for asynchronous iteration")
    (description "@code{aiostream} provides a collection of stream operators that can
be combined to create asynchronous pipelines of operations.  It can be seen as an
asynchronous version of @code{itertools}, although some aspects are slightly
different. All the provided operators return a unified interface called a stream.  A
stream is an enhanced asynchronous iterable.")
    (license license:gpl3)))

(define-public python-asgiref
  (package
    (name "python-asgiref")
    (version "3.8.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "asgiref" version))
              (sha256
               (base32 "146mhkn3zp2bqf7j6r3chdlvfzgs5x1lrnqahsllgjdyl20bshy3"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-mypy python-pytest python-pytest-asyncio python-setuptools
           python-wheel))
    (home-page "https://github.com/django/asgiref/")
    (synopsis "ASGI specs, helper code, and adapters")
    (description
     "ASGI is a standard for Python asynchronous web apps and servers to
communicate with each other, and positioned as an asynchronous successor to
WSGI.  This package includes libraries for implementing ASGI servers.")
    (license license:bsd-3)))

(define-public python-asgi-csrf
  (package
    (name "python-asgi-csrf")
    (version "0.11")
    (source (origin
              (method git-fetch)        ;for tests
              (uri (git-reference
                    (url "https://github.com/simonw/asgi-csrf")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1dn9v47z2b599cnwahxvzsll2w28940ycgh5skxgq04vcqqssf29"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-itsdangerous python-multipart))
    (native-inputs (list python-asgi-lifespan
                         python-httpx
                         python-pytest
                         python-pytest-asyncio
                         python-starlette
                         python-setuptools
                         python-wheel))
    (home-page "https://github.com/simonw/asgi-csrf")
    (synopsis "ASGI middleware for protecting against CSRF attacks")
    (description "This Asynchronous Server Gateway Interface (ASGI)
middleware protects against Cross-site request forgery (CSRF) attacks.
It implements the Double Submit Cookie pattern, where a cookie is set
that is then compared to a @code{csrftoken} hidden form field or a
@code{x-csrftoken} HTTP header.")
    (license license:asl2.0)))

(define-public python-asgi-lifespan
  (package
    (name "python-asgi-lifespan")
    (version "2.1.0")
    (source (origin
              (method git-fetch)        ;for tests
              (uri (git-reference
                    (url "https://github.com/florimondmanca/asgi-lifespan")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0iqa3h61gsq1qd6j9v68k989596m9n9k1dx8zv6135rmhzzrs296"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      '(list "-c" "/dev/null"           ;ignore coverage-related options
             "-k"
             (string-append
              ;; XXX: Some tests fail because of "Exceptions from Trio nursery"
              "not (test_lifespan_manager[trio-None-None-StartupFailed]"
              " or test_lifespan_manager[trio-None-BodyFailed-None]"
              " or test_lifespan_manager[trio-None-BodyFailed-StartupFailed]"
              " or test_lifespan_manager[trio-ShutdownFailed-None-None]"
              " or test_lifespan_manager[trio-ShutdownFailed-None-StartupFailed]"
              " or test_lifespan_manager[trio-ShutdownFailed-BodyFailed-StartupFailed]"
              " or test_lifespan_timeout[trio-slow_shutdown]"
              " or test_lifespan_not_supported[trio-http_only]"
              " or test_lifespan_not_supported[trio-http_no_assert]"
              " or test_lifespan_not_supported[trio-http_no_assert_before_receive_request]"
              ")"))
      #:phases
      '(modify-phases %standard-phases
         (add-after 'unpack 'compatibility
           (lambda _
             ;; httpx version 0.28.0 removed the "app" shortcut.
             (substitute* "tests/test_manager.py"
               (("app=manager.app")
                "transport=httpx.ASGITransport(manager.app)")))))))
    (native-inputs (list python-httpx
                         python-pytest
                         python-pytest-asyncio
                         python-pytest-trio
                         python-starlette
                         python-setuptools
                         python-wheel))
    (propagated-inputs (list python-sniffio))
    (home-page "https://github.com/florimondmanca/asgi-lifespan")
    (synopsis "Programmatic startup/shutdown of ASGI apps")
    (description "Programmatically send startup/shutdown lifespan events
into Asynchronous Server Gateway Interface (ASGI) applications.  When
used in combination with an ASGI-capable HTTP client such as HTTPX, this
allows mocking or testing ASGI applications without having to spin up an
ASGI server.")
    (license license:expat)))

(define-public python-a2wsgi
  (package
    (name "python-a2wsgi")
    (version "1.10.7")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "a2wsgi" version))
              (sha256
               (base32
                "13ikyfmkx7hlrbg5rpcdm6kw4wcsy00giil3f72hpb6sw7vjyinf"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list "-k" (string-join
                    (list "not test_starlette_stream_response"
                          "test_starlette_base_http_middleware"
                          "test_baize_stream_response"
                          ;; Fails because of harmless whitespace differences.
                          "test_wsgi_post")
                    " and not "))
      #:build-backend "pdm.backend"))
    (native-inputs
     (list python-pdm-backend
           python-pytest
           ;; python-baize ; not packed yet
           python-httpx-bootstrap
           ;; Cycle: python-a2wsgi->python-uvicorn->
           ;; python-httpx->python-starlette->python-a2wsgi
           ;; python-starlette
           python-pytest-asyncio))
    (home-page "https://github.com/abersheeran/a2wsgi")
    (synopsis "Convert WSGI to ASGI or vice versa")
    (description
     "This program converts a WSGI program to an ASGI program or the other
way around.  It depends only on the Python standard library.")
    (license license:asl2.0)))

(define-public python-httpauth
  (package
    (name "python-httpauth")
    (version "0.4.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "httpauth" version))
              (sha256
               (base32
                "1m6rwvivg61l3h34hf6p6gkqmr69sb1c4k5ha379nxq0p8bfgahb"))))
    (build-system pyproject-build-system)
    (native-inputs (list python-pytest python-setuptools python-wheel))
    (home-page "https://github.com/jonashaag/httpauth")
    (synopsis "WSGI HTTP Digest Authentication middleware")
    (description "@code{python-httpauth} is WSGI middleware that secures some/all
routes using HTTP Digest Authentication.")
    (license license:bsd-2)))

(define-public python-css-html-js-minify
  (package
    (name "python-css-html-js-minify")
    (version "2.5.5")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "css-html-js-minify" version ".zip"))
              (sha256
               (base32
                "0v3l2dqdk2y4r6ax259gs4ij1zzm9yxg6491s6254vs9w3vi37sa"))))
    (build-system python-build-system)
    ;; XXX: The git repository has no tags, and the PyPI releases do not
    ;; contain tests.
    (arguments '(#:tests? #f))
    (native-inputs (list unzip))
    (home-page "https://github.com/juancarlospaco/css-html-js-minify")
    (synopsis "CSS/HTML/JS minifier")
    (description
     "This package provides a single-file minifier for CSS, HTML, and JavaScript.")
    ;; XXX: The README just says "GNU GPL and GNU LGPL and MIT".  From
    ;; <https://github.com/juancarlospaco/css-html-js-minify/issues/9> it
    ;; looks like the user can choose a license.
    (license (list license:gpl3+ license:lgpl3+ license:expat))))

(define-public python-aws-sam-translator
  (package
    (name "python-aws-sam-translator")
    (version "1.94.0")
    (source
     (origin
       (method git-fetch)               ; no tests in PyPI release
       (uri (git-reference
             (url "https://github.com/aws/serverless-application-model")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0rrmp2a2lr1bb909x34j7dqkdynx48hfwxg9m488s0mws68f78m3"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list "--numprocesses" (number->string (parallel-job-count))
              "--ignore=tests/bin/test_public_interface.py"
              "tests")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-pytest-config
            (lambda _
              ;; Drop test coverage requirements.
              (substitute* "pytest.ini"
                ((".*addopts.*") ""))))
          (add-before 'check 'pre-check
            (lambda _
              (setenv "AWS_DEFAULT_REGION" "eu-west-3"))))))
    (native-inputs
     (list python-pytest
           python-setuptools
           python-pytest-xdist
           python-parameterized
           python-pyyaml
           python-wheel))
    (propagated-inputs
     (list python-boto3
           python-jsonschema
           python-pydantic-2
           python-typing-extensions))
    (home-page "https://github.com/aws/serverless-application-model")
    (synopsis "Transform AWS SAM templates into AWS CloudFormation templates")
    (description
     "AWS SAM Translator is a library that transform @dfn{Serverless Application
Model} (SAM) templates into AWS CloudFormation templates.")
    (license license:asl2.0)))

(define-public python-aws-xray-sdk
  (package
    (name "python-aws-xray-sdk")
    (version "2.14.0")
    (home-page "https://github.com/aws/aws-xray-sdk-python")
    (source (origin
              (method git-fetch)
              (uri (git-reference (url home-page) (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0khm86218rfjmgfw0azk7gnq6y9gkj95i0i30wa5v2li1z4z8qxd"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      '(list ;; Tries to connect to external network resources
             "--ignore=tests/ext/httpx"
             ;; TODO: How to configure Django for these tests?
             "--ignore=tests/ext/django"
             ;; These tests require packages not yet in Guix.
             "--ignore=tests/ext/aiobotocore/test_aiobotocore.py"
             "--ignore=tests/ext/aiohttp/test_middleware.py"
             "--ignore=tests/ext/pg8000/test_pg8000.py"
             "--ignore=tests/ext/psycopg2/test_psycopg2.py"
             "--ignore=tests/ext/pymysql/test_pymysql.py"
             "--ignore=tests/ext/pynamodb/test_pynamodb.py"
             "--ignore=tests/ext/sqlalchemy_core/test_postgres.py"
             "--ignore=tests/ext/sqlalchemy_core/test_dburl.py"
             "--ignore=tests/test_async_recorder.py"
             ;; FIXME: module 'sqlalchemy.orm' has no attribute 'DeclarativeBase'.
             "--ignore-glob=tests/ext/sqlalchemy*"
             "--ignore=tests/ext/flask_sqlalchemy/test_query.py"
             ;; FIXME: Why is this failing?
             "--ignore=tests/test_patcher.py"
             "--ignore=tests/test_lambda_context.py"
             ;; These tests want to access httpbin.org.
             "--ignore=tests/ext/requests/test_requests.py"
             "--ignore=tests/ext/httplib/test_httplib.py"
             "--ignore=tests/ext/aiohttp/test_client.py")
      #:phases
      '(modify-phases %standard-phases
         (add-before 'check 'pre-check
           (lambda _
             ;; Allow "import tests.utils" to work as expected.
             (setenv "PYTHONPATH" (getcwd)))))))
    (native-inputs
     (list ;; These are required for the test suite.
           python-bottle
           python-flask
           python-flask-sqlalchemy
           python-httpx
           python-mock
           python-pymysql
           python-pytest
           python-pytest-aiohttp
           python-pytest-asyncio
           python-pytest-benchmark
           python-requests
           python-sqlalchemy
           python-webtest
           python-setuptools
           python-wheel))
    (propagated-inputs
     (list python-aiohttp
           python-botocore
           python-future
           python-jsonpickle
           python-urllib3
           python-wrapt))
    (synopsis "Profile applications on AWS X-Ray")
    (description
     "The AWS X-Ray SDK for Python enables Python developers to record and
emit information from within their applications to the AWS X-Ray service.")
    (license license:asl2.0)))

(define-public python-publicsuffixlist
  (package
    (name "python-publicsuffixlist")
    (version "1.0.2.20250202")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "publicsuffixlist" version))
       (sha256
        (base32 "0llam7g7sv08lcfgy18iph8br8ldjmy2qbjaykc9pd3z4iisb0yd"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-setuptools
           python-wheel))
    (home-page "https://github.com/ko-zu/psl")
    (synopsis "Public suffix list for Python")
    (description
     "This package provides an implementation of
@url{https://publicsuffix.org/, Public Suffix List } as Python library.

Features:
@itemize
@item supports IDN (unicode and punycoded)
@item shipped with built-in PSL and an updater script
@item written in Pure Python with no library dependencies
@end itemize")
    (license license:mpl2.0)))

(define-public python-python3-saml
  (package
    (name "python-python3-saml")
    (version "1.16.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/SAML-Toolkits/python3-saml")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1isisymhk8zblj5942jfl0zq392q5fpjikx52nywvzb0m2dcc81b"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; 214 tests pass, 50 fail; many fail because of required Internet
      ;; access or because of assumptions about the location of test files.
      #:tests? #false
      #:phases
      #~(modify-phases %standard-phases
          ;; FIXME: This should be fixed in python-xmlsec
          (add-before 'check 'pre-check
            (lambda* (#:key inputs #:allow-other-keys)
              (setenv "LD_LIBRARY_PATH"
                      (dirname (search-input-file inputs "lib/libxmlsec1-openssl.so.1.2.37"))))))))
    (propagated-inputs (list python-isodate python-lxml python-xmlsec))
    (native-inputs (list python-coverage
                         python-flake8
                         python-freezegun
                         python-poetry-core
                         python-pytest
                         python-setuptools
                         python-wheel))
    (home-page "https://github.com/SAML-Toolkits/python3-saml")
    (synopsis "Saml Python toolkit")
    (description
     "The SAML Python Toolkit lets you add SAML support to your Python
software.")
    (license license:expat)))

(define-public python-ovh
  (package
    (name "python-ovh")
    (version "1.1.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "ovh" version))
              (sha256
               (base32
                "0ygniv12lixh9rvnjcd01gzbzk2b5xwcg2a59b7964a77dd9p3qh"))))
    (build-system pyproject-build-system)
    (arguments (list #:tests? #f))      ; XXX: tests require networking
    (propagated-inputs (list python-requests))
    (native-inputs (list python-black
                         python-coverage
                         python-flake8
                         python-isort
                         python-pytest
                         python-pytest-cov
                         python-setuptools
                         python-sphinx
                         python-wheel))
    (home-page "https://api.ovh.com")
    (synopsis "Interact with OVHcloud APIs")
    (description "This package provides the official module to perform HTTP requests
to the OVHcloud APIs.")
    (license license:bsd-3)))

(define-public python-cbor2
  (package
    (name "python-cbor2")
    (version "5.4.2.post1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "cbor2" version))
       (sha256
        (base32 "15y78xcc3zkmvj1mdzz8gyhf3apbl91073kwhzbjk5abc1civwlw"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-pytest
           python-pytest-cov
           python-setuptools
           python-setuptools-scm
           python-wheel))
    (home-page "https://github.com/agronholm/cbor2")
    (synopsis "Concise Binary Object Representation (CBOR) (de)serializer")
    (description "This Python library provides encoding and decoding for the
Concise Binary Object
Representation (CBOR) (@url{https://www.rfc-editor.org/rfc/rfc8949.html, RFC
8949}) serialization format.  The specification is fully compatible with the
original RFC 7049.  Among its features are:
@itemize
@item Simple API like the @code{json} or @code{pickle} modules.
@item Support many CBOR tags with stdlib objects.
@item Generic tag decoding.
@item Shared value references including cyclic references.
@item String references compact encoding with repeated strings replaced with
indices.
@item Optional C module backend tested on big- and little-endian architectures.
@item Extensible tagged value handling using tag_hook and object_hook on
decode and default on encode.
@end itemize")
    (license license:expat)))

(define-public python-cfn-lint
  (package
    (name "python-cfn-lint")
    (version "1.22.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/aws-cloudformation/cfn-lint")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1zz121r9yv1irwdbk07s7958fh43h3r3q39qcj0gv4kpgb0vdf32"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list "-k" (string-join
                    (list
                     ;; Skip documentation tests.
                     "not test_update_docs"
                     ;; Tests fail with error: AssertinError ...
                     "test_parameter_for_autopublish_code_sha256"
                     "test_sam_with_language_extension"
                     "test_module_integration"
                     "test_templates"
                     ;; Test fails with error: diff error while comparing
                     ;; graphs.
                     "test_build_graph")
                    " and not "))))
    (native-inputs
     (list python-defusedxml
           python-pydot
           python-pytest
           python-setuptools
           python-wheel))
    (propagated-inputs
     (list python-aws-sam-translator
           python-importlib-resources
           python-jschema-to-python
           python-jsonpatch
           python-junit-xml
           python-networkx
           python-pyyaml
           python-regex
           python-sarif-om
           python-sympy
           python-typing-extensions))
    (home-page "https://github.com/aws-cloudformation/cfn-lint")
    (synopsis "Validate CloudFormation templates")
    (description
     "This package lets you validate CloudFormation YAML/JSON templates against
the CloudFormation spec and additional checks.  Includes checking valid values
for resource properties and best practices.")
    (license license:expat)))

(define-public python-jschema-to-python
  (package
    (name "python-jschema-to-python")
    (version "1.2.3")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "jschema_to_python" version))
              (sha256
               (base32
                "14cvaiwm56g0v6p7zviikaa5i9ln3yqy910jmp60hirhbpz19zvn"))))
    (build-system python-build-system)
    (propagated-inputs (list python-attrs python-jsonpickle python-pbr))
    (native-inputs (list python-pytest))
    (home-page "https://github.com/microsoft/jschema-to-python")
    (synopsis "Generate Python classes from a JSON schema")
    (description "This package generates source code for Python classes from a
@url{http://jschema.org,JSchema} JSON schema.")
    (license license:expat)))

(define-public python-requests-cache
  (package
    (name "python-requests-cache")
    (version "1.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "requests_cache" version))
       (sha256
        (base32 "1l8i4l2pd26gbrlcdb5z056wvsqfr3vvbyqq2f8x1f65zn3ckav8"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list "--numprocesses" (number->string (parallel-job-count))
              ;; These tests require access to external services, e.g. MongoDB
              ;; and Redis.
              "--ignore=tests/integration"
              ;; AttributeError: <class 'requests.adapters.HTTPAdapter'> does
              ;; not have the attribute 'get_connection_with_tls_context'
              "-k" "not test_mock_session")
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'pre-check
            (lambda _
              ;; PermissionError: [Errno 13] Permission denied:
              ;; '/homeless-shelter'
              (setenv "HOME" "/tmp"))))))
    (native-inputs
     (list python-poetry-core
           python-pytest
           python-pytest-rerunfailures
           python-pytest-xdist
           python-requests-mock
           python-responses
           python-rich
           python-tenacity
           python-time-machine
           python-timeout-decorator))
    (propagated-inputs
     (list python-attrs
           python-boto3
           python-bson
           python-cattrs
           python-itsdangerous
           python-orjson
           python-platformdirs
           python-pymongo
           python-pyyaml
           python-redis
           python-requests
           python-ujson
           python-url-normalize
           python-urllib3))
    (home-page "https://github.com/requests-cache/requests-cache")
    (synopsis "Persistent HTTP cache for Python requests")
    (description
     "This package provides a drop-in replacement for @code{requests.Session}
with caching offload to SQLite, Redis, MongoDB and AWS DynamoDB or save
responses as planin JSON/YAML file or save responses as plain JSON/YAML
files.")
    (license license:bsd-2)))

(define-public python-s3path
  (package
    (name "python-s3path")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "s3path" version))
       (sha256
        (base32 "0gbvyr60mkpm7jbjiya0pmx26q6cfp27p5czw08jwn3k5cp77krk"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; XXX: All tests fail with error: fixture 's3_mock' not found.
      #:tests? #f))
    (native-inputs
     (list python-setuptools
           python-wheel))
    (propagated-inputs
     (list python-boto3
           python-smart-open))
    (home-page "https://github.com/liormizr/s3path")
    (synopsis "Pathlib extension for AWS S3 Service")
    (description
     "S3Path provide a Python convenient File-System/Path like interface for
AWS S3 Service using boto3 S3 resource as a driver.")
    (license license:asl2.0)))

(define-public python-sarif-om
  (package
    (name "python-sarif-om")
    (version "1.0.4")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "sarif_om" version))
              (sha256
               (base32
                "167gb8xjm0310km3w1s12bqldbv7zyklkr4j5900vq4361ml2pyd"))))
    (build-system python-build-system)
    (propagated-inputs (list python-attrs))
    (native-inputs (list python-pbr))
    (home-page "https://github.com/microsoft/sarif-python-om")
    (synopsis "Python implementation of the SARIF 2.1.0 object model")
    (description "This module contains classes for the object model defined
by the @url{https://sarifweb.azurewebsites.net,Static Analysis Results
Interchange Format (SARIF)} file format.")
    (license license:expat)))

(define-public python-falcon
  (package
    (name "python-falcon")
    (version "4.0.2")
    (source
     (origin
       ;; Use git, as there are some test files missing from the PyPI release,
       ;; such as 'examples' directory.
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/falconry/falcon")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1zhyvfbz4c1bxd4vbsgk19dzih6kkzgin10lmsr32x3b4qgnwqxs"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      '(list "-k" "not slow"
             "--ignore-glob=examples/*" "--ignore-glob=bench/*" "tests")
      #:phases
      '(modify-phases %standard-phases
         (add-before 'check 'pre-check
           (lambda _
             (setenv "FALCON_ASGI_WRAP_NON_COROUTINES" "Y")
             (setenv "FALCON_TESTING_SESSION" "Y")
             (setenv "PYTHONASYNCIODEBUG" "1")
             (setenv "HOME" "/tmp"))))))
    (propagated-inputs
     (list python-mimeparse))
    (native-inputs
     (list python-aiofiles
           python-cbor2
           python-cython-3                ;for faster binaries
           python-fakeredis
           python-httpx
           python-mujson
           python-msgpack
           python-orjson
           python-pecan
           python-pillow
           python-pytest
           python-pytest-asyncio
           python-pytest-runner
           python-pyyaml
           python-rapidjson
           python-requests
           python-setuptools
           python-testtools
           python-ujson
           python-websockets
           python-wheel))
    (home-page "https://falconframework.org")
    (synopsis "Web framework for building APIs and application backends")
    (description "Falcon is a web API framework for building microservices,
application backends and higher-level frameworks.  Among its features are:
@itemize
@item Optimized and extensible code base
@item Routing via URI templates and REST-inspired resource
classes
@item Access to headers and bodies through request and response
classes
@item Request processing via middleware components and hooks
@item Idiomatic HTTP error responses
@item Straightforward exception handling
@item Unit testing support through WSGI helpers and mocks
@item Compatible with both CPython and PyPy
@item Cython support for better performance when used with CPython
@end itemize")
    (license license:asl2.0)))

(define-public python-falcon-cors
  (package
    (name "python-falcon-cors")
    (version "1.1.7")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "falcon-cors" version))
       (sha256
        (base32
         "12pym7hwsbd8b0c1azn95nas8gm3f1qpr6lpyx0958xm65ffr20p"))))
    (build-system python-build-system)
    (native-inputs
     (list python-falcon))
    (home-page
     "https://github.com/lwcolton/falcon-cors")
    (synopsis "Falcon @dfn{cross-origin resource sharing} (CORS) library")
    (description "This middleware provides @dfn{cross-origin resource
sharing} (CORS) support for Falcon.  It allows applying a specially crafted
CORS object to the incoming requests, enabling the ability to serve resources
over a different origin than that of the web application.")
    (license license:asl2.0)))

(define-public python-furl
  (package
    (name "python-furl")
    (version "2.1.3")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "furl" version))
        (sha256
          (base32
            "0knc76pm8pzigs3bpx9fccfsfxqrgblqphar46hq9i364vz8hqas"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-six python-orderedmultidict))
    (native-inputs
     (list python-flake8))
    (home-page "https://github.com/gruns/furl")
    (synopsis "URL manipulation in Python")
    (description "Furl provides an easy-to-use alternative to the
@code{urllib} and @code{urlparse} modules for manipulating URLs.")
    (license license:unlicense)))

(define-public python-httplib2
  (package
    (name "python-httplib2")
    (version "0.22.0")
    (source
     (origin
       ;; Tests not included in the release tarball.
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/httplib2/httplib2")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1zkp3glv0iha7p68p5m0sb8mrrammkdc0mx32517xry52s4iva7g"))))
    (build-system pyproject-build-system)
    (arguments
     (list
       #:phases
       #~(modify-phases %standard-phases
           (add-after 'unpack 'adjust-test-requirements
             (lambda _
               (substitute* "requirements-test.txt"
                 (("==") ">=")))))))
    (native-inputs
     (list python-cryptography
           python-flake8
           python-future
           python-mock
           python-pytest
           python-pytest-cov
           python-pytest-forked
           python-pytest-randomly
           python-pytest-timeout
           python-pytest-xdist
           python-six
           python-setuptools
           python-wheel))
    (home-page "https://github.com/httplib2/httplib2")
    (synopsis "Comprehensive HTTP client library")
    (description
     "A comprehensive HTTP client library supporting many features left out of
other HTTP libraries.")
    (license license:expat)))

(define-public python-cheroot
  (package
    (name "python-cheroot")
    (version "10.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "cheroot" version))
       (sha256
        (base32
         "0h0p3fnpa4dxi589s7ljlzb6p3mhqdivb3pc2f36pljqfrwjzf70"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list "--cov=cheroot"
              ;; Tests are flaky in parallel invocation.
              ;; "--numprocesses=auto"
              "--doctest-modules"
              "--showlocals"
              "-k" (string-append
                    ;; Disable test requiring networking.
                    "not test_tls_client_auth"
                    ;; TypeError: HTTPConnection.request() got an unexpected keyword
                    ;; argument 'chunked'
                    " and not test_peercreds_unix_sock"
                    " and not test_peercreds_unix_sock_with_lookup"))
      #:phases
      #~(modify-phases %standard-phases
          (replace 'check
            (lambda* (#:key tests? test-flags #:allow-other-keys)
              (when tests?
                (with-directory-excursion "/tmp"
                  (apply invoke "pytest" "-v"
                         (append test-flags (list #$output))))))))))
    (propagated-inputs
     (list python-jaraco-functools python-more-itertools))
    (native-inputs
     (list python-jaraco-text
           python-portend
           python-pyopenssl
           python-pypytools
           python-pytest-cov
           python-pytest-mock
           python-requests
           python-requests-toolbelt
           python-requests-unixsocket2
           python-setuptools
           python-setuptools-scm
           python-trustme
           python-wheel))
    (home-page "https://cheroot.cherrypy.dev")
    (synopsis "Highly-optimized, pure-python HTTP server")
    (description
     "Cheroot is a high-performance, pure-Python HTTP server.")
    (license license:bsd-3)))

(define-public httpie
  (package
    (name "httpie")
    (version "3.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "httpie" version))
       (sha256
        (base32
         "140w4mr0w7scpf4j5qm4h475vbwrgxzkdwyygwcmql1r1cgngywb"))))
    (build-system python-build-system)
    (arguments
     ;; The tests attempt to access external web servers, so we cannot run them.
     '(#:tests? #f))
    (propagated-inputs
     (list python-charset-normalizer
           python-colorama
           python-defusedxml
           python-importlib-metadata
           python-multidict
           python-pip
           python-pygments
           python-requests
           python-requests-toolbelt
           python-rich
           python-setuptools))
    (home-page "https://httpie.io")
    (synopsis "cURL-like tool for humans")
    (description
     "A command line HTTP client with an intuitive UI, JSON support,
syntax highlighting, wget-like downloads, plugins, and more.  It consists of
a single http command designed for painless debugging and interaction with
HTTP servers, RESTful APIs, and web services.")
    ;; This was fixed in 1.0.3.
    (properties `((lint-hidden-cve . ("CVE-2019-10751"))))
    (license license:bsd-3)))

(define-public parfive
  (package
    (name "parfive")
    (version "2.1.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "parfive" version))
              (sha256
               (base32
                "13nw2y7wjzj6w049av6ff4d0zxgbhkrgck0xyh676c114hcv8v6d"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; Disable tests requiring network access.
      #:test-flags
      #~(list "-k" (string-append
                    "not test_ftp"
                    " and not test_ftp_pasv_command"
                    " and not test_ftp_http"))))
    (propagated-inputs (list python-aiofiles python-aioftp python-aiohttp
                             python-tqdm))
    (native-inputs (list python-pytest
                         python-pytest-asyncio
                         python-pytest-cov
                         python-pytest-localserver
                         python-pytest-socket
                         python-setuptools
                         python-setuptools-scm
                         python-wheel))
    (home-page "https://parfive.readthedocs.io/")
    (synopsis "HTTP and FTP parallel file downloader")
    (description
     "This package provides CLI tool and Python library @code{parallel} file
downloader using asyncio. parfive can handle downloading multiple files in
parallel as well as downloading each file in a number of chunks.

asciicast demo of parfive parfive works by creating a downloader object,
appending files to it and then running the download. parfive has a synchronous
API, but uses asyncio to parallelise downloading the files.")
    (license license:expat)))

(define-public python-html2text
  (package
    (name "python-html2text")
    (version "2020.1.16")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "html2text" version))
       (sha256
        (base32
         "1fvv4z6dblii2wk1x82981ag8yhxbim1v2ksgywxsndh2s7335p2"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (invoke "pytest" "test/"))))))
    (native-inputs
     (list python-pytest))
    (home-page "https://github.com/Alir3z4/html2text")
    (synopsis "Convert HTML into plain text")
    (description "html2text takes HTML and converts it into plain ASCII text
which is also valid markdown.  html2text was originally written by Aaron
Swartz.")
    (license license:gpl3+)))

(define-public python-jose
  (package
    (name "python-jose")
    (version "3.3.0")
    (home-page "https://github.com/mpdavis/python-jose")
    (source (origin
              (method git-fetch)
              (uri (git-reference (url home-page) (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "18whsdpllg8574ma4r0qawkgw4nam6lsf63pi6761j38rvl84lg9"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      '(list "-k"
             ;; These fail because of unexpected locations of line breaks.
             (string-append "not test_public_key_to_pem "
                            "and not test_private_key_to_pem "
                            "and not test_public_key_load_cycle "
                            "and not test_private_key_load_cycle"))))
    (native-inputs
     (list ;; All native inputs are for tests.
           python-pyasn1 python-pytest python-pytest-cov
           python-pytest-runner
           python-setuptools
           python-wheel))
    (propagated-inputs
     (list python-cryptography python-ecdsa python-rsa python-six))
    (synopsis "JOSE implementation in Python")
    (description
     "The @dfn{JavaScript Object Signing and Encryption} (JOSE) technologies
- JSON Web Signature (JWS), JSON Web Encryption (JWE), JSON Web Key (JWK), and
JSON Web Algorithms (JWA) - collectively can be used to encrypt and/or sign
content using a variety of algorithms.")
    (license license:expat)))

(define-public python-joserfc
  (package
    (name "python-joserfc")
    (version "1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "joserfc" version))
       (sha256
        (base32 "11m600bmj70bp96wxhwvyj4d57w0zahzq3vic7s4a4k85pl7nl64"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      ;; No module named 'Crypto'
      '(list "--ignore=tests/jwe/test_chacha20.py"
             ;; Deprecation warnings are not raised.
             "-k" (string-append "not test_guess_bytes_key"
                                 " and not test_guess_callable_key"
                                 " and not test_guess_str_key"))))
    (propagated-inputs (list python-cryptography))
    (native-inputs (list python-pytest python-setuptools python-wheel))
    (home-page "https://github.com/authlib/joserfc")
    (synopsis "Python library for JOSE RFCs")
    (description
     "This package provides a Python library for JOSE RFCs, including JWS,
JWE, JWK, JWA, and JWT.")
    (license license:bsd-3)))

(define-public python-jwcrypto
  (package
    (name "python-jwcrypto")
    (version "1.5.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "jwcrypto" version))
              (sha256
               (base32
                "138bh6x1yy0qpk63bxa7mxnd97gfdm1fkpwm8wrdz3g3z0fca79c"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-cryptography python-deprecated))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://github.com/latchset/jwcrypto")
    (synopsis "Implementation of JOSE Web standards")
    (description
     "WCrypto is an implementation of the @dfn{Javascript Object Signing and
Encryption} (JOSE) Web Standards.")
    (license license:lgpl3+)))

(define-public python-pypitoken
  (package
    (name "python-pypitoken")
    (version "7.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pypitoken" version))
       (sha256
        (base32 "18hhwmdfs9y5ypl1n8z2h2ys2f52mv5xr043gzgb34mpmg0wi46x"))))
    (build-system pyproject-build-system)
    ;; There are no tests.
    (arguments (list #:tests? #false))
    (propagated-inputs (list python-jsonschema python-pymacaroons
                             python-typing-extensions))
    (native-inputs (list python-poetry-core))
    (home-page "https://pypitoken.readthedocs.io/en/latest/")
    (synopsis "Manipulate PyPI API tokens")
    (description "This package lets you manipulate PyPI API tokens.")
    (license license:expat)))

(define-public python-pyscss
  (package
    (name "python-pyscss")
    (version "1.4.0")
    (source
     (origin
       (method git-fetch)               ; no tests in PyPI release
       (uri (git-reference
             (url "https://github.com/Kronuz/pyScss")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1vinddg8sbh3v9n1r1wmvjx6ydk8xp7scbvhb3csl4y9xz7vhk6g"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-pytest python-pytest-cov python-setuptools python-wheel))
    (inputs
     (list pcre))
    (home-page "https://github.com/Kronuz/pyScss")
    (synopsis "Scss compiler for Python")
    (description "@code{pyScss} is a compiler for Sass, a superset language of
CSS3 that adds programming capabilities and some other syntactic sugar.")
    (license license:expat)))

(define-public python-jsonpickle
  (package
    (name "python-jsonpickle")
    (version "4.0.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "jsonpickle" version))
              (sha256
               (base32
                "0dipys8p4hkqnp9nwkpysfv7mhws2hrr73wgn00pdmq4n990hrzw"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      ;; Prevent running the flake8 and black pytest plugins, which only tests
      ;; style and frequently causes harmless failures.
      '(list "-o" "addopts=''" "tests")
      #:phases
      '(modify-phases %standard-phases
         (add-before 'check 'pre-check
           (lambda* (#:key inputs #:allow-other-keys)
             (setenv "TZ" "UTC")
             (setenv "TZDIR"
                     (search-input-directory inputs "share/zoneinfo")))))))
    (propagated-inputs
     (list python-importlib-metadata))
    (native-inputs
     (list python-bson
           python-ecdsa
           python-feedparser
           python-pymongo
           python-pytest
           python-pytest-benchmark
           python-pytest-cov
           python-pytest-enabler
           python-setuptools
           python-setuptools-scm
           python-simplejson
           python-sqlalchemy
           python-ujson
           python-wheel
           tzdata-for-tests))
    (home-page "https://jsonpickle.github.io/")
    (synopsis "Serialize object graphs into JSON")
    (description
     "This package provides a Python library for serializing any arbitrary
object graph to and from JSON.")
    (license license:bsd-3)))

(define-public python-mechanicalsoup
  (package
    (name "python-mechanicalsoup")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "MechanicalSoup" version))
       (sha256
        (base32 "01sddjxy3rznh63hnl5lbv1hhk6xyiviwmkiw4x7v4ap35fb3lrp"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-beautifulsoup4 python-lxml python-requests python-six))
    (native-inputs
     (list python-pytest-cov
           python-pytest-flake8
           python-pytest-httpbin
           python-pytest-mock
           python-pytest-runner
           python-requests-mock
           python-setuptools
           python-wheel))
    (home-page "https://mechanicalsoup.readthedocs.io/")
    (synopsis "Python library for automating website interaction")
    (description
     "MechanicalSoup is a Python library for automating interaction with
websites.  It automatically stores and sends cookies, follows redirects, and can
follow links and submit forms.  It doesn’t do JavaScript.")
    (license license:expat)))

(define-public python-hyperframe
  (package
    (name "python-hyperframe")
    (version "6.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "hyperframe" version))
       (sha256
        (base32 "055951gyhnjqpa2al52rj34g8yrls9inyn56n7nfkj0x4d300ldf"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? inputs outputs #:allow-other-keys)
             (when tests?
               (add-installed-pythonpath inputs outputs)
               (invoke "pytest" "-vv" "test")))))))
    (native-inputs
     (list python-pytest))
    (home-page "https://github.com/python-hyper/hyperframe")
    (synopsis "HTTP/2 framing layer for Python")
    (description
     "This library contains the HTTP/2 framing code used in the hyper project.
It provides a pure-Python codebase that is capable of decoding a binary stream
into HTTP/2 frames.")
    (license license:expat)))

(define-public python-hpack
  (package
    (name "python-hpack")
    (version "4.0.0")
    (source
     (origin
       ;; PyPI tarball is missing some files necessary for the tests.
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/python-hyper/hpack")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "11qdayvz5a8zlzdcdm37f2z1fgnl67pz6j8xj2dz5rfa5lds29yq"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? inputs outputs #:allow-other-keys)
             (when tests?
               (add-installed-pythonpath inputs outputs)
               (invoke "pytest" "-vv" "test")))))))
    (native-inputs
     (list python-pytest))
    (home-page "https://hyper.rtfd.org")
    (synopsis "Pure-Python HPACK header compression")
    (description
     "This module contains a pure-Python HTTP/2 header encoding (HPACK) logic
for use in Python programs that implement HTTP/2.")
    (license license:expat)))

(define-public python-h11
  (package
    (name "python-h11")
    (version "0.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "h11" version))
       (sha256
        (base32 "17b97c56y31hi0n0x0cb5a87x4xr9jijf2y06pzj0977k6zgn6cg"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-pytest python-setuptools python-wheel))
    (home-page "https://github.com/python-hyper/h11")
    (synopsis "Pure-Python, bring-your-own-I/O implementation of HTTP/1.1")
    (description
     "This is a little HTTP/1.1 library written from scratch in Python, heavily
inspired by hyper-h2.  It's a bring-your-own-I/O library; h11 contains no IO
code whatsoever.  This means you can hook h11 up to your favorite network API,
and that could be anything you want.")
    (license license:expat)))

(define-public python-h2
  (package
    (name "python-h2")
    (version "4.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "h2" version))
       (sha256
        (base32 "1fraip114fm1ha5w37pdc0sk8dn9pb0ck267zrwwpap7zc4clfm8"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; AssertionError: assert '<RemoteSettingsChanged
      ;; changed_settings:{ChangedSetting(setting=4, original_value=65536,
      ;; new_value=32768)}>' == '<RemoteSettingsChanged
      ;; changed_settings:{ChangedSetting(setting=SettingCodes.INITIAL_WINDOW_SIZE,
      ;; original_value=65536, new_value=32768)}>'
      #:test-flags
      #~(list "-k"
              (string-join
               (list "not test_remotesettingschanged_repr"
                     ;; This test exceededs the Hypothesis deadline.
                     ,@(if (target-riscv64?)
                           `("test_changing_max_frame_size")
                           '())
                     "test_streamreset_repr"
                     "test_settingsacknowledged_repr"
                     "test_connectionterminated_repr[None-None]"
                     "test_connectionterminated_repr[some"
                     "test_remotesettingschanged_repr"
                     "test_streamreset_repr"
                     "test_settingsacknowledged_repr"
                     "test_connectionterminated_repr[None-None]"
                     "test_connectionterminated_repr[some")
               " and not "))))
    (native-inputs
     (list python-pytest
           python-setuptools
           python-wheel))
    (propagated-inputs
     (list python-hpack
           python-hyperframe))
    (home-page "https://github.com/python-hyper/h2")
    (synopsis "HTTP/2 State-Machine based protocol implementation")
    (description
     "This module contains a pure-Python implementation of a HTTP/2 protocol
stack.  It does not provide a parsing layer, a network layer, or any rules
about concurrency.  Instead, it's a purely in-memory solution, defined in
terms of data actions and HTTP/2 frames.  This is one building block of a full
Python HTTP implementation.")
    (license license:expat)))

(define-public python-sockjs-tornado
  (package
    (name "python-sockjs-tornado")
    (version "1.0.6")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "sockjs-tornado" version))
       (sha256
        (base32
         "15dgv6hw6c7h3m310alw1h6p5443lrm9pyqhcv2smc13fz1v04pc"))))
    (build-system python-build-system)
    (arguments
     `(;; There are no tests, and running the test phase requires missing
       ;; dependencies
       #:tests? #f))
    (propagated-inputs
     (list python-tornado))
    (home-page "https://github.com/mrjoes/sockjs-tornado/")
    (synopsis
     "SockJS Python server implementation on top of the Tornado framework")
    (description
     "SockJS-tornado provides the server-side counterpart to a SockJS client
library, through the Tornado framework.

SockJS provides a low-latency, full-duplex, cross-domain communication channel
between a web browser and web server.")
    (license license:expat)))

(define-public python-flask-assets
  ;; The latest release still depends on the abandoned flask-script, whereas
  ;; at this later commit (already 2 years old at the time of this writing)
  ;; the dependency has been lifted.
  (let ((commit "62efd23fe95ee6a86fc1cfaa98fc1e2152093557")
        (revision "1"))
    (package
      (name "python-flask-assets")
      (version (git-version "2.1.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/miracle2k/flask-assets/")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "11q8vm2ipj4xrvawy9vl1l4k8kb4r72ghy1n8xrzci52v6lx8960"))))
      (build-system pyproject-build-system)
      (propagated-inputs
       (list python-flask python-webassets))
      (native-inputs
       (list nss-certs-for-test python-pytest python-setuptools python-wheel))
      (home-page "https://github.com/miracle2k/flask-assets")
      (synopsis "Asset management for Flask")
      (description "This package integrates @code{webassets} with Flask, adding
support for merging, minifying and compiling CSS and Javascript files.")
      (license license:bsd-2))))

(define-public python-flask-babel
  (package
    (name "python-flask-babel")
    (version "4.0.0")
    (source
     (origin
       (method git-fetch) ;no tests in pypi release
       (uri (git-reference
             (url "https://github.com/python-babel/flask-babel")
             (commit (string-append "v" version))))
       (sha256
        (base32
         "19yq3f5rixbwhv7wxdarvi9hdj98a9dq8y76in9c1q3jxaigw104"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-poetry-core
           python-pytest
           python-pytest-cov
           python-pytest-mock))
    (propagated-inputs
     (list python-babel
           python-flask
           python-jinja2
           python-pytz))
    (home-page "https://github.com/python-babel/flask-babel")
    (synopsis "Add i18n/l10n support to Flask applications")
    (description "This package implements internationalization and localization
support for Flask.  This is based on the Python babel module as well as pytz -
both of which are installed automatically if you install this library.")
    (license license:bsd-3)))

(define-public python-flask-cors
  (package
    (name "python-flask-cors")
    (version "3.0.10")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "Flask-Cors" version))
              (sha256
               (base32
                "1pl16615fn1pc5n0vdrqlxm45mqsdjjxqv3gfkrs111v7wwkj25n"))))
    (build-system python-build-system)
    (native-inputs
     (list python-flask python-nose python-packaging))
    (propagated-inputs
     (list python-six))
    (home-page "https://flask-cors.readthedocs.io/en/latest/")
    (synopsis "Handle Cross-Origin Resource Sharing with Flask")
    (description
     "This package provides a Flask extension for handling @acronym{CORS,Cross
Origin Resource Sharing}, making cross-origin AJAX possible.")
    (license license:expat)))

(define-public python-flask-markdown
  (package
    (name "python-flask-markdown")
    (version "0.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "Flask-Markdown" version))
       (sha256
        (base32
         "0l32ikv4f7va926jlq4f7gx0xid247bhlxl6bd9av5dk8ljz1hyq"))))
    (build-system python-build-system)
    (arguments
     '(#:tests? #f))        ; Tests seem to be incompatible with latest python
    (propagated-inputs
     (list python-markdown python-flask))
    (native-inputs
     (list python-nose))
    (home-page "https://github.com/dcolish/flask-markdown")
    (synopsis "Small extension to help with using Markdown in Flask")
    (description
     "Flask-Markdown supports several extensions for Markdown and integrates
into Jinja2 by default.")
    (license license:bsd-3)))

(define-public python-flask-misaka
  (package
    (name "python-flask-misaka")
    (version "1.0.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "Flask-Misaka" version))
        (sha256
          (base32
            "1yi9iall3ml1n8bff1mg49xajmldcm5pc0fkpl3w2rlnny08giax"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:test-flags #~(list "tests.py")))
    (native-inputs
     (list python-flask python-pytest python-setuptools python-wheel))
    (propagated-inputs
      (list python-markupsafe python-misaka))
    (home-page "https://github.com/singingwolfboy/flask-misaka/")
    (synopsis "Flask interface to Misaka, a Markdown parsing library")
    (description
      "This package provides an interface between the Flask web framework and
the Misaka Markdown parser.")
    (license license:expat)))

(define-public python-flask-session
  (package
    (name "python-flask-session")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "Flask-Session" version))
       (sha256
        (base32
         "0ihzlhdhss8f93p3njzva9rdm7kmhaakdlzz680wmi583wr59vf9"))))
    (build-system python-build-system)
    (arguments
     '(#:tests? #f)) ; Tests require the various storage backends to be present
    (propagated-inputs
     (list python-cachelib python-flask))
    (home-page "https://github.com/fengsp/flask-session")
    (synopsis "Adds server-side session support to your Flask application")
    (description
     "Flask-Session is an extension for Flask that adds support for
Server-side sessions, with a variety of different backends for session
storage.")
    (license license:bsd-3)))

(define-public python-html5lib
  (package
    (name "python-html5lib")
    (version "1.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "html5lib" version))
        (sha256
          (base32
            "0vqlhk0hgbsfkh7ybmby93xhlx8dq6pr5blf356ka3z2c41b9rdj"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-six python-webencodings
           ;; Required by Calibre 5.
           python-chardet))
    (arguments
     `(#:test-target "check"))
    (home-page
      "https://github.com/html5lib/html5lib-python")
    (synopsis
      "Python HTML parser based on the WHATWG HTML specification")
    (description
      "Html5lib is an HTML parser based on the WHATWG HTML specification
and written in Python.")
    (license license:expat)))

(define-public python-html5-parser
  (package
    (name "python-html5-parser")
    (version "0.4.10")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "html5-parser" version))
              (sha256
               (base32
                "195wgxls3df7djry9cz3p2k9644l6bfd66fczbaw55fsq0c48agr"))))
    (build-system python-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     (list libxml2))
    (propagated-inputs
     (list python-lxml python-beautifulsoup4 python-chardet))
    (home-page "https://html5-parser.readthedocs.io")
    (synopsis "Fast C-based HTML5 parsing for Python")
    (description "This package provides a fast implementation of the HTML5
parsing spec for Python.  Parsing is done in C using a variant of the gumbo
parser.  The gumbo parse tree is then transformed into an lxml tree, also in
C, yielding parse times that can be a thirtieth of the html5lib parse times.")
    ;; src/as-python-tree.[c|h] are licensed GPL3.  The other files
    ;; indicate ASL2.0, including the LICENSE file for the whole project.
    (license (list license:asl2.0 license:gpl3))))

(define-public python-html5tagger
  (package
    (name "python-html5tagger")
    (version "1.3.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "html5tagger" version))
              (sha256
               (base32
                "1acd1a4f66gi4plqnsml7cf33qp83mxsnmnqpdwkpj7597xkvyl4"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-setuptools python-setuptools-scm python-wheel))
    (home-page "https://github.com/sanic-org/html5tagger")
    (synopsis "Create HTML documents from Python")
    (description
     "@code{html5tagger} provides a simplified HTML5 syntax that can
be written directly in Python without templates.")
    (license license:unlicense)))

(define-public python-minio
  (package
    (name "python-minio")
    (version "7.1.9")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "minio" version))
              (sha256
               (base32
                "02nh865xbf2glxvcy70ir6gkcwqxl119zryfc70q7w0yjvkg64d7"))))
    (build-system python-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (add-before 'check 'disable-failing-tests
                    (lambda _
                      ;; This test requires network access.
                      (delete-file "tests/unit/credentials_test.py"))))))
    (native-inputs
     (list python-faker python-mock python-nose))
    (propagated-inputs
     (list python-certifi python-dateutil python-pytz python-urllib3))
    (home-page "https://github.com/minio/minio-py")
    (synopsis "Programmatically access Amazon S3 from Python")
    (description
     "This package provides a Python library for interacting with any
Amazon S3 compatible object storage server.")
    (license license:asl2.0)))

(define-public python-pycurl
  (package
    (name "python-pycurl")
    (version "7.45.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pycurl/pycurl")
             (commit (string-append
                      "REL_" (string-replace-substring version "." "_")))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "00cd1l0lsml0708hpv5k4qyhqfv0qphb0p317j9aci2wdspn1mpl"))))
    (build-system pyproject-build-system)
    (arguments
     '(#:test-flags
       ;; The test suite is not thread safe:
       ;; - some tests want to use the same port: address already in use
       ;; - some tests use signal.Signal, i.e. main-thread only
       (list "-k" (string-append
                   ;; Disable hanginging tests
                   "not test_multi_socket_select"
                   ;; E assert None is not None
                   ;; E+ where None =
                   ;; <tests.multi_callback_test.MultiCallbackTest
                   ;; testMethod=test_multi_socket_action>.timer_result
                   " and not test_multi_socket_action"
                   ;; OSError: tests/fake-curl/libcurl/with_gnutls.so: cannot
                   ;; open shared object file: No such file or directory
                   " and not test_libcurl_ssl_gnutls"
                   ;; OSError: tests/fake-curl/libcurl/with_nss.so: cannot
                   ;; open shared object file: No such file or directory
                   " and not test_libcurl_ssl_nss"
                   ;; OSError: tests/fake-curl/libcurl/with_openssl.so: cannot
                   ;; open shared object file: No such file or directory
                   " and not test_libcurl_ssl_openssl"
                   ;; Probably due to an expired CA
                   " and not test_request_without_certinfo"
                   ;; "A libcurl function was given a bad argument"
                   " and not test_proxy_tlsauth"))
       #:phases (modify-phases %standard-phases
                  (add-before 'build 'configure-tls-backend
                    (lambda _
                      ;; XXX: PycURL fails to automatically determine which
                      ;; TLS backend to use when cURL is built with
                      ;; --disable-static.  See setup.py and
                      ;; <https://github.com/pycurl/pycurl/pull/147>.
                      (setenv "PYCURL_SSL_LIBRARY" "gnutls"))))))
    (native-inputs
     (list python-bottle
           python-flaky
           python-flask
           python-pytest
           python-setuptools
           python-wheel))
    (inputs
     (list curl gnutls))
    (home-page "http://pycurl.io/")
    (synopsis "Lightweight Python wrapper around libcurl")
    (description "Pycurl is a lightweight wrapper around libcurl.  It provides
high-speed transfers via libcurl and frequently outperforms alternatives.")

    ;; Per 'README.rst', this is dual-licensed: users can redistribute pycurl
    ;; under the terms of LGPLv2.1+ or Expat.
    (license (list license:lgpl2.1+ license:expat))))

(define-public python-url-normalize
  (package
    (name "python-url-normalize")
    (version "1.4.3")
    (source
     (origin
       (method git-fetch)               ; no tests in PyPI release
       (uri (git-reference
             (url "https://github.com/niksite/url-normalize")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "09nac5nh94x0n4bfazjfxk96b20mfsx6r1fnvqv85gkzs0rwqkaq"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'use-poetry-core
            (lambda _
              ;; Patch to use the core poetry API.
              (substitute* "pyproject.toml"
                (("poetry.masonry.api") "poetry.core.masonry.api")))))))
    (native-inputs
     (list python-poetry-core
           python-pytest
           python-pytest-flakes
           python-pytest-cov
           python-pytest-socket))
    (home-page "https://github.com/niksite/url-normalize")
    (synopsis "URL normalization for Python")
    (description
     "This package provides a URI Normalization function with following
features:
@itemize
@item take care of IDN domains
@item always provide the URI scheme in lowercase characters
@item always provide the host, if any, in lowercase characters
@item only perform percent-encoding where it is essential
@item always use uppercase A-through-F characters when percent-encoding
@item prevent dot-segments appearing in non-relative URI paths
@item for schemes that define a default authority, use an empty authority if
the default is desired
@item for schemes that define an empty path to be equivalent to a path of
@code{/}, use @code{/}
@item for schemes that define a port, use an empty port if the default is
desired
@item all portions of the URI must be utf-8 encoded NFC from Unicode strings
@end itemize")
    (license license:expat)))

(define-public python-webencodings
  (package
    (name "python-webencodings")
    (version "0.5.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "webencodings" version))
              (sha256
               (base32
                "08qrgrc4hrximb2gqnl69g01s93rhf2842jfxdjljc1dbwj1qsmk"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (invoke "py.test" "-v" "webencodings/tests.py")
             #t)))))
    (native-inputs
     (list python-pytest))
    (home-page "https://github.com/SimonSapin/python-webencodings")
    (synopsis "Character encoding aliases for legacy web content")
    (description
     "In order to be compatible with legacy web content when interpreting
something like @code{Content-Type: text/html; charset=latin1}, tools need
to use a particular set of aliases for encoding labels as well as some
overriding rules.  For example, @code{US-ASCII} and @code{iso-8859-1} on
the web are actually aliases for @code{windows-1252}, and a @code{UTF-8}
or @code{UTF-16} BOM takes precedence over any other encoding declaration.
The WHATWG @url{https://encoding.spec.whatwg.org/,Encoding} standard
defines all such details so that implementations do not have to
reverse-engineer each other.

This module implements the Encoding standard and has encoding labels and
BOM detection, but the actual implementation for encoders and decoders
is Python’s.")
    (license license:bsd-3)))

(define-public python-omnipath
  ;; The latest release is incompatible with Numpy 2 and pretty old.  A new
  ;; release is expected soon.
  (let ((commit "3d1613493aa1554618fb2d7297f82e034b7694ce")
        (revision "0"))
    (package
      (name "python-omnipath")
      (version (git-version "1.0.8" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/saezlab/omnipath/")
                      (commit commit)))
                (file-name (git-file-name name commit))
                (sha256
                 (base32
                  "03lqbgqihglh9mmp7lwmar26fcnsb1qaidrzbmp0z2wvsrgi4fg7"))))
      (build-system pyproject-build-system)
      (arguments
       (list
        #:test-flags
        '(list "-k"
               (string-append
                ;; These require internet access
                "not test_download_homologene"
                " and not test_complex_genes"
                ;; Arrays are not equal: Mismatched elements: 4759 / 255465 (1.86%)
                " and not test_import_intercell_network"))
        #:phases
        #~(modify-phases %standard-phases
            (add-before 'build 'pretend-version
              ;; The version string is usually derived via setuptools-scm, but
              ;; without the git metadata available, the version string is set
              ;; to '0.0.0'.
              (lambda _
                (setenv "SETUPTOOLS_SCM_PRETEND_VERSION"
                        #$(version-major+minor (package-version this-package)))))
            (add-after 'unpack 'set-home
              (lambda _ (setenv "HOME" "/tmp"))))))
      (propagated-inputs
       (list python-attrs
             python-docrep
             python-inflect
             python-networkx
             python-packaging
             python-pandas
             python-requests
             python-tqdm
             python-typing-extensions
             python-urllib3
             python-wrapt))
      (native-inputs
       (list nss-certs-for-test
             python-bump2version
             python-pytest
             python-pytest-mock
             python-pytest-socket
             python-requests-mock
             python-setuptools
             python-setuptools-scm
             python-tox
             python-wheel))
      (home-page "https://omnipathdb.org/")
      (synopsis "Python client for the OmniPath web service")
      (description "This package provides a Python client for the OmniPath web
service.")
      (license license:expat))))

(define-public python-openai
  (package
    (name "python-openai")
    (version "1.3.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/openai/openai-python")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0k1rwhj7v22x5bnv9xbk9nj9grxqmmclh9538qqjadgqxfn5x74q"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      ;; These require internet access and an openai API key.
      '(list "--ignore=openai/tests/asyncio/test_endpoints.py"
             "--ignore=openai/tests/test_endpoints.py"
             "--ignore-glob=tests/api_resources/*"
             ;; Needs respx
             "--ignore=tests/api_resources/audio/test_speech.py"
             "--ignore=tests/api_resources/test_files.py"
             "--ignore=tests/test_client.py"
             "-k" "not test_requestor_cycle_sessions\
 and not test_requestor_sets_request_id\
 and not test_file_cli\
 and not test_basic_async\
 and not test_async\
 and not test_datetime_with_alias\
 and not test_pydantic")
      #:phases
      '(modify-phases %standard-phases
         (add-after 'unpack 'relax-requirements
           (lambda _
             (substitute* "pyproject.toml"
               (("distro>=1.7.0") "distro>=1.6.0")
               (("anyio>=3.5.0, <4") "anyio"))))
         (add-after 'unpack 'httpx-compatibility
           (lambda _
             (substitute* "src/openai/_base_client.py"
               (("proxies=proxies,") "")
               (("self._proxies = proxies") "")
               (("proxies: ProxiesTypes . None,") "")))))))
    (propagated-inputs
     (list python-aiohttp
           python-distro
           python-httpx
           python-pydantic-2
           python-requests
           python-tqdm
           python-typing-extensions))
    (native-inputs (list nss-certs-for-test
                         python-dirty-equals
                         python-hatchling
                         python-pytest python-pytest-asyncio
                         python-pytest-mock python-setuptools python-wheel))
    (home-page "https://github.com/openai/openai-python")
    (synopsis "Python client library for the OpenAI API")
    (description "This package provides a Python client library for the
OpenAI API.")
    (license license:expat)))

(define-public python-openapi-schema-validator
  (package
    (name "python-openapi-schema-validator")
    (version "0.6.2")
    (source
     (origin
       (method git-fetch)               ;no tests in pypi release
       (uri (git-reference
             (url "https://github.com/p1c2u/openapi-schema-validator")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0pgsm1x7lhc5ryj4l45ik8m5x9gqyi50dkflaiqqr4fnl0bsbx09"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      ;; This one returns 5 instead of 4 elements
      '(list "-k" "not test_array_prefixitems_invalid")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'remove-coverage-pytest-options
            (lambda _
              (substitute* "pyproject.toml"
                (("^--cov.*") ""))))
          ;; See https://github.com/python-openapi/openapi-schema-validator/issues/204
          (add-after 'unpack 'relax-requirements
            (lambda _
              (substitute* "pyproject.toml"
                (("jsonschema-specifications.*")
                 "jsonschema-specifications = \">=2023.5.2\"\n")))))))
    (native-inputs (list python-poetry-core python-pytest))
    (propagated-inputs
     (list python-isodate
           python-jsonschema
           python-rfc3339-validator
           python-strict-rfc3339))
    (home-page "https://github.com/p1c2u/openapi-schema-validator")
    (synopsis "OpenAPI schema validation library for Python")
    (description "Openapi-schema-validator is a Python library that validates
a schema against:
@itemize
@item OpenAPI Schema Specification v3.0 which is an extended subset of the
JSON Schema Specification Wright Draft 00.
@item OpenAPI Schema Specification v3.1 which is an extended superset of the
JSON Schema Specification Draft 2020-12.
@end itemize")
    (license license:bsd-3)))

(define-public python-openapi-spec-validator
  (package
    (name "python-openapi-spec-validator")
    (version "0.7.1")
    (source
     (origin
       (method git-fetch)               ;no tests in pypi release
       (uri (git-reference
             (url "https://github.com/p1c2u/openapi-spec-validator")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0s5yd4dbr6knwd2g1g4v2931k14n1lm80l11n2ija18yfis8yisz"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; These tests attempt to fetch resources from the Internet
      #:test-flags '(list "--ignore-glob=tests/integration/validation/**"
                          "-k" "not example")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'remove-coverage-pytest-options
            (lambda _
              (substitute* "pyproject.toml"
                (("^--cov.*") "")))))))
    (native-inputs
     (list python-poetry-core
           python-pytest))
    (propagated-inputs
     (list python-jsonschema
           python-jsonschema-path
           python-lazy-object-proxy
           python-openapi-schema-validator
           python-pyyaml
           python-requests
           python-setuptools))
    (home-page "https://github.com/p1c2u/openapi-spec-validator")
    (synopsis "OpenAPI spec validator")
    (description "OpenAPI Spec Validator is a Python library that validates an
OpenAPI specification against the OpenAPI 2.0 (also known as Swagger), OpenAPI
3.0 and OpenAPI 3.1 specifications.  The validator aims to check for full
compliance with the specification.")
    (license license:asl2.0)))

(define-public python-openapi-core
  (package
    (name "python-openapi-core")
    (version "0.19.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/p1c2u/openapi-core")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "092sj2933jmvq5r76khr0vphih490vls1ga3s43gz8i47aqaxx96"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      '(list "tests/unit"
             ;; Ignore Pytest configuration in setup.cfg that adds
             ;; unwanted flake8 and coverage options.
             "-c" "/dev/null"
             "-k" "not test_chars_valid")))
    (native-inputs (list python-django
                         python-falcon
                         python-flask
                         python-mock
                         python-poetry-core
                         python-pytest
                         python-responses))
    (propagated-inputs
     (list python-attrs
           python-aiohttp
           python-dictpath
           python-isodate
           python-jsonschema
           python-lazy-object-proxy
           python-more-itertools
           python-openapi-schema-validator
           python-openapi-spec-validator
           python-parse
           python-six
           python-werkzeug))
    (home-page "https://github.com/p1c2u/openapi-core")
    (synopsis "OpenAPI core library")
    (description "Openapi-core is a Python library that adds client-side and
server-side support for the OpenAPI Specification v3.  It has features such
as:
@itemize
@item Validation of requests and responses
@item Schema casting and unmarshalling
@item Media type and parameters deserialization
@item Security providers (API keys, Cookie, Basic and Bearer HTTP
authentications)
@item Custom deserializers and formats
@item Integration with libraries and frameworks.
@end itemize")
    (license license:bsd-3)))

(define-public python-openid
  (package
    (name "python-openid")
    (version "3.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "python3-openid" version))
       (sha256
        (base32
         "1bxf9a3ny1js422j962zfzl4a9dhj192pvai05whn7j0iy9gdyrk"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
        (replace 'check
          (lambda* (#:key tests? #:allow-other-keys)
            (when tests?
              (invoke "coverage" "run" "-m"
                      "unittest" "openid.test.test_suite")))))))
    (propagated-inputs
     (list python-defusedxml))
    (native-inputs
     (list python-coverage python-psycopg2 python-django))
    (home-page "https://github.com/necaris/python3-openid")
    (synopsis "OpenID support for servers and consumers")
    (description "This library provides OpenID authentication for Python, both
for clients and servers.")
    (license license:asl2.0)))

(define-public python-cssutils
  (package
    (name "python-cssutils")
    (version "2.11.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "cssutils" version))
       (sha256
        (base32
         "1qpq7xvzqp4cfcb8837rnd30x4hws3rkp33qpvmnxbxn2djsfqq5"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (replace 'check
            (lambda _
              (invoke "pytest" "-vv" "-k"
                      (string-append "not encutils "
                                     "and not website.logging")))))))
    (native-inputs
     (list python-cssselect
           python-jaraco-test
           python-lxml
           python-pytest
           python-setuptools
           python-wheel))
    (propagated-inputs
     (list python-more-itertools))
    (home-page "https://github.com/jaraco/cssutils")
    (synopsis
      "CSS Cascading Style Sheets library for Python")
    (description
      "Cssutils is a Python package for parsing and building CSS
Cascading Style Sheets.  Currently it provides a DOM only and no rendering
options.")
    (license license:lgpl3+)))

(define-public python-css-parser
  (package
    (inherit python-cssutils)
    (name "python-css-parser")
    (version "1.0.7")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "css-parser" version ".tar.gz"))
       (sha256
        (base32 "1ldv9s3an4pgk7b5x6z07rw9b5fdrasdldnf22829pb26b39dq15"))))
    (home-page "https://github.com/ebook-utils/css-parser")
    (synopsis "Fork of cssutils modified for parsing ebooks")
    (description
      "Css-parser is a Python package for parsing and building CSS
Cascading Style Sheets.  Currently it provides a DOM only and no rendering
options.

It's a fork of cssutils 1.0.2, updated and modified for parsing ebooks, due to
cssutils not receiving updates as of 1.0.2.")
    (license license:lgpl3+)))

(define-public python-cssselect
  (package
    (name "python-cssselect")
    (version "1.2.0")
    (source (origin
              ;; The PyPI release does not contain tests.
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/scrapy/cssselect")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1x4nrvb1p1byi1whmspik7lbh303akdlh762dayfxam3hycsh5kk"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-lxml python-pytest python-setuptools python-wheel))
    (home-page "https://github.com/scrapy/cssselect")
    (synopsis "CSS3 selector parser and translator to XPath 1.0")
    (description
     "Cssselect ia a Python module that parses CSS3 Selectors and translates
them to XPath 1.0 expressions.  Such expressions can be used in lxml or
another XPath engine to find the matching elements in an XML or HTML document.")
    (license license:bsd-3)))

(define-public python-databricks-cli
  (package
    (name "python-databricks-cli")
    (version "0.17.0")
    (home-page "https://github.com/databricks/databricks-cli")
    (source (origin
              (method git-fetch)
              (uri (git-reference (url home-page) (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1qwbxnx64kw7lrzlyx3hfbnjxpc19cqvvj0gcrkqpyjsgd08vja0"))))
    (build-system python-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (replace 'check
                    (lambda _
                      (invoke "pytest" "tests" "-vv"))))))
    (native-inputs
     ;; For tests.
     (list python-decorator
           python-mock
           python-pytest
           python-requests-mock))
    (propagated-inputs
     (list python-click
           python-oauthlib
           python-pyjwt
           python-requests
           python-six
           python-tabulate))
    (synopsis "Command line interface for Databricks")
    (description
     "The Databricks Command Line Interface is a tool which provides an easy
to use interface to the Databricks platform.  The CLI is built on top of the
Databricks REST APIs.")
    (license license:asl2.0)))

(define-public python-openid-cla
  (package
    (name "python-openid-cla")
    (version "1.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "python-openid-cla" version))
       (sha256
        (base32
         "102hy2qisvjxp5s0v9lvwqi4f2dk0dhns40vjgn008yxc7k0h3cr"))))
    (build-system python-build-system)
    (arguments '(#:tests? #f)) ; No tests.
    (propagated-inputs (list python-openid python-six))
    (home-page "https://github.com/puiterwijk/python-openid-cla/")
    (synopsis "Implementation of the OpenID CLA extension for python-openid")
    (description "@code{openid-cla} is an implementation of the OpenID
contributor license agreement extension for python-openid.")
    (license license:bsd-3)))

(define-public python-openid-teams
  (package
    (name "python-openid-teams")
    (version "1.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "python-openid-teams" version))
       (sha256
        (base32
         "05zrh78alav24rxkbqlpbad6d3x2nljk6z6j7kflxf3vdqa7w969"))))
    (build-system python-build-system)
    (arguments '(#:tests? #f)) ; No tests.
    (propagated-inputs (list python-openid python-six))
    (home-page "https://github.com/puiterwijk/python-openid-teams/")
    (synopsis "Implementation of the OpenID teams extension for python-openid")
    (description
     "@code{openid-teams} is an implementation of the OpenID
teams extension for python-openid.")
    (license license:bsd-3)))

(define-public python-priority
  (package
    (name "python-priority")
    (version "1.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "priority" version))
       (sha256
        (base32 "1gpzn9k9zgks0iw5wdmad9b4dry8haiz2sbp6gycpjkzdld9dhbb"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (add-installed-pythonpath inputs outputs)
             (invoke "pytest" "-vv" "test" "-k"
                     ;; This test exceeded the Hypothesis deadline.
                     "not test_period_of_repetition"))))))
    (native-inputs
     (list python-hypothesis python-pytest python-pytest-cov
           python-pytest-xdist))
    (home-page "https://python-hyper.org/projects/priority/en/latest/")
    (synopsis "Pure-Python implementation of the HTTP/2 priority tree")
    (description
     "Priority is a pure-Python implementation of the priority logic for HTTP/2,
set out in RFC 7540 Section 5.3 (Stream Priority).")
    (license license:expat)))

(define-public python-wsproto
  (package
    (name "python-wsproto")
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "wsproto" version))
       (sha256
        (base32 "0rahm1j9danv1l6i6as80acwv16ycihxkhrvwjiqh9drxhk5ymmd"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-pytest python-setuptools python-wheel))
    (propagated-inputs
     (list python-h11))
    (home-page "https://github.com/python-hyper/wsproto/")
    (synopsis "WebSockets state-machine based protocol implementation")
    (description
     "@code{wsproto} is a pure-Python implementation of a WebSocket protocol
stack.  It's written from the ground up to be embeddable in whatever program you
choose to use, ensuring that you can communicate via WebSockets, as defined in
RFC6455, regardless of your programming paradigm.")
    (license license:expat)))

(define-public hypercorn
  (package
    (name "hypercorn")
    (version "0.17.3")
    (source
     (origin
       (method git-fetch) ;PyPI does not have tests
       (uri (git-reference
             (url "https://github.com/pgjones/hypercorn")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "13v6cy4frvnddv9zak06a4lmwxp121hjyfrlapxfmb7m3i8qrm02"))))
    (build-system pyproject-build-system)
    ;; Propagate because Hypercorn also exposes functionality over a module.
    (propagated-inputs (list python-exceptiongroup
                             python-h11
                             python-taskgroup
                             python-h2
                             python-priority
                             python-tomli
                             python-wsproto))
    (native-inputs (list nss-certs-for-test
                         python-httpx
                         python-poetry-core
                         python-pytest
                         python-pytest-asyncio
                         python-pytest-cov
                         python-pytest-trio
                         python-trio))
    (home-page "https://gitlab.com/pgjones/hypercorn/")
    (synopsis "ASGI Server based on Hyper libraries")
    (description
     "Hypercorn is an ASGI web server based on the sans-io hyper, h11, h2, and
wsproto libraries and inspired by Gunicorn.  It supports HTTP/1, HTTP/2,
WebSockets (over HTTP/1 and HTTP/2), ASGI/2, and ASGI/3 specifications.  It
can utilise asyncio, uvloop, or trio worker types.")
    (license license:expat)))

(define-public python-hypercorn
  (deprecated-package "python-hypercorn" hypercorn))

(define-public python-querystring-parser
  (package
    (name "python-querystring-parser")
    (version "1.2.4")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "querystring_parser" version))
              (sha256
               (base32
                "0qlar8a0wa003hm2z6wcpb625r6vjj0a70rsni9h8lz0zwfcwkv4"))))
    (build-system python-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (replace 'check
                    (lambda _
                      ;; XXX FIXME: This test is broken with Python 3.7:
                      ;; https://github.com/bernii/querystring-parser/issues/35
                      (substitute* "querystring_parser/tests.py"
                        (("self\\.assertEqual\\(self\\.knownValuesNormalized, result\\)")
                         "True"))
                      (invoke "python" "querystring_parser/tests.py"))))))
    (propagated-inputs
     (list python-six))
    (home-page "https://github.com/bernii/querystring-parser")
    (synopsis "QueryString parser that correctly handles nested dictionaries")
    (description
     "This package provides a query string parser for Python and Django
projects that correctly creates nested dictionaries from sent form/querystring
data.")
    (license license:expat)))

(define-public python-tornado
  (package
    (name "python-tornado")
    (version "5.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "tornado" version))
       (sha256
        (base32
         "02clqk2116jbnq8lnaqmdw3p52nqrd9ib59r4xz2ll43fpcmhlaf"))))
    (build-system python-build-system)
    (arguments
     (list
      #:phases
      '(modify-phases %standard-phases
         (add-after 'unpack 'fix-collections
           (lambda _
             (substitute* "tornado/httputil.py"
               (("collections.MutableMapping")
                "collections.abc.MutableMapping"))))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (substitute* "tornado/test/runtests.py"
                 (("\"error\", category=DeprecationWarning")
                  "\"ignore\", category=DeprecationWarning")
                 ;; Disable tests that use SSL.
                 (("'tornado.test.simple_httpclient_test',") "")
                 (("'tornado.test.iostream_test',") "")
                 (("'tornado.test.httpserver_test',") "")
                 ;; Some timeouts are triggered in these two modules
                 (("'tornado.test.queues_test',") "")
                 (("'tornado.test.locks_test',") ""))
               ;; Skip all network tests
               (setenv "NO_NETWORK" "1")
               ;; Skip timing-relevant tests
               (setenv "TRAVIS" "1")
               (invoke "python" "-m" "tornado.test.runtests"
                       "--verbose=yes")))))))
    (native-inputs
     (list python-certifi))
    (home-page "https://www.tornadoweb.org/")
    (synopsis "Python web framework and asynchronous networking library")
    (description
     "Tornado is a Python web framework and asynchronous networking library,
originally developed at FriendFeed.  By using non-blocking network I/O,
Tornado can scale to tens of thousands of open connections, making it ideal
for long polling, WebSockets, and other applications that require a long-lived
connection to each user.")
    (license license:asl2.0)))

(define-public python-tornado-6
  (package
    (name "python-tornado")
    (version "6.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "tornado" version))
       (sha256
        (base32 "02v2mlvr58xg0l0gh08nswl53z73wkf23sziggypk63ffjsdbflj"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      ;; AttributeError: 'TestIOStreamWebMixin' object has no attribute 'io_loop'
      #~(list "--ignore=tornado/test/iostream_test.py")))
    (native-inputs
     (list python-certifi
           python-pytest
           python-setuptools
           python-wheel))
    (home-page "https://www.tornadoweb.org/")
    (synopsis "Python web framework and asynchronous networking library")
    (description
     "Tornado is a Python web framework and asynchronous networking library,
originally developed at FriendFeed.  By using non-blocking network I/O,
Tornado can scale to tens of thousands of open connections, making it ideal
for long polling, WebSockets, and other applications that require a long-lived
connection to each user.")
    (license license:asl2.0)))

(define-public python-tornado-http-auth
  (package
    (name "python-tornado-http-auth")
    (version "1.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "tornado-http-auth" version))
       (sha256
        (base32 "0hyc5f0a09i5yb99pk4bxpg6w9ichbrb5cv7hc9hff7rxd8w0v0x"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-tornado))
    (home-page "https://github.com/gvalkov/tornado-http-auth")
    (synopsis "Digest and basic authentication module for Tornado")
    (description
     "Provides support for adding authentication to services using the Tornado
web framework, either via the basic or digest authentication schemes.")
    (license license:asl2.0)))

(define-public python-terminado
  (package
    (name "python-terminado")
    (version "0.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "terminado" version))
       (sha256
        (base32
         "1smvra3sc9sg64w49kfn5yhagshq3x55839748ck5dvxvk4hgza6"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-tornado-6 python-ptyprocess))
    (native-inputs
     (list python-pytest))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _ (invoke "pytest" "-vv"))))))
    (home-page "https://github.com/jupyter/terminado")
    (synopsis "Terminals served to term.js using Tornado websockets")
    (description "This package provides a Tornado websocket backend for the
term.js Javascript terminal emulator library.")
    (license license:bsd-2)))

(define-public python-wsgi-intercept
  (package
    (name "python-wsgi-intercept")
    (version "1.13.1")
    (source (origin
             (method url-fetch)
             (uri (pypi-uri "wsgi_intercept" version))
             (sha256
              (base32
               "1h94dj7h7n7zdj3ll4imqjhq5vk21yidk2niwnmpw7xik9inrgbr"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; XXX: tests required internet access
      #:tests? #f))
    (propagated-inputs
     (list python-six))
    (native-inputs
     (list python-httplib2
           python-pytest
           python-requests
           python-setuptools
           python-urllib3
           python-wheel))
    (synopsis "Puts a WSGI application in place of a real URI for testing")
    (description "Wsgi_intercept installs a WSGI application in place of a real
URI for testing.  Testing a WSGI application normally involves starting a
server at a local host and port, then pointing your test code to that address.
Instead, this library lets you intercept calls to any specific host/port
combination and redirect them into a WSGI application importable by your test
program.  Thus, you can avoid spawning multiple processes or threads to test
your Web app.")
    (home-page "https://github.com/cdent/wsgi-intercept")
    (license license:expat)))

(define-public python-webob
  (package
    (name "python-webob")
    (version "1.8.6")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "WebOb" version))
       (sha256
        (base32
          "026i3z99nr3px75isa9mbnky5i7rffiv4d124h5kxfjjsxz92fma"))))
    (build-system python-build-system)
    (native-inputs
      (list python-nose))
    (home-page "https://webob.org/")
    (synopsis "WSGI request and response object")
    (description
      "WebOb provides wrappers around the WSGI request environment, and an
object to help create WSGI responses.")
    (license license:expat)))

(define-public python-zc-lockfile
  (package
    (name "python-zc-lockfile")
    (version "3.0.post1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "zc.lockfile" version))
       (sha256
        (base32 "1v41irj7azaag3f14xyviv3l8mvap74v5p3q274k68vakrnyxcmd"))))
    (build-system python-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (replace 'check
                    (lambda* (#:key tests? #:allow-other-keys)
                      (if tests?
                          (invoke "zope-testrunner" "--test-path=src")
                          (format #t "test suite not run~%")))))))
    (native-inputs (list python-zope-testing python-zope-testrunner))
    (home-page "https://github.com/zopefoundation/zc.lockfile")
    (synopsis "Interprocess locks using lock files")
    (description
     "This package provides an implementation of interprocess locks using lock
files.  These locks can also be used to mediate access to other files.")
    (license license:zpl2.1)))

(define-public python-zconfig
  (package
    (name "python-zconfig")
    (version "4.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "ZConfig" version))
       (sha256
        (base32 "0mh13p38vq7ip4zkvaplzr8w0mqrmmqiyb5y663d165slvxl5mpq"))))
    (build-system python-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (replace 'check
                    (lambda* (#:key tests? #:allow-other-keys)
                      (if tests?
                          (begin
                            ;; This test assumes we still have setup.py in the
                            ;; directory from which we import zconfig, which
                            ;; does not work after installing the package.
                            (delete-file-recursively
                             "src/ZConfig/tests/test_readme.py")
                            (invoke "zope-testrunner" "-vv" "--test-path=src"
                                    "--all"))
                          (format #t "test suite not run~%")))))))
    (native-inputs (list python-docutils python-manuel python-zope-exceptions
                         python-zope-testrunner))
    (home-page "https://github.com/zopefoundation/ZConfig/")
    (synopsis "Structured configuration library intended for general use")
    (description
     "@code{zconfig} is a configuration library intended for general
use.  It supports a hierarchical schema-driven configuration model that allows
a schema to specify data conversion routines written in Python.  Its model is
very different from the model supported by the @code{configparser} module
found in Python's standard library, and is more suitable to
configuration-intensive applications.")
    (license license:zpl2.1)))

(define-public python-zodb
  (package
    (name "python-zodb")
    (version "5.8.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "ZODB" version))
       (sha256
        (base32 "1pv4w8mnx6j4xvkcjbkh99pv8ljby7g9f7zjq7zhdmk06sykmiy6"))))
    (build-system pyproject-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (replace 'check
                    (lambda* (#:key tests? #:allow-other-keys)
                      (if tests?
                          (begin
                            ;; This test does not work after installing the
                            ;; package, since it expects the ZODB source code
                            ;; to reside in the src/ directory.
                            (delete-file-recursively
                             "src/ZODB/tests/testdocumentation.py")
                            (invoke "zope-testrunner" "-vv" "--test-path=src"
                                    "--all"))
                          (format #t "test suite not run~%")))))))
    (propagated-inputs (list python-btrees
                             python-persistent
                             python-zconfig
                             python-six
                             python-transaction
                             python-zc-lockfile
                             python-zodbpickle
                             python-zope-interface))
    (native-inputs (list python-manuel python-zope-testing
                         python-zope-testrunner))
    (home-page "http://zodb-docs.readthedocs.io")
    (synopsis "Object-oriented database for Python")
    (description
     "@code{ZODB} provides an object-oriented and @acronym{ACID,
Atomicity Consistency Isolation Durability} compliant database for Python with
a high degree of transparency.  @code{ZODB} is an object-oriented database,
not an object-relational mapping.  This comes with several advantaged:

@itemize
@item no separate language for database operations
@item very little impact on your code to make objects persistent
@item no database mapper that partially hides the database.
@item almost no seam between code and database.
@end itemize")
    (license license:zpl2.1)))

(define-public python-zodbpickle
  (package
    (name "python-zodbpickle")
    (version "3.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "zodbpickle" version))
       (sha256
        (base32 "035bjrksl4h92mvjkx6id4gjcpc1k4mbci8ryjl6l9mki7ihx77b"))))
    (build-system pyproject-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (replace 'check
                    (lambda* (#:key tests? #:allow-other-keys)
                      (if tests?
                          (invoke "zope-testrunner" "-vv" "--test-path=src"
                                  "--all")
                          (format #t "test suite not run~%")))))))
    (native-inputs (list python-zope-testrunner python-setuptools python-wheel))
    (home-page "https://github.com/zopefoundation/zodbpickle")
    (synopsis "Uniform pickling interface for @code{zodb}")
    (description
     "This package is a fork of the @code{pickle} module (and the
supporting C extension) from both Python 3.2 and Python 3.3.  The fork adds
support for the @code{noload} operations used by @code{zodb}.")
    (license (list license:psfl license:zpl2.1))))

(define-public python-zope-event
  (package
    (name "python-zope-event")
    (version "4.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "zope.event" version))
       (sha256
        (base32
         "1ksbc726av9xacml6jhcfyn828hlhb9xlddpx6fcvnlvmpmpvhk9"))))
    (build-system python-build-system)
    (home-page "https://pypi.org/project/zope.event/")
    (synopsis "Event publishing system for Python")
    (description "Zope.event provides an event publishing API, intended for
use by applications which are unaware of any subscribers to their events.  It
is a simple event-dispatching system on which more sophisticated event
dispatching systems can be built.")
    (license license:zpl2.1)))

(define-public python-zope-interface
  (package
    (name "python-zope-interface")
    (version "5.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "zope.interface" version))
       (sha256
        (base32
         "03nrl6b8cb600dnnh46y149awvrm0gxyqgwq5hdw3lvys8mw9r20"))))
    (build-system python-build-system)
    (arguments '(#:tests? #f))  ; test suite can't find python-zope-testing
    (native-inputs
     (list python-coverage python-nose python-zope-event
           python-zope-testing))
    (home-page "https://github.com/zopefoundation/zope.interface")
    (synopsis "Python implementation of the \"design by contract\"
methodology")
    (description "Zope.interface provides an implementation of \"object
interfaces\" for Python.  Interfaces are a mechanism for labeling objects as
conforming to a given API or contract.")
    (license license:zpl2.1)))

(define-public python-zope-exceptions
  (package
    (name "python-zope-exceptions")
    (version "4.6")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "zope.exceptions" version))
       (sha256
        (base32
         "1kc3hql2i35ys5alkj9csiaz2s9bx0rff585vnrrgvavqsj297b1"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (if tests?
                 (invoke "zope-testrunner" "--test-path=src")
                 (format #t "test suite not run~%")))))))
    (native-inputs
     `(("python-zope-testrunner" ,python-zope-testrunner-bootstrap)))
    (propagated-inputs
     (list python-zope-interface))
    (home-page "https://pypi.org/project/zope.exceptions/")
    (synopsis "Zope exceptions")
    (description "Zope.exceptions provides general-purpose exception types
that have uses outside of the Zope framework.")
    (license license:zpl2.1)))

(define (python-zope-bootstrap-package orig)
  (package
    (inherit orig)
    (name (string-append (package-name orig) "-bootstrap"))
    (arguments
     (if (null? (package-arguments orig))
         `(#:tests? #f
           #:phases (modify-phases %standard-phases
                      (delete 'sanity-check)))
         (substitute-keyword-arguments (package-arguments orig)
           ((#:tests? _ #f) #f)
           ((#:phases phases '%standard-phases)
            `(modify-phases ,phases
               (delete 'sanity-check))))))
    (propagated-inputs `())
    (native-inputs `())
    (properties `((hidden? . #t)))))

(define-public python-zope-exceptions-bootstrap
  (python-zope-bootstrap-package python-zope-exceptions))

(define-public python-zope-testing
  (package
    (name "python-zope-testing")
    (version "5.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "zope.testing" version))
       (sha256
        (base32 "0jfnycp9kzmmkk0rard8chd81v5yp6vnm09ky7d3qmv6svcd0z78"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-setuptools
           python-wheel))
    (home-page "https://zopetesting.readthedocs.io/")
    (synopsis "Zope testing helpers")
    (description
     "Zope.testing provides a number of testing utilities for HTML forms, HTTP
servers, regular expressions, and more.")
    (license license:zpl2.1)))

(define-public python-zope-testrunner
  (package
    (name "python-zope-testrunner")
    (version "5.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "zope.testrunner" version))
       (sha256
        (base32
         "0jyyf1dcz156q95x2y7yw2v420q2xn3cff0c5aci7hmdmcbn0gc7"))))
    (build-system python-build-system)
    (arguments
     '(#:tests? #f                    ;FIXME: Tests can't find zope.interface.
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'remove-problematic-test
           (lambda _
             ;; This test contains invalid syntax, which breaks bytecode
             ;; compilation.  For simplicity just remove it.
             (delete-file
              "src/zope/testrunner/tests/testrunner-ex/sample2/badsyntax.py"))))))
    (native-inputs
     (list python-zope-testing))
    (propagated-inputs
     (list python-six python-zope-exceptions python-zope-interface))
    (home-page "https://pypi.org/project/zope.testrunner/")
    (synopsis "Zope testrunner script")
    (description "Zope.testrunner provides a script for running Python
tests.")
    (license license:zpl2.1)))

(define-public python-zope-testrunner-bootstrap
  (package
    (inherit (python-zope-bootstrap-package python-zope-testrunner))
    (propagated-inputs
     `(("python-six" ,python-six)
       ("python-zope-exceptions" ,python-zope-exceptions-bootstrap)))
    (properties `((hidden? . #t)))))

(define-public python-zope-i18nmessageid
  (package
    (name "python-zope-i18nmessageid")
    (version "5.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "zope.i18nmessageid" version))
       (sha256
        (base32 "14mx62khys82p2gmmf3m40x4jmvcz3rndvl6qik2n2qfp13n7ds7"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-coverage python-setuptools
           python-wheel python-zope-testrunner))
    (propagated-inputs
     (list python-six))
    (home-page "https://pypi.org/project/zope.i18nmessageid/")
    (synopsis "Message identifiers for internationalization")
    (description "Zope.i18nmessageid provides facilities for declaring
internationalized messages within program source text.")
    (license license:zpl2.1)))

(define-public python-zope-schema
  (package
    (name "python-zope-schema")
    (version "7.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "zope.schema" version))
       (sha256
        (base32
          "1fgvx7nim9plxnyiq6vmah1dji7ba5290fws1i0lwk9m0g5xpm7a"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key inputs outputs tests? #:allow-other-keys)
             (add-installed-pythonpath inputs outputs)
             (if tests?
               (invoke "zope-testrunner" "--test-path=src")
               #t))))))
    (propagated-inputs
     (list python-zope-event python-zope-interface))
    (native-inputs
     (list python-zope-i18nmessageid python-zope-testing
           python-zope-testrunner))
    (home-page "https://pypi.org/project/zope.schema/")
    (synopsis "Zope data schemas")
    (description "Zope.scheme provides extensions to zope.interface for
defining data schemas.")
    (license license:zpl2.1)))

(define-public python-zope-sqlalchemy
  (package
    (name "python-zope-sqlalchemy")
    (version "1.6")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "zope.sqlalchemy" version))
              (sha256
               (base32
                "1azm2awl2ra10xl6wps3yvy14jk2rpzvsyfsb9cncm97aydbwlww"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-sqlalchemy
           python-transaction
           python-zope-interface))
    (native-inputs (list python-zope-testing))
    (home-page "https://github.com/zopefoundation/zope.sqlalchemy")
    (synopsis "Minimal SQLAlchemy transaction integration for Zope")
    (description "The aim of this package is to unify the plethora of existing
packages integrating SQLAlchemy with Zope's transaction management.  As such,
it only provides a data manager and makes no attempt to define a @i{zopeish}
way to configure engines.")
    (license license:zpl2.1)))

(define-public python-zope-configuration
  (package
    (name "python-zope-configuration")
    (version "4.4.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "zope.configuration" version))
              (sha256
               (base32
                "0g6vrl7y27z9cj5xyrww9xlzk4npj55mgmlrcd9d2nj08jn2pw79"))))
    (build-system python-build-system)
    (native-inputs
     (list python-manuel python-zope-testing python-zope-testrunner))
    (propagated-inputs
     (list python-zope-i18nmessageid python-zope-interface
           python-zope-schema))
    (home-page "https://pypi.org/project/zope.configuration/")
    (synopsis "Zope Configuration Markup Language")
    (description "Zope.configuration implements ZCML, the Zope Configuration
Markup Language.")
    (license license:zpl2.1)))

(define-public python-zope-configuration-bootstrap
  (python-zope-bootstrap-package python-zope-configuration))

(define-public python-zope-copy
  (package
    (name "python-zope-copy")
    (version "4.2")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "zope.copy" version))
        (sha256
         (base32
          "06m75434krl57n6p73c2qj55k5i3fixg887j8ss01ih6zw4rvfs7"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (invoke "zope-testrunner" "--test-path=src" "\\[]"))))))
    (propagated-inputs
     (list python-zope-interface))
    (native-inputs
     `(("python-zope-component" ,python-zope-component-bootstrap)
       ("python-zope-location" ,python-zope-location-bootstrap)
       ("python-zope-testing" ,python-zope-testing)
       ("python-zope-testrunner" ,python-zope-testrunner)))
    (home-page "https://github.com/zopefoundation/zope.copy")
    (synopsis "Pluggable object copying mechanism")
    (description
     "This package provides a pluggable mechanism for copying persistent objects.")
    (license license:zpl2.1)))

(define-public python-zope-proxy
  (package
    (name "python-zope-proxy")
    (version "4.3.5")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "zope.proxy" version))
       (sha256
        (base32
         "14h7nyfbl5vpfk0rbviy4ygdfx0yx5kncvg6jpbdb0dhwna0ssm6"))))
    (build-system python-build-system)
    (native-inputs
     (list python-zope-security-bootstrap python-zope-testrunner))
    (propagated-inputs
     (list python-zope-interface))
    (home-page "https://pypi.org/project/zope.proxy/")
    (synopsis "Generic, transparent proxies")
    (description "Zope.proxy provides generic, transparent proxies for Python.
Proxies are special objects which serve as mostly-transparent wrappers around
another object, intervening in the apparent behavior of the wrapped object
only when necessary to apply the policy (e.g., access checking, location
brokering, etc.) for which the proxy is responsible.")
    (license license:zpl2.1)))

(define-public python-zope-proxy-bootstrap
  (python-zope-bootstrap-package python-zope-proxy))

(define-public python-zope-hookable
  (package
    (name "python-zope-hookable")
    (version "5.0.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "zope.hookable" version))
        (sha256
         (base32
          "0hc82lfr7bk53nvbxvjkibkarngyrzgfk2i6bg8wshl0ly0pdl19"))))
    (build-system python-build-system)
    (native-inputs
     (list python-coverage python-zope-testing))
    (home-page "https://github.com/zopefoundation/zope.hookable")
    (synopsis "Zope hookable")
    (description "This package supports the efficient creation of hookable
objects, which are callable objects that are meant to be optionally replaced.
The idea is that you create a function that does some default thing and make i
hookable.  Later, someone can modify what it does by calling its sethook method
and changing its implementation.  All users of the function, including those
that imported it, will see the change.")
    (license license:zpl2.1)))

(define-public python-zope-location
  (package
    (name "python-zope-location")
    (version "4.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "zope.location" version))
       (sha256
        (base32
         "1b40pzl8v00d583d3gsxv1qjdw2dhghlgkbgxl3m07d5r3izj857"))))
    (build-system python-build-system)
    (arguments
     '(#:tests? #f)) ; FIXME: Tests can't find zope.interface.
    (native-inputs
     (list python-zope-testrunner))
    (propagated-inputs
     (list python-zope-interface python-zope-proxy python-zope-schema))
    (home-page "https://pypi.org/project/zope.location/")
    (synopsis "Zope location library")
    (description "Zope.location implements the concept of \"locations\" in
Zope3, which are are special objects that have a structural location.")
    (license license:zpl2.1)))

(define-public python-zope-location-bootstrap
  (python-zope-bootstrap-package python-zope-location))

(define-public python-zope-security
  (package
    (name "python-zope-security")
    (version "5.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "zope.security" version))
       (sha256
        (base32
         "11lfw67cigscfax9c5j63xcvz2qcj724zx5fcdqyc94am2glim0h"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-zope-component
           python-zope-i18nmessageid
           python-zope-interface
           python-zope-location
           python-zope-proxy
           python-zope-schema))
    (native-inputs
     (list python-btrees
           python-zope-component-bootstrap
           python-zope-configuration-bootstrap
           python-zope-location-bootstrap
           python-zope-testing
           python-zope-testrunner))
    (home-page "https://pypi.org/project/zope.security/")
    (synopsis "Zope security framework")
    (description "Zope.security provides a generic mechanism to implement
security policies on Python objects.")
    (license license:zpl2.1)))

(define-public python-zope-security-bootstrap
  (package
    (inherit (python-zope-bootstrap-package python-zope-security))
    (propagated-inputs
     `(("python-zope-i18nmessageid" ,python-zope-i18nmessageid)
       ("python-zope-interface" ,python-zope-interface)
       ("python-zope-proxy" ,python-zope-proxy-bootstrap)
       ("python-zope-schema" ,python-zope-schema)))))

(define-public python-zope-component
  (package
    (name "python-zope-component")
    (version "4.6.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "zope.component" version))
       (sha256
        (base32
         "14iwp95hh6q5dj4k9h1iw75cbp89bs27nany4dinyglb44c8jqli"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (if tests?
                 (invoke "python" "setup.py" "test")
                 (format #t "test suite not run~%")))))))
    (native-inputs
     `(("python-persistent" ,python-persistent)
       ("python-zope-configuration" ,python-zope-configuration-bootstrap)
       ("python-zope-i18nmessageid" ,python-zope-i18nmessageid)
       ("python-zope-location" ,python-zope-location-bootstrap)
       ("python-zope-proxy" ,python-zope-proxy-bootstrap)
       ("python-zope-security" ,python-zope-security-bootstrap)
       ("python-zope-testing" ,python-zope-testing)
       ("python-zope-testrunner" ,python-zope-testrunner)))
    (propagated-inputs
     (list python-zope-deferredimport python-zope-deprecation
           python-zope-event python-zope-hookable python-zope-interface))
    (home-page "https://github.com/zopefoundation/zope.component")
    (synopsis "Zope Component Architecture")
    (description "Zope.component represents the core of the Zope Component
Architecture.  Together with the zope.interface package, it provides
facilities for defining, registering and looking up components.")
    (license license:zpl2.1)))

(define-public python-zope-component-bootstrap
  (python-zope-bootstrap-package python-zope-component))

(define-public python-zope-deferredimport
  (package
    (name "python-zope-deferredimport")
    (version "4.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "zope.deferredimport" version))
        (sha256
         (base32
          "1q89v54dwniiqypjbwywwdfjdr4kdkqlyqsgrpplgvsygdg39cjp"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-zope-proxy))
    (native-inputs
     (list python-zope-testrunner))
    (home-page "https://github.com/zopefoundation/zope.deferredimport")
    (synopsis "Defer imports until used by code")
    (description
     "Often, especially for package modules, you want to import names for
convenience, but not actually perform the imports until necessary.  The
@code{zope.deferredimport} package provided facilities for defining names in
modules that will be imported from somewhere else when used.  You can also cause
deprecation warnings to be issued when a variable is used.")
    (license license:zpl2.1)))

(define-public python-ndg-httpsclient
  (package
    (name "python-ndg-httpsclient")
    (version "0.5.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "ndg_httpsclient" version))
              (sha256
                (base32
                  "0412b7i1s4vj7lz9r72nmb28h9syd4q2x89bdirkkc3a6z8awbyp"))))
    (build-system python-build-system)
    (arguments
     '(;; The tests appear to require networking.
       #:tests? #f))
    (propagated-inputs
     (list python-pyasn1 python-pyopenssl))
    (synopsis "HTTPS support for Python's httplib and urllib2")
    (description "This is a HTTPS client implementation for httplib and urllib2
based on PyOpenSSL.  PyOpenSSL provides a more fully-featured SSL implementation
over the default provided with Python and, importantly, enables full
verification of the SSL peer.")
    (home-page "https://github.com/cedadev/ndg_httpsclient/")
    (license license:bsd-3)))

(define-public python-nh3
  (package
    (name "python-nh3")
    (version "0.2.20")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "nh3" version))
       (sha256
        (base32 "1mcf3y5294glji9lhzh57wymw4srbvzdg0kcakm0p2pqgwnw81cp"))))
    (build-system cargo-build-system)
    (arguments
     (list
      #:imported-modules `(,@%cargo-build-system-modules
                           ,@%pyproject-build-system-modules)
      #:modules '((guix build cargo-build-system)
                  ((guix build pyproject-build-system) #:prefix py:)
                  (guix build utils))
      #:phases
      #~(modify-phases %standard-phases
          (replace 'build (assoc-ref py:%standard-phases 'build))
          (replace 'install (assoc-ref py:%standard-phases 'install))
          ;; cargo-build-system's %standard-phases has 'check before 'install.
          (delete 'check)
          (add-after 'install 'check
            (lambda* (#:key tests? inputs outputs #:allow-other-keys)
              (when tests?
                (py:add-installed-pythonpath inputs outputs)
                (invoke "pytest" "-vv" "tests")))))
      #:cargo-inputs
      `(("rust-ammonia" ,rust-ammonia-4)
        ("rust-pyo3" ,rust-pyo3-0.23))
      #:install-source? #false))
    (native-inputs (list maturin python-pytest python-wrapper))
    (home-page "https://nh3.readthedocs.io")
    (synopsis "Python bindings to Ammonia HTML sanitization library")
    (description "This package provides Python bindings to Ammonia HTML
sanitizer Rust crate.")
    (license license:expat)))

(define-public python-noiseprotocol
  (package
    (name "python-noiseprotocol")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "noiseprotocol" version))
       (sha256
        (base32 "0ifnj0mpbqsfqba9n12vf5yzxj4qf2gxql3ry43qyshgnrqsi4mh"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-cryptography))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://github.com/plizonczyk/noiseprotocol")
    (synopsis "Implementation of Noise Protocol Framework")
    (description
     "This package provides an implementation of Noise Protocol Framework.")
    (license license:expat)))

(define-public python-websocket-client
  (package
    (name "python-websocket-client")
    (version "1.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "websocket_client" version))
       (sha256
        (base32 "1nn3qi5g59j55wfy2z02j6lq2cm281fq0wi406b2yqys8jgxyf9j"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      ;; This test requires networking.
      '(list "-k" "not testConnect")))
    (native-inputs
     (list python-pysocks
           python-pytest
           python-setuptools
           python-websockets
           python-wheel))
    (home-page "https://github.com/websocket-client/websocket-client")
    (synopsis "WebSocket client for Python")
    (description "The Websocket-client module provides the low level APIs for
WebSocket usage in Python programs.")
    (license license:lgpl2.1+)))

(define-public python-websocket-client-0.59
  (package
    (inherit python-websocket-client)
    (version "0.59.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "websocket-client" version))
       (sha256
        (base32 "0p0cz2mdissq7iw1n7jrmsfir0jfmgs1dvnpnrx477ffx9hbsxnk"))))))

(define-public python-purl
  (package
    (name "python-purl")
    (version "1.5")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "purl" version))
        (sha256
          (base32
            "15ibnz1xrh5msmn04j0nr00sz4n7jwx6cwd6zlx99kkz3vpin53m"))))
    (build-system python-build-system)
    (propagated-inputs (list python-six))
    (home-page
      "https://github.com/codeinthehole/purl")
    (synopsis
      "Python package for URL manipulation")
    (description
      "Purl is a Python package for handling URLs.")
    (license license:expat)))

(define-public python-apiron
  (package
    (name "python-apiron")
    (version "5.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "apiron" version))
       (sha256
        (base32 "1qwbqn47sf0aqznj1snbv37v8ijx476qqkjf5l9pac7xjkxsr8qk"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:test-flags '(list "-k" "not test_call")))
    (propagated-inputs
     (list python-requests))
    (native-inputs
     (list python-pytest python-pytest-cov
           python-setuptools python-wheel))
    (home-page "https://github.com/ithaka/apiron")
    (synopsis "Python wrapper for interacting with RESTful APIs")
    (description
     "@code{apiron} provides a declarative, structured configuration of
services and endpoints with a unified interface for interacting with RESTful
APIs.")
    (license license:expat)))

(define-public python-beren
  (package
    (name "python-beren")
    (version "0.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "beren" version))
       (sha256
        (base32 "1v3mdwfqsyza892zvs124ym9w1bkng1j56b7l4dwfjir3723xcgf"))))
    (build-system python-build-system)
    (arguments
     ;; The test tries to open a connection to a remote server.
     `(#:tests? #f))
    (propagated-inputs
     (list python-apiron))
    (home-page "https://github.com/teffalump/beren")
    (synopsis "REST client for Orthanc DICOM servers")
    (description
     "@code{beren} provides a REST client for Orthanc, a DICOM server.")
    (license license:gpl3+)))

(define-public python-requests
  (package
    (name "python-requests")
    (version "2.31.0")
    (source (origin
             (method url-fetch)
             (uri (pypi-uri "requests" version))
             (sha256
              (base32
               "1qfidaynsrci4wymrw3srz8v1zy7xxpcna8sxpm91mwqixsmlb4l"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:test-flags
           ;; These require internet access
           '(list "--ignore=tests/test_requests.py"
                  "-k" (string-append "not test_use_proxy_from_environment"
                                      " and not requests.sessions.Session"
                                      " and not requests.models.PreparedRequest"
                                      " and not requests.api.request")
                  "tests/")))
    (propagated-inputs
     (list python-certifi
           python-charset-normalizer
           python-idna
           python-urllib3))
    (native-inputs
     (list python-pip
           python-pytest
           python-pytest-mock
           python-setuptools
           python-wheel))
    (home-page "https://requests.readthedocs.io/")
    (synopsis "Python HTTP library")
    (description
     "Requests is a Python HTTP client library.  It aims to be easier to use
than Python’s urllib2 library.")
    (license license:asl2.0)))

(define-public python-requests-next
  (package
    (inherit python-requests)
    (name "python-requests")
    (version "2.32.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "requests" version))
       (sha256
        (base32 "0q5742pnibwy74169kacin3dmqg9jzmzk7qab5aq5caffcbm8djm"))))
     (native-inputs
      (modify-inputs (package-native-inputs python-requests)
        (prepend nss-certs-for-test)))))

(define-public python-requests-kerberos
  (package
    (name "python-requests-kerberos")
    (version "0.14.0")
    ;; No tests in the PyPI tarball.
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/requests/requests-kerberos")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0s30pcnlir3j2jmf7yh065f294cf3x0x5i3ldskn8mm0a3657mv3"))))
    (build-system pyproject-build-system)
     (propagated-inputs (list python-cryptography
                              python-pyspnego
                              python-requests))
    (native-inputs (list python-pytest python-pytest-mock python-setuptools
                         python-wheel))
    (home-page "https://github.com/requests/requests-kerberos")
    (synopsis "Kerberos authentication handler for python-requests")
    (description "This package provides a Kerberos authentication handler for
python-requests.")
    (license license:isc)))

(define-public python-requests-unixsocket2
  (package
    (name "python-requests-unixsocket2")
    (version "0.4.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/thelabnyc/requests-unixsocket2")
             (commit (string-append "v" version))))
       (sha256
        (base32 "0vy0c1xwwmm6xqabhl2j7zqgsldvcs8ar547nk5r9l2yb7gngzjk"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-poetry-core python-pytest python-waitress))
    (propagated-inputs
     (list python-pbr python-requests))
    (home-page "https://gitlab.com/thelabnyc/requests-unixsocket2")
    (synopsis "Talk HTTP via a UNIX domain socket")
    (description
     "This Python package lets you use the @code{requests} library to talk
HTTP via a UNIX domain socket.")
    (license license:asl2.0)))

(define-public python-requests-unixsocket
  (deprecated-package "python-requests-unixsocket" python-requests-unixsocket2))

(define-public python-requests_ntlm
  (package
    (name "python-requests_ntlm")
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "requests_ntlm" version))
       (sha256
        (base32
         "1a0np7lk8ma1plv1s4aw5q9h2z3aljprkl9qsfypqcaf0zsqbhik"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-cryptography python-pyspnego python-requests))
    (home-page "https://github.com/requests/requests-ntlm")
    (synopsis
     "NTLM authentication support for Requests")
    (description
     "This package allows for HTTP NTLM authentication using the requests
library.")
    (license license:isc)))

(define-public python-requests-mock
  (package
    (name "python-requests-mock")
    (version "1.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "requests-mock" version))
       (sha256
        (base32
         "09nj8fmyj7xz2mgwyvbw0fl9zybmx2d3qd2hf529vvjc9s24d3z6"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-requests python-six))
    (native-inputs
     (list python-pbr
           python-discover
           python-docutils
           python-fixtures
           python-mock
           python-purl
           python-pytest
           python-sphinx
           python-testrepository))
    (home-page "https://requests-mock.readthedocs.org/")
    (synopsis "Mock out responses from the requests package")
    (description
      "This module provides a building block to stub out the HTTP requests
portions of your testing code.")
    (license license:asl2.0)))

(define-public python-requests-toolbelt
  (package
    (name "python-requests-toolbelt")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "requests-toolbelt" version))
       (sha256
        (base32 "1ijvip427ki177ycrblcn1mfgsq7ixzpvqqfvidjn0a7s2is10bn"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      ;; Requiring networking or fail due not valid certs: AttributeError:
      ;; 'MockHTTPResponse' object has no attribute 'close'
      #~(list "-k" (string-append "not test_dump_all"
                                  " and not test_dump_response"
                                  " and not test_prepared_request_override_base"
                                  " and not test_prepared_request_with_base"
                                  " and not test_request_override_base"
                                  " and not test_request_with_base"))))
    (native-inputs
     (list python-betamax
           python-mock
           python-pyopenssl
           python-pytest
           python-setuptools
           python-trustme
           python-wheel))
    (propagated-inputs
     (list python-requests))
    (synopsis "Extensions to python-requests")
    (description "This is a toolbelt of useful classes and functions to be used
with python-requests.")
    (home-page "https://github.com/requests/toolbelt/")
    (license license:asl2.0)))

(define-public python-requests-ftp
  (package
    (name "python-requests-ftp")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "requests-ftp" version))
       (sha256
        (base32
         "0yh5v21v36dsjsgv4y9dx4mmz35741l5jf6pbq9w19d8rfsww13m"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-requests))
    (home-page
     "https://github.com/Lukasa/requests-ftp")
    (synopsis "FTP Transport Adapter for Requests")
    (description
     "Requests-FTP is an implementation of a simple FTP transport
adapter for use with the Requests library.")
    (license license:asl2.0)))

(define-public python-aioftp
  (package
    (name "python-aioftp")
    (version "0.22.3")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "aioftp" version))
              (sha256
               (base32
                "0w621mg956m9rn7v39jpwi4gpnpl90pprwl29cp640dahqqv38ms"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-async-timeout
           python-pytest
           python-pytest-asyncio
           python-pytest-cov
           python-setuptools
           python-trustme
           python-wheel))
    (propagated-inputs (list python-siosocks))
    (home-page "https://aioftp.readthedocs.io/")
    (synopsis "FTP client/server for asyncio in Python")
    (description
     "FTP client and server for asyncio (Python 3) Library implementing FTP
protocol, both client and server for Python asyncio module.

 Supported commands as client: USER, PASS, ACCT, PWD, CWD, CDUP, MKD, RMD,
 MLSD, MLST, RNFR, RNTO, DELE, STOR, APPE, RETR, TYPE, PASV, ABOR, QUIT,
 REST, LIST (as fallback).

 Supported commands as server: USER, PASS, QUIT, PWD, CWD, CDUP, MKD, RMD,
 MLSD, LIST (non-standard), MLST, RNFR, RNTO, DELE, STOR, RETR,
 TYPE (\"I\" and \"A\"), PASV, ABOR, APPE, REST.")
    (license license:asl2.0)))

(define-public python-mohawk
  (package
    (name "python-mohawk")
    (version "1.1.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "mohawk" version))
              (sha256
               (base32
                "08wppsv65yd0gdxy5zwq37yp6jmxakfz4a2yx5wwq2d222my786j"))))
    (build-system python-build-system)
    (native-inputs (list python-mock  python-nose))
    (propagated-inputs (list python-six))
    (home-page "https://github.com/kumar303/mohawk")
    (synopsis "Library for Hawk HTTP authorization")
    (description
     "Mohawk is an alternate Python implementation of the Hawk HTTP
authorization scheme.")
    (license license:bsd-3)))

(define-public python-msal
  (package
    (name "python-msal")
    (version "1.31.1")
    (home-page
     "https://github.com/AzureAD/microsoft-authentication-library-for-python")
    (source (origin
              (method git-fetch)
              ;; Pypi does not have tests.
              (uri (git-reference (url home-page) (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "18dg1j2az5ywk6klfd3kp36fxa4cmmf9yvma4li0a2nz2jgc1gdd"))))
    (build-system python-build-system)
    (arguments
     ;; Tests (all?) rely on network access and only some can be disabled by
     ;; setting the environment variable TRAVIS_TAG.
     (list #:tests? #f))
    (native-inputs (list python-mock))
    (propagated-inputs (list python-cryptography python-pyjwt python-requests))
    (synopsis "Microsoft Authentication Library (MSAL) for Python")
    (description
     "The Microsoft Authentication Library for Python enables applications to
integrate with the Microsoft identity platform.  It allows you to sign in
users or apps with Microsoft identities (Azure AD, Microsoft Accounts and
Azure AD B2C accounts) and obtain tokens to call Microsoft APIs such as
Microsoft Graph or your own APIs registered with the Microsoft identity
platform.  It is built using industry standard OAuth2 and OpenID Connect
protocols.")
    (license license:expat)))

(define-public oauth2ms
  (let ((commit "a1ef0cabfdea57e9309095954b90134604e21c08")
        (revision "0"))
    (package
      (name "oauth2ms")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/harishkrupo/oauth2ms")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0dqi6n4npdrvb42r672n4sl1jl8z5lsk554fwiiihpj0faa9dx64"))))
      (build-system copy-build-system)
      (arguments
       (list #:install-plan #~`(("oauth2ms" "bin/oauth2ms")
                                ("." #$(string-append "share/doc/" name "-"
                                                      version "/")
                                 #:include-regexp ("\\.org$")))
             #:phases #~(modify-phases %standard-phases
                          (add-after 'install 'wrap-pythonpath
                            (lambda* (#:key inputs outputs #:allow-other-keys)
                              (let ((path (getenv "GUIX_PYTHONPATH")))
                                (wrap-program (string-append #$output
                                                             "/bin/oauth2ms")
                                              `("GUIX_PYTHONPATH" ":" prefix
                                                (,path)))))))))
      (inputs (list bash-minimal python python-gnupg python-msal python-pyxdg))
      (home-page "https://github.com/harishkrupo/oauth2ms")
      (synopsis "XOAUTH2 compatible Microsoft Office 365 token fetcher")
      (description
       "Oauth2ms can be used to fetch OAuth 2.0 tokens from the Microsoft Identity
endpoint.  Additionally, it can encode the token in the XOAUTH2 format to be
used as authentication in IMAP mail servers.")
      (license license:asl2.0))))

(define-public python-oauthlib
  (package
    (name "python-oauthlib")
    (version "3.1.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "oauthlib" version))
              (sha256
               (base32
                "12gqnabwck30gdlpwm6af3s28qm9p2yc7b1w8s4fk9ncbz1irr5y"))))
    (build-system python-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (replace 'check
                    (lambda _
                      (invoke "pytest" "-vv"))))))
    (native-inputs
     (list python-pytest python-pytest-cov python-mock))
    (propagated-inputs
     (list python-cryptography python-pyjwt python-blinker))
    (home-page "https://github.com/oauthlib/oauthlib")
    (synopsis "OAuth implementation for Python")
    (description
     "Oauthlib is a generic, spec-compliant, thorough implementation of the
OAuth request-signing logic.")
    (license license:bsd-3)))

(define-public python-rauth
  (package
    (name "python-rauth")
    (version "0.7.3")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "rauth" version))
        (sha256
         (base32
          "02kv8w8l98ky223avyq7vw7x1f2ya9chrm59r77ylq45qb0xnk2j"))))
    (build-system python-build-system)
    (arguments
     `(#:test-target "check"))
    (propagated-inputs
     (list python-requests))
    (home-page "https://github.com/litl/rauth")
    (synopsis "Python library for OAuth 1.0/a, 2.0, and Ofly")
    (description
     "Rauth is a Python library for OAuth 1.0/a, 2.0, and Ofly.  It also
provides service wrappers for convenient connection initialization and
authenticated session objects providing things like keep-alive.")
    (license license:expat)))

(define-public python-unalix
  (package
    (name "python-unalix")
    (version "0.9")
    (source
     (origin
       ;; There's only a wheel on PyPI.
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/AmanoTeam/Unalix")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0h8wc1axv26h753a8brc6dccqndx005c2bhr09gwg5c1hj9zsfml"))))
    (build-system python-build-system)
    (native-inputs (list python-pytest))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "pytest")))))))
    (home-page "https://pypi.org/project/Unalix/")
    (synopsis "Python library for removing tracking fields from URLs")
    (description "Unalix is a library written in Python implementing the
specification used by the @url{https://github.com/ClearURLs/Addon, ClearURLs}
addon for removing tracking fields from URLs.")
    (license license:lgpl3+)))

(define-public python-urllib3
  (package
    (name "python-urllib3")
    (version "2.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "urllib3" version))
       (sha256
        (base32
         "06fg8jj64lgxr2jrizzrsdvaf1avpg5qmhvnswhfbf8sqrv0hmyh"))))
    (build-system pyproject-build-system)
    (arguments `(#:tests? #f))
    (native-inputs (list python-hatchling))
    (propagated-inputs
     (append
      ;; These 5 inputs are used to build urrlib3[secure]
      (list python-certifi)
      (if (member (%current-system)
                  (package-transitive-supported-systems python-cryptography))
          (list python-cryptography)
          '())
      (list python-idna)
      (if (member (%current-system)
                  (package-transitive-supported-systems python-pyopenssl))
          (list python-pyopenssl)
          '())
      (list python-pysocks)))
    (home-page "https://urllib3.readthedocs.io/")
    (synopsis "HTTP library with thread-safe connection pooling")
    (description
     "Urllib3 supports features left out of urllib and urllib2 libraries.  It
can reuse the same socket connection for multiple requests, it can POST files,
supports url redirection and retries, and also gzip and deflate decoding.")
    (license license:expat)))

(define-public python-urllib3-next
  (package
    (name "python-urllib3")
    (version "1.26.17")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "urllib3" version))
        (sha256
         (base32
          "08fzhaf77kbjj5abpl9xag6fpfxkdp1k5s7sqd3ayacdq91a5mi4"))))
    (build-system python-build-system)
    (arguments `(#:tests? #f))
    (propagated-inputs
     (append
       ;; These 5 inputs are used to build urrlib3[secure]
       (list python-certifi)
       (if (member (%current-system)
                   (package-transitive-supported-systems python-cryptography))
         (list python-cryptography)
         '())
       (list python-idna)
       (if (member (%current-system)
                   (package-transitive-supported-systems python-pyopenssl))
         (list python-pyopenssl)
         '())
       (list python-pysocks)))
    (home-page "https://urllib3.readthedocs.io/")
    (synopsis "HTTP library with thread-safe connection pooling")
    (description
     "Urllib3 supports features left out of urllib and urllib2 libraries.  It
can reuse the same socket connection for multiple requests, it can POST files,
supports url redirection and retries, and also gzip and deflate decoding.")
    (license license:expat)))

(define-public python-awscrt
  (package
    (name "python-awscrt")
    (version "0.23.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "awscrt" version))
       (sha256
        (base32 "0a669xxfmgw3g6xpcnm64pbmlrbxw5wf3jcrivixscl2glapdxgx"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'disable-broken-tests
            (lambda _
              ;; Disable broken tests.  These tests fail because the
              ;; certificate bundle at the default location does not exist.
              (substitute* "test/test_auth.py"
                (("def test_default_provider")
                 "def _test_default_provider"))
              (substitute* "test/test_http_client.py"
                (("def test_h2_client")
                 "def _test_h2_client"))
              (substitute* "test/test_s3.py"
                (("def test_sanity")
                 "def _test_sanity")
                (("def test_sanity_secure")
                 "def _test_sanity_secure")
                (("def test_wait_shutdown")
                 "def _test_wait_shutdown"))))
          (add-after 'unpack 'override-cert-bundle-location
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((bundle (search-input-file inputs
                                               "/etc/ssl/certs/ca-certificates.crt")))
                (setenv "SSL_CERT_FILE" bundle)
                (substitute* "awscrt/io.py"
                  (("( +)opt = TlsContextOptions\\(\\)" m indent)
                   (string-append m "\n"
                                  indent "import os\n"
                                  indent "\
opt.override_default_trust_store_from_path(None, os.getenv('SSL_CERT_FILE')) if os.getenv('SSL_CERT_FILE') else None\n")))
                (substitute* "test/appexit_http.py"
                  (("( +)tls_ctx_opt = awscrt.io.TlsContextOptions.*" m indent)
                   (string-append m indent
                                  "tls_ctx_opt.override_default_trust_store_from_path(None, '"
                                  bundle "')\n")))
                (substitute* "test/test_io.py"
                  (("( +)opt = TlsContextOptions\\(\\).*" m indent)
                   (string-append m indent
                                  "opt.override_default_trust_store_from_path(None, '"
                                  bundle "')\n"))))))
          (add-after 'unpack 'use-system-libraries
            (lambda _
              (setenv "AWS_CRT_BUILD_USE_SYSTEM_LIBCRYPTO" "1")))
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (invoke "python3" "-m" "unittest"
                        "discover" "--verbose")))))))
    (inputs (list openssl))
    (native-inputs (list cmake-minimal
                         python-setuptools
                         python-wheel
                         ;; For tests only
                         nss-certs-for-test
                         python-boto3
                         python-websockets))
    (home-page "https://github.com/awslabs/aws-crt-python")
    (synopsis "Common runtime for AWS Python projects")
    (description
     "This package provides a common runtime for AWS Python projects.")
    (license license:asl2.0)))

(define-public python-awscrt-for-awscli
  (package
    (inherit python-awscrt)
    (version "0.22.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "awscrt" version))
       (sha256
        (base32 "0w6pw42jbznrxh92cd97p96dg2nz698mcbfy7md3zw18jfsb18jc"))))))

(define-public awscli
  (package
    ;; Note: updating awscli typically requires updating botocore as well.
    (name "awscli")
    (version "1.36.32")
    (source
     (origin
       (method git-fetch)               ; no tests in PyPI release
       (uri (git-reference
             (url "https://github.com/aws/aws-cli")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1b4bfv7mgrxw7nfbv9ag97qcsqch2p7raip6111rqg5vdhvsck19"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list "--numprocesses" (number->string (parallel-job-count))
              ;; Tests require networking.
              "--ignore" "tests/integration"
              ;; It struggles to set PYTHONPATH.
              ;;
              ;; AssertionError: 'argument operation: Invalid choice, valid
              ;; choices are:' not found in '
              "-k"
              (string-append "not test_subscribe_to_shard_removed"
                             " and not test_start_conversation_removed"
                             ;; Tests fail during mocking.
                             " and not test_no_groff_or_mandoc_exists"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-reference-to-groff
            (lambda _
              ;; XXX: Consider to use wrap-program instead, it tries to parse
              ;; the PATH.
              (substitute* "awscli/help.py"
                (("if self._exists_on_path\\('groff'\\):") "if 'groff':")
                (("raise ExecutableNotFoundError\\('groff'\\)") "")
                (("cmdline = \\['groff'")
                 (format #f "cmdline = ['~a/bin/groff'"
                         #$(this-package-input "groff-minimal"))))))
          (add-before 'check 'set-environment
            (lambda _
              ;; PermissionError: [Errno 13] Permission denied:
              ;; '/homeless-shelter'
              (setenv "HOME" "/tmp"))))))
    (native-inputs
     (list python-pytest
           python-pytest-xdist
           python-setuptools
           python-wheel))
    (inputs
     (list groff-minimal
           python-botocore
           python-colorama
           python-docutils-0.15
           python-pyyaml
           python-rsa
           python-s3transfer))
    (home-page "https://aws.amazon.com/cli/")
    (synopsis "Command line client for AWS")
    (description
     "AWS CLI provides a unified command line interface to the Amazon Web
Services (AWS) API.")
    (license license:asl2.0)))

(define-public awscli-2
  (package
    (inherit awscli)
    (name "awscli")
    (version "2.20.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/aws/aws-cli")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0hyr9gmcfk7nzkgs0v6wgkh8k15dyhknqzfymbc9a9sa2dblc40q"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      ;; The resource leak tests use ps to check for memory consumption.
      '(list "--ignore=functional/botocore/leak/test_resource_leaks.py"
             ;; These tests complain about unavailable TLS certs.
             "--ignore=functional/ec2instanceconnect/test_opentunnel.py"
             ;; These seem to require Internet access.
             "--ignore=unit/botocore/test_awsrequest.py")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'ignore-deprecations
            (lambda _
              (substitute* "pyproject.toml"
                (("\"error::") "\"ignore::"))))
          (add-after 'unpack 'fix-reference-to-groff
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "awscli/help.py"
                (("if self._exists_on_path\\('groff'\\):") "if True:")
                (("cmdline = \\['groff'")
                 (string-append "cmdline = ['"
                                (search-input-file inputs "bin/groff")
                                "'")))))
          (replace 'check
            (lambda* (#:key tests? test-flags #:allow-other-keys)
              (when tests?
                (let ((skip-args
                       (string-append
                        "-k" "\""
                        ;; No idea why this fails.
                        "not test_no_groff_or_mandoc_exists"
                        ;; Needs $HOME
                        " and not test_attach_history_handler"
                        ;; Complains about TLS certs.
                        " and not test_command_returns_shutdown_exception"
                        "\"")))
                  (substitute* "scripts/ci/run-tests"
                    (("--numprocesses=auto" m)
                     (string-join (cons* m skip-args test-flags) " "))))
                (invoke "python" "scripts/ci/run-tests")))))))
    (inputs
     (list groff
           nss-certs-for-test
           python-awscrt-for-awscli
           python-colorama
           python-botocore
           python-cryptography
           python-dateutil
           python-docutils
           python-jmespath
           python-jsonschema
           python-prompt-toolkit
           python-ruamel.yaml-0.16
           python-ruamel.yaml.clib
           python-urllib3))
    (native-inputs
     (list python-distro
           python-flit
           python-pytest
           python-pytest-xdist
           python-wheel))))

;; This is not an official release of awscli version 2, so it should not be
;; named awscli.
(define-public awscliv2
  (package
    (inherit awscli)
    (name "awscliv2")
    (version "2.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri name version))
       (sha256
        (base32
         "1bpp6kmb75qdhgzsx69ki04345bfkzwnmg84y5x6nyfpph2g3fsz"))))
    (arguments
     ;; FIXME: The 'pypi' release does not contain tests.
     '(#:tests? #f))
    (inputs
     (list python-importlib-resources
           python-executor))
    (native-inputs
     (list python-poetry-core
           python-pytest))))


(define-public python-wsgiproxy2
  (package
    (name "python-wsgiproxy2")
    (version "0.4.6")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "WSGIProxy2" version ".tar.gz"))
       (sha256
        (base32 "16jch5nic0hia28lps3c678s9s9mjdq8n87igxncjg0rpi5adqnf"))))
    (build-system python-build-system)
    (native-inputs
     (list python-webtest))
    (propagated-inputs
     (list python-requests python-six python-urllib3 python-webob))
    (home-page "https://github.com/gawel/WSGIProxy2/")
    (synopsis "WSGI Proxy with various http client backends")
    (description "WSGI turns HTTP requests into WSGI function calls.
WSGIProxy turns WSGI function calls into HTTP requests.
It also includes code to sign requests and pass private data,
and to spawn subprocesses to handle requests.")
    (license license:expat)))

(define-public python-pastedeploy
  (package
    (name "python-pastedeploy")
    (version "2.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "PasteDeploy" version))
       (sha256
        (base32 "05s88qdjdwd9d9qs13fap7nqgxs7qs5qfzzjbrc5va13k2mxdskd"))))
    (build-system python-build-system)
    (arguments
     '(#:test-target "pytest"))
    (native-inputs
     (list python-pytest python-pytest-runner))
    (home-page "https://pylonsproject.org/")
    (synopsis
     "Load, configure, and compose WSGI applications and servers")
    (description
     "This tool provides code to load WSGI applications and servers from URIs;
these URIs can refer to Python Eggs for INI-style configuration files.  Paste
Script provides commands to serve applications based on this configuration
file.")
    (license license:expat)))

(define-public python-webtest
  (package
    (name "python-webtest")
    (version "2.0.35")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "WebTest" version))
       (sha256
        (base32 "11xhgdj251zkvz5w30fvspii08ki2vrpr1im9sph1wmlnasnihda"))))
    (build-system python-build-system)
    (arguments
     ;; Tests require python-pyquery, which creates a circular dependency.
     `(#:tests? #f))
    (propagated-inputs
     (list python-waitress python-webob python-six python-beautifulsoup4))
    (home-page "https://docs.pylonsproject.org/projects/webtest/")
    (synopsis "Helper to test WSGI applications")
    (description "Webtest allows you to test your Python web applications
without starting an HTTP server.  It supports anything that supports the
minimum of WSGI.")
    (license license:expat)))

(define-public python-flask
  (package
    (name "python-flask")
    (version "3.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "flask" version))
       (sha256
        (base32 "1b1nb250jxrjplbs21wnwq69250vs3iz2pdhs7cwi5y8hi8kr1sz"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-flit-core
           python-pytest))
    (propagated-inputs
     (list python-asgiref               ;async extra
           python-click
           python-blinker
           python-importlib-metadata
           python-itsdangerous
           python-jinja2
           python-werkzeug))
    (home-page "https://palletsprojects.com/p/flask/")
    (synopsis "Microframework based on Werkzeug, Jinja2 and good intentions")
    (description "Flask is a micro web framework based on the Werkzeug toolkit
and Jinja2 template engine.  It is called a micro framework because it does not
presume or force a developer to use a particular tool or library.")
    (license license:bsd-3)))

(define-public python-flask-compress
  (package
    (name "python-flask-compress")
    (version "1.13")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "Flask-Compress" version))
              (sha256
               (base32
                "178jzz6jxlxllcjqamzh5q7ahfh90m5cl1il9vmjs3xhz65z35pf"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-brotli python-flask))
    (native-inputs (list python-setuptools-scm python-setuptools python-wheel))
    (home-page "https://github.com/colour-science/flask-compress")
    (synopsis "Compress responses in a Flask app")
    (description
     "This package lets you compress Flask application responses and static
files with gzip, deflate or brotli.  Flask-Compress both adds the various
headers required for a compressed response and compresses the response data.")
    (license license:expat)))

(define-public python-flask-seasurf
  (package
    (name "python-flask-seasurf")
    (version "1.1.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "Flask-SeaSurf" version))
              (sha256
               (base32
                "1aaib4n27q0f2alp87mhv3f79vg7qckp71rphbd0mb39qw470lsl"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:tests? #false)) ;there are none
    (propagated-inputs (list python-flask))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://github.com/maxcountryman/flask-seasurf/")
    (synopsis "CSRF extension for Flask")
    (description "SeaSurf is a Flask extension for preventing cross-site
request forgery (CSRF).  CSRF attacks are problematic because the mechanism
they use is relatively easy to exploit.  This extension attempts to aid you in
securing your application from such attacks.  This extension is based on the
Django middleware.")
    (license license:bsd-3)))

(define-public python-flask-wtf
  (package
    (name "python-flask-wtf")
    (version "1.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "Flask-WTF" version))
       (sha256
        (base32
         "0sv6hjva1fdizb7sr46230pplx8qz8rnb4bxrfy71xbc8cgfxlkr"))))
    (build-system pyproject-build-system)
    (propagated-inputs
     (list python-flask python-flask python-itsdangerous python-wtforms))
    (native-inputs
     (list python-hatchling python-pytest))
    (home-page "https://github.com/lepture/flask-wtf")
    (synopsis "Simple integration of Flask and WTForms")
    (description "Flask-WTF integrates Flask and WTForms, including CSRF, file
upload, and reCAPTCHA.")
    (license license:bsd-3)))

(define-public python-flask-multistatic
  (package
    (name "python-flask-multistatic")
    (version "1.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "flask-multistatic" version))
       (sha256
        (base32
         "0p4v50rwv64wcd0zlq7rzl4waprwr4hj19s3cgf1isywa7jcisgm"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-flask))
    (home-page "https://pagure.io/flask-multistatic")
    (synopsis "Flask plugin to allow overriding static files")
    (description "@code{flask-multistatic} is a flask plugin that adds support
for overriding static files.")
    (license license:gpl3+)))

(define-public python-cookies
  (package
    (name "python-cookies")
    (version "2.2.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "cookies" version))
              (sha256
               (base32
                "13pfndz8vbk4p2a44cfbjsypjarkrall71pgc97glk5fiiw9idnn"))))
    (build-system python-build-system)
    (arguments
     `(;; test are broken: https://gitlab.com/sashahart/cookies/issues/3
       #:tests? #f))
    (native-inputs
     (list python-pytest))
    (synopsis "HTTP cookie parser and renderer")
    (description "A RFC 6265-compliant HTTP cookie parser and renderer in
Python.")
    (home-page "https://gitlab.com/sashahart/cookies")
    (license license:expat)))

(define-public python-responses
  (package
    (name "python-responses")
    (version "0.25.3")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "responses" version))
              (sha256
               (base32
                "1fld6lsa143md4lxccwxq9iyqm9d8a05i9sp7lqjibnrmd3r4yv1"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-pytest
           python-pytest-asyncio
           python-pytest-httpserver
           python-setuptools
           python-tomli
           python-tomli-w
           python-wheel))
    (propagated-inputs
     (list python-requests
           python-pyyaml
           python-urllib3))
    (home-page "https://github.com/getsentry/responses")
    (synopsis "Utility for mocking out the `requests` Python library")
    (description "A utility library for mocking out the `requests` Python
library.")
    (license license:asl2.0)))

(define-public python-grequests
  (package
    (name "python-grequests")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "grequests" version))
       (sha256
        (base32
         "1j9icncllbkv7x5719b20mx670c6q1jrdx1sakskkarvx3pc8h8g"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-gevent python-requests))
    (native-inputs
     (list python-nose python-zope-interface python-zope-event))
    (home-page "https://github.com/kennethreitz/grequests")
    (synopsis "Python library for asynchronous HTTP requests")
    (description "GRequests is a Python library that allows you to use
@code{Requests} with @code{Gevent} to make asynchronous HTTP Requests easily")
    (license license:bsd-2)))

(define-public python-gwebsockets
  (package
    (name "python-gwebsockets")
    (version "0.7")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "gwebsockets" version))
              (sha256
               (base32
                "0kgq7wssz0mrhxdafkfc9prj0qjv9z0lyivyqvjvjnnypg54di7m"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-pygobject))
    (home-page "https://github.com/sugarlabs/gwebsockets")
    (synopsis "GLib based websockets server")
    (description "This package provides a websocket server written in Python.
It uses GIO for network communication and hence it easily integrates with the
GLib mainloop.")
    (license license:asl2.0)))

(define-public python-dpkt
  (package
    (name "python-dpkt")
    (version "1.9.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "dpkt" version))
       (sha256
        (base32
         "1d28r8pmhzjjd6hrn1xcddinfhwv8lcl1s59ygmqa8kfmz5pkrgl"))))
    (build-system python-build-system)
    (home-page "https://github.com/kbandla/dpkt")
    (synopsis "Packet generator and parser for TCP/IP protocols")
    (description "The dpkt module is a fast, simple packet generator and parser
for the basic TCP/IP protocols.")
    (license license:bsd-3)))

(define-public python-geventhttpclient
  (package
    (name "python-geventhttpclient")
    (version "2.3.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/geventhttpclient/geventhttpclient")
                    (commit version)
                    (recursive? #true)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1ya0i0fbx054mfx5d1k75fcf64xzp7vva8lkwwzan41xbnc56nyj"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      ;; Disable network tests.
      '(list "-k"
             (string-append "not test_client_simple"
                            " and not test_client_ssl"
                            " and not test_client_with_default_headers"
                            " and not test_client_without_leading_slash"
                            " and not test_download"
                            " and not test_fail_invalid_ca_certificate"
                            " and not test_httpbin_multipart"
                            " and not test_multi_queries_greenlet_safe"
                            " and not test_no_form_encode_header"
                            " and not test_no_form_encoded_header"
                            " and not test_request_with_headers"
                            " and not test_response_context_manager"
                            " and not test_urllib_request"))))
    (native-inputs (list nss-certs-for-test
                         python-dpkt python-pytest python-requests
                         python-setuptools python-wheel))
    (propagated-inputs (list python-brotli python-certifi python-gevent
                             python-urllib3))
    (home-page "https://github.com/geventhttpclient/geventhttpclient")
    (synopsis "HTTP client library for gevent")
    (description "@code{python-geventhttpclient} is a high performance,
concurrent HTTP client library for python using @code{gevent}.")
    (license license:expat)))

(define-public python-requests-oauthlib
  (package
    (name "python-requests-oauthlib")
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "requests-oauthlib" version))
       (sha256
        (base32
         "0mrglgcvq7k48pf27s4gifdk0za8xmgpf55jy15yjj471qrk6rdx"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; removes tests that require network access
         (add-before 'check 'pre-check
           (lambda _
             (delete-file "tests/test_core.py")
             #t)))))
    (native-inputs
     (list python-pyjwt python-requests-mock python-mock))
    (propagated-inputs
     (list python-oauthlib python-requests))
    (home-page
     "https://github.com/requests/requests-oauthlib")
    (synopsis
     "OAuthlib authentication support for Requests")
    (description
     "Requests-OAuthlib uses the Python Requests and OAuthlib libraries to
provide an easy-to-use Python interface for building OAuth1 and OAuth2 clients.")
    (license license:isc)))

(define-public python-url
  (package
    (name "python-url")
    (version "0.2.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "url" version))
              (sha256
               (base32
                "0v879yadcz9qxfl41ak6wkga1kimp9cflla9ddz03hjjvgkqy5ki"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-publicsuffix))
    (native-inputs
     (list python-coverage python-nose))
    (arguments
     `(#:tests? #f)) ; FIXME: tests fail with "ImportError: No module named 'tests'"
    (home-page "https://github.com/seomoz/url-py")
    (synopsis "URL Parsing")
    (description "Library for parsing urls.")
    (license license:expat)))

(define-public python-cachecontrol
  (package
    (name "python-cachecontrol")
    (version "0.14.1")
    (source
     (origin
       (method git-fetch)
       ;; Pypi does not have tests.
       (uri (git-reference
             (url "https://github.com/ionrock/cachecontrol")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "07ifs4dhahhsgj4vxgfl5ymnmb4my5l2wb8jiinn6fncszcfmr59"))))
    (build-system pyproject-build-system)
    (arguments
     ;; Versions > 0.11.6 depend on CherryPy for testing.
     ;; It's too much work to package CherryPy for now.
     `(#:tests? #f))
    (propagated-inputs
     (list python-filelock python-lockfile python-msgpack python-requests))
    (native-inputs
     (list python-flit-core))
    (home-page "https://github.com/ionrock/cachecontrol")
    (synopsis "The httplib2 caching algorithms for use with requests")
    (description "CacheControl is a port of the caching algorithms in
@code{httplib2} for use with @code{requests} session objects.")
    (license license:asl2.0)))

(define-public python-cachecontrol-0.11
  (package
    (inherit python-cachecontrol)
    (name "python-cachecontrol")
    (version "0.11.7")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "CacheControl" version))
        (sha256
         (base32
          "07jsfhlbcwgqg6ayz8nznzaqg5rmxqblbzxz1qvg5wc44pcjjy4g"))))
    (native-inputs (list python-setuptools python-wheel))))

(define-public python-betamax
  (package
    (name "python-betamax")
    (version "0.9.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "betamax" version))
        (sha256
         (base32
          "152zil1j4gl1whnldi08zwjr4z2bnlbd061kr3ipjs5wg4b6wcc2"))))
    (build-system python-build-system)
    (arguments
     '(;; Many tests fail because they require networking.
       #:tests? #f))
    (propagated-inputs
     (list python-requests))
    (home-page "https://github.com/sigmavirus24/betamax")
    (synopsis "Record HTTP interactions with python-requests")
    (description "Betamax will record your test suite's HTTP interactions and
replay them during future tests.  It is designed to work with python-requests.")
    (license license:expat)))

(define-public python-betamax-matchers
  (package
    (name "python-betamax-matchers")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "betamax-matchers" version))
       (sha256
        (base32
         "07qpwjyq2i2aqhz5iwghnj4pqr2ys5n45v1vmpcfx9r5mhwrsq43"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-betamax python-requests-toolbelt))
    (home-page "https://github.com/sigmavirus24/betamax_matchers")
    (synopsis "VCR imitation for python-requests")
    (description "@code{betamax-matchers} provides a set of Matchers for
Betamax.")
    (license license:asl2.0)))

(define-public python-betamax-serializers
  (package
    (name "python-betamax-serializers")
    (version "0.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "betamax-serializers" version))
       (sha256
        (base32 "0ja9isbjmzzhxdj69s0kdsvw8nkp073w6an6a4liy5vk3fdl2p1l"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-betamax python-pyyaml))
    (synopsis "Set of third-party serializers for Betamax")
    (description "Betamax-Serializers are an experimental set of Serializers for
Betamax that may possibly end up in the main package.")
    (home-page "https://gitlab.com/betamax/serializers")
    (license license:asl2.0)))

(define-public python-s3fs
  (package
    (name "python-s3fs")
    (version "2024.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "s3fs" version))
       (sha256
        (base32 "0mpxk4xvg0gwzapdh9r3fvnjhi8ymf96f7c756xabk26b67kl3qv"))))
    (build-system pyproject-build-system)
    ;; Many tests fail with "No such file or directory" raised by the HTTP
    ;; client.
    (arguments (list #:tests? #false))
    (propagated-inputs (list python-aiobotocore python-aiohttp python-fsspec))
    (native-inputs
     (list python-flask ;for moto
           python-flask-cors
           python-moto
           python-pytest
           python-pytest-asyncio
           python-setuptools
           python-wheel))
    (home-page "https://github.com/fsspec/s3fs/")
    (synopsis "Convenient filesystem interface over S3")
    (description "This package provides a convenient filesystem interface over
S3.")
    (license license:bsd-3)))

(define-public python-s3transfer
  (package
    (name "python-s3transfer")
    (version "0.10.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "s3transfer" version))
       (sha256
        (base32 "032bjky1q8r5x80mvb0ah60g0zq4snwf0xa4c7779m44mdsfsl2g"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list "--numprocesses" (number->string (parallel-job-count))
              ;; Tests require networking.
              "--ignore" "tests/integration")))
    (native-inputs
     (list python-docutils
           python-mock
           python-nose
           python-pytest
           python-pytest-xdist
           python-setuptools
           python-wheel))
    (propagated-inputs
     (list python-botocore
           python-urllib3))
    (home-page "https://github.com/boto/s3transfer")
    (synopsis "Amazon S3 Transfer Manager")
    (description
     "S3transfer is a Python library for managing Amazon S3 transfers.")
    (license license:asl2.0)))

(define-public python-flask-restful
  (package
    (name "python-flask-restful")
    (version "0.3.10")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "Flask-RESTful" version))
        (sha256
         (base32
          "0dzckjjz3aw8nmisr5g9mnv02s2nqlhblykr9ydqzpr703pz4jpy"))))
    (build-system pyproject-build-system)
    (arguments
     ;; See <https://github.com/flask-restful/flask-restful/issues/980>.
     (list #:tests? #f))
    (propagated-inputs
     (list python-aniso8601 python-flask python-pytz))
    (native-inputs
     (list python-blinker
           python-mock
           python-pytest
           python-setuptools
           python-wheel))
    (home-page "https://www.github.com/flask-restful/flask-restful/")
    (synopsis "Flask module for creating REST APIs")
    (description
     "This package contains a Flask module for creating REST APIs.")
    (license license:bsd-3)))

(define-public python-flask-htpasswd
  (package
    (name "python-flask-htpasswd")
    (version "0.5.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "flask-htpasswd" version))
        (sha256
          (base32
            "19arq4rn5nll0bbnsff1j10blm0ydp8nvs09c56s29kmlgj3r42x"))))
    (build-system pyproject-build-system)
    (arguments (list #:tests? #false)) ;none included
    (propagated-inputs
     (list python-flask python-passlib python-pyjwt python-tox))
    (native-inputs (list python-pytest
                         python-setuptools
                         python-wheel))
    (home-page "https://github.com/carsongee/flask-htpasswd")
    (synopsis "Basic authentication via htpasswd files in Flask applications")
    (description "This package provides Basic authentication via
@file{htpasswd} files and access_token authentication in Flask
applications.")
    (license license:bsd-3)))

(define-public python-flask-sqlalchemy
  (package
    (name "python-flask-sqlalchemy")
    (version "3.1.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "flask_sqlalchemy" version))
              (sha256
               (base32
                "04l3dfivznlpvk9p9f20zdbyxl869k42z2w7glddlbc0h6w8pdp4"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list "-k" (string-append "not test_explicit_table[db2]"
                                  " and not test_explicit_table[db4]"))))
    (native-inputs
     (list python-flit-core
           python-pytest))
    (propagated-inputs
     (list python-flask
           python-sqlalchemy-2))
    (home-page "https://github.com/mitsuhiko/flask-sqlalchemy")
    (synopsis "Module adding SQLAlchemy support to your Flask application")
    (description
     "This package adds SQLAlchemy support to your Flask application.")
    (license license:bsd-3)))

(define-public python-flask-restful-swagger
  (package
    (name "python-flask-restful-swagger")
    (version "0.20.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "flask-restful-swagger" version))
       (sha256
        (base32
         "1p66f98b5zpypnnz56pxpbirchqj6aniw6qyrp8h572l0dn9xlvq"))))
    (build-system python-build-system)
    (arguments '(#:tests? #f))          ;no tests
    (propagated-inputs
     (list python-flask-restful python-six))
    (home-page "https://github.com/rantav/flask-restful-swagger")
    (synopsis "Extract Swagger specs from Flask-Restful projects")
    (description "This package lets you extract Swagger API documentation
specs from your Flask-Restful projects.")
    (license license:expat)))

(define-public python-htmlmin
  (package
    (name "python-htmlmin")
    (version "0.1.12")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "htmlmin" version))
       (sha256
        (base32
         "0y51xhabw6x8jk8k93xl8vznshpz3jb6l28075r5sjip613fzhah"))))
    (arguments
     `(#:tests? #f))                    ; no tests
    (build-system python-build-system)
    (home-page "https://htmlmin.readthedocs.org/en/latest/")
    (synopsis "HTML minifier")
    (description "@code{htmlmin} is an HTML minifier that just works.
It comes with safe defaults and easily configurable options.")
    (license license:bsd-3)))

(define-public python-flask-htmlmin
  (package
    (name "python-flask-htmlmin")
    (version "1.2")
    (source
    (origin
      (method url-fetch)
      (uri (pypi-uri "Flask-HTMLmin" version))
      (sha256
       (base32
        "1n6zlq72kakkw0z2jpq6nh74lfsmxybm4g053pwhc14fbr809348"))))
    (propagated-inputs
     (list python-flask python-htmlmin))
    (build-system python-build-system)
    (home-page "https://github.com/hamidfzm/Flask-HTMLmin")
    (synopsis "HTML response minifier for Flask")
    (description
     "Minify @code{text/html} MIME type responses when using @code{Flask}.")
    (license license:bsd-3)))

(define-public python-jsmin
  (package
    (name "python-jsmin")
    (version "3.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "jsmin" version))
       (sha256
        (base32
         "1z1brjsvni0260bypldkl8a05sgp0qk18x560zl44igr3q99m5f0"))))
    (build-system python-build-system)
    (home-page "https://github.com/tikitu/jsmin/")
    (synopsis "Python JavaScript minifier")
    (description
     "@code{jsmin} is a JavaScript minifier, usable from both Python code and
on the command line.")
    (license license:expat)))

(define-public python-flask-login
  (package
    (name "python-flask-login")
    (version "0.6.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/maxcountryman/flask-login")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0wk3dni5qssmbn8ii3hsh89qa3kwr97i92k27bzgsgxvvsfwwzja"))))
    (build-system pyproject-build-system)
    (propagated-inputs
     (list python-flask))
    (native-inputs
     ;; For tests.
     (list python-blinker
           python-mock
           python-pytest
           python-semantic-version
           python-setuptools
           python-werkzeug
           python-wheel))
    (home-page "https://github.com/maxcountryman/flask-login")
    (synopsis "User session management for Flask")
    (description
     "@code{Flask-Login} provides user session management for Flask.  It
handles the common tasks of logging in, logging out, and remembering your
users' sessions over extended periods of time.")
    (license license:expat)))

(define-public python-oauth2client
  (package
    (name "python-oauth2client")
    (version "4.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "oauth2client" version))
       (sha256
        (base32
         "1irqqap2zibysf8dba8sklfqikia579srd0phm5n754ni0h59gl0"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f))
    (propagated-inputs
     (list python-httplib2
           python-pyasn1
           python-pyasn1-modules
           python-pyparsing
           python-rsa
           python-six))
    (home-page "https://github.com/google/oauth2client/")
    (synopsis "OAuth 2.0 client library")
    (description "@code{python-oauth2client} provides an OAuth 2.0 client
library for Python")
    (license license:asl2.0)))

(define-public python-flask-oidc
  (package
    (name "python-flask-oidc")
    (version "1.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "flask-oidc" version))
       (sha256
        (base32
         "0klgwpn2iy5y7011xh2c8zkryxdwkpxh7qjs3hp5cynl748ia4hc"))))
    (build-system python-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (replace 'check
                 (lambda* (#:key tests? #:allow-other-keys)
                   (when tests?
                     (invoke "nosetests")))))))
    (propagated-inputs
     (list python-flask python-itsdangerous python-oauth2client
           python-six))
    (native-inputs
     (list python-nose python-mock))
    (home-page "https://github.com/puiterwijk/flask-oidc")
    (synopsis "OpenID Connect extension for Flask")
    (description "@code{python-flask-oidc} provides an OpenID Connect extension
for Flask.")
    (license license:bsd-2)))

(define-public python-webassets
  (package
    (name "python-webassets")
    (version "2.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "webassets" version))
       (sha256
        (base32
         "1kc1042jydgk54xpgcp0r1ib4gys91nhy285jzfcxj3pfqrk4w8n"))))
    (build-system python-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (add-before 'check 'adjust-tests
                    (lambda _
                      ;; Fix for Python 3.9 compatibility.
                      (substitute* "tests/test_script.py"
                        (("self\\.t\\.isAlive")
                         "self.t.is_alive"))
                      ;; This test requires 'postcss' and 'babel' which are
                      ;; not yet available in Guix.
                      (delete-file "tests/test_filters.py")
                      ;; These expect pytest, but when pytest is added to the
                      ;; environment more tests fail.
                      (delete-file "tests/test_cache.py")
                      (delete-file "tests/test_bundle_build.py")
                      (delete-file "tests/test_bundle_urls.py")))
                  (replace 'check
                    (lambda _
                      (invoke "nosetests" "-vv"))))))
    (propagated-inputs
     (list python-pyyaml))
    (native-inputs
     (list python-jinja2 python-mock python-nose))
    (home-page "https://github.com/miracle2k/webassets")
    (synopsis "Media asset management")
    (description "Merges, minifies and compresses Javascript and CSS files,
supporting a variety of different filters, including YUI, jsmin, jspacker or
CSS tidy.  Also supports URL rewriting in CSS files.")
    (license license:bsd-2)))

(define-public python-cssmin
  (package
    (name "python-cssmin")
    (version "0.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "cssmin" version))
        (sha256
         (base32
          "1dk723nfm2yf8cp4pj785giqlwv42l0kj8rk40kczvq1hk6g04p0"))))
    (build-system python-build-system)
    (home-page "https://github.com/zacharyvoase/cssmin")
    (synopsis "Python port of the YUI CSS Compressor")
    (description "Python port of the YUI CSS Compressor.")
    (license (list license:expat license:bsd-3))))

(define-public python-elasticsearch
  (package
    (name "python-elasticsearch")
    (version "7.17.12")
    (source
     (origin
       (method git-fetch)               ; no tests in PyPI release
       (uri (git-reference
             (url "https://github.com/elastic/elasticsearch-py")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0v3azgxh1nd0jyqhnb1w28ky3nfx8sjq5vlx2gp33v6vxmvqy1qr"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      ;; Requiring network config.
      #~(list "--ignore=test_elasticsearch/test_connection.py"
              ;; All tests failed.
              "--ignore=test_elasticsearch/test_async")))
    (native-inputs
     (list python-aiohttp
           python-dateutil
           python-mock
           python-pytest
           python-pytest-asyncio
           python-pytest-cov
           python-pyyaml
           python-requests
           python-setuptools
           python-wheel))
    (propagated-inputs
     (list python-certifi
           python-urllib3-next))
    (home-page "https://github.com/elastic/elasticsearch-py")
    (synopsis "Low-level client for Elasticsearch")
    (description "Official low-level client for Elasticsearch.  Its goal is to
provide common ground for all Elasticsearch-related code in Python; because of
this it tries to be opinion-free and very extendable.")
    ;; Apache-2.0 in setup.py and LICENSE file for 11 years.
    (license license:asl2.0)))

(define-public python-engineio
  (package
    (name "python-engineio")
    (version "4.11.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "python_engineio" version))
       (sha256
        (base32
         "0abijzn4srf89jlj0kqcd9xbhyxvglzxkciy5nxv815rrvdb0nql"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; File not found.
      #:test-flags '(list "-k" "not test_static_files")))
    (propagated-inputs
     (list python-aiohttp
           python-requests
           python-simple-websocket
           python-tornado
           python-websocket-client))
    (native-inputs (list python-pytest python-setuptools python-wheel))
    (home-page "https://github.com/miguelgrinberg/python-engineio/")
    (synopsis "Engine.IO server")
    (description "This package provides a Python implementation of the
@code{Engine.IO} realtime client and server.")
    (license license:expat)))

(define-public python-ephemeral-port-reserve
  (package
    (name "python-ephemeral-port-reserve")
    (version "1.1.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "ephemeral_port_reserve" version))
       (sha256
        (base32 "1chl9hil7ggz6l4sfhmp0l2j55qcskbc3pj9360b0309jwndmxxq"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-setuptools
           python-wheel))
    (home-page "https://github.com/Yelp/ephemeral-port-reserve/")
    (synopsis "Find an unused port, reliably")
    (description
     "Bind to an ephemeral port, force it into the TIME_WAIT state, and unbind
it.")
    (license license:expat)))

(define-public python-flask-migrate
  (package
    (name "python-flask-migrate")
    (version "4.1.0")
    (home-page "https://github.com/miguelgrinberg/flask-migrate/")
    (source (origin
              (method git-fetch)
              (uri (git-reference (url home-page)
                                  (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "09qw1hhyssa5gahm199x67d55c55m3k9m23xp2bdacwsiv8jw57g"))))
    (build-system pyproject-build-system)
    (propagated-inputs
     (list python-flask python-alembic python-flask-sqlalchemy))
    (native-inputs
     (list python-pytest python-setuptools python-wheel))
    (synopsis "SQLAlchemy database migrations for Flask programs using
Alembic")
    (description "This package contains SQLAlchemy database migration tools
for Flask programs that are using @code{python-alembic}.")
    (license license:expat)))

(define-public python-genshi
  (package
    (name "python-genshi")
    (version "0.7.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/edgewall/genshi")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "06q14mn5n56rdnqvsbbjixygilblgg85fzw029n96qf6f1rvldiq"))))
    (propagated-inputs
     (list python-six))
    (build-system python-build-system)
    (home-page "https://genshi.edgewall.org/")
    (synopsis "Toolkit for generation of output for the web")
    (description "Genshi is a Python library that provides an integrated set
of components for parsing, generating, and processing HTML, XML or other
textual content for output generation on the web.")
    (license license:bsd-3)))

(define-public python-flask-principal
  (package
    (name "python-flask-principal")
    (version "0.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "Flask-Principal" version))
        (sha256
          (base32
           "0lwlr5smz8vfm5h9a9i7da3q1c24xqc6vm9jdywdpgxfbi5i7mpm"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-blinker))
    (native-inputs
     (list python-flask python-nose))
    (home-page "https://pythonhosted.org/Flask-Principal/")
    (synopsis "Identity management for Flask")
    (description "@code{flask_principal} is a identity management library for
Flask.  It supports managing both authentication and authorization data in a
thread-local variable.")
    (license license:expat)))

(define-public python-flask-httpauth
  (package
    (name "python-flask-httpauth")
    (version "4.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "Flask-HTTPAuth" version))
       (sha256
        (base32
         "0yh9fn6pak64has89pcy03f1cn198vkil872y5jjr53kph2qlmk6"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-flask))
    (native-inputs
     (list nss-certs-for-test
           python-pytest
           python-setuptools
           python-wheel))
    (home-page "https://github.com/miguelgrinberg/flask-httpauth/")
    (synopsis "Basic and Digest HTTP authentication for Flask routes")
    (description "@code{flask_httpauth} provides Basic and Digest HTTP
authentication for Flask routes.")
    (license license:expat)))

(define-public python-uritemplate
  (package
    (name "python-uritemplate")
    (version "4.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "uritemplate" version))
       (sha256
        (base32
         "1w14a775d92mx9pdhb5zimifpfr2lfcn0vfdpjagcy9vbkyfsij3"))))
    (build-system python-build-system)
    (home-page "https://uritemplate.readthedocs.org")
    (synopsis "Library to deal with URI Templates")
    (description "@code{uritemplate} provides Python library to deal with URI
Templates.")
    ;; The software is made available under the terms of *either* of the
    ;; licenses found in LICENSE.APACHE or LICENSE.BSD.  Contributions
    ;; are made under *both* licenses (excerpt from the LICENSE file).
    (license (list license:bsd-2 license:asl2.0))))

(define-public python-uritemplate-3
  (package
    (inherit python-uritemplate)
    (name "python-uritemplate")
    (version "3.0.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "uritemplate" version))
              (sha256
               (base32 "1bkwmgr0ia9gcn4bszs2xlvml79f0bi2s4a87xg22ky9rq8avy2s"))))))

(define-public python-publicsuffix
  (package
    (name "python-publicsuffix")
    (version "1.1.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "publicsuffix" version))
              (sha256
               (base32
                "1adx520249z2cy7ykwjr1k190mn2888wqn9jf8qm27ly4qymjxxf"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f)) ; tests use the internet
    (home-page "https://www.tablix.org/~avian/git/publicsuffix.git")
    (synopsis "Get suffix for a domain name")
    (description "Get a public suffix for a domain name using the Public Suffix
List.")
    (license license:expat)))

(define-public python-publicsuffix2
  (package
    (name "python-publicsuffix2")
    (version "2.20191221")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "publicsuffix2" version))
       (sha256
        (base32 "0yzysvfj1najr1mb4pcqrbmjir3xpb69rlffln95a3cdm8qwry00"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'ignore-maintainer-inputs
           (lambda _
             ;; Comment out a demand for python-requests, which is used only by
             ;; the unused ‘update_psl’ helper command.
             (substitute* "setup.py"
               (("'requests " match)
                (format #f "# ~a" match)))
             #t)))
       #:tests? #f))                  ; the test suite requires network access
    (home-page "https://github.com/pombredanne/python-publicsuffix2")
    (synopsis "Get a public suffix for a domain name using the Public Suffix List")
    (description "Get a public suffix for a domain name using the Public Suffix
List.  Forked from and using the same API as the publicsuffix package.")
    (license (list license:expat license:mpl2.0))))

(define-public python-werkzeug
  (package
    (name "python-werkzeug")
    (version "3.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "werkzeug" version))
       (sha256
        (base32
         "0ij7si3aa3ykac7k7fz481h4majqqwn2iqwhjxkji4y18plkqwk0"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      ;; Test requiring networking setup.
      #~(list "--ignore=tests/test_serving.py"
              "--deselect=tests/test_debug.py::test_basic"
              "--deselect=tests/test_exceptions.py::test_response_body"
              "--deselect=tests/middleware/test_http_proxy.py::test_http_proxy")))
    (native-inputs
     (list python-flit-core
           python-pytest
           python-watchdog
           python-ephemeral-port-reserve
           python-pytest-timeout
           python-pytest-xprocess))
    (propagated-inputs
     (list python-markupsafe
           python-requests))
    (home-page "https://palletsprojects.com/p/werkzeug/")
    (synopsis "Utilities for WSGI applications")
    (description "One of the most advanced WSGI utility modules.  It includes a
powerful debugger, full-featured request and response objects, HTTP utilities to
handle entity tags, cache control headers, HTTP dates, cookie handling, file
uploads, a powerful URL routing system and a bunch of community-contributed
addon modules.")
    (license license:x11)))

(define-public python-werkzeug-1.0
  (package
    (inherit python-werkzeug)
    (version "1.0.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "Werkzeug" version))
              (sha256
               (base32
                "0z74sa1xw5h20yin9faj0vvdbq713cgbj84klc72jr9nmpjv303c"))
              (patches (search-patches "python-werkzeug-tests.patch"))))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (delete 'check)
         (add-after 'install 'check
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (add-installed-pythonpath inputs outputs)
             (invoke "python" "-m" "pytest"))))))
    (propagated-inputs
     (list python-requests))
    (native-inputs
     (list python-pytest python-pytest-timeout python-setuptools
           python-wheel))))

(define-public python-bottle
  (package
    (name "python-bottle")
    (version "0.12.23")
    (source
     (origin
      (method url-fetch)
      (uri (pypi-uri "bottle" version))
      (sha256
        (base32 "0rs1w293gp5bhxip2cci8mfkhld6n5qcznvlna3nxclz76mf6gb8"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (with-directory-excursion "test"
                 (invoke "pytest" "-vvv"))))))))
    (native-inputs (list python-pytest))
    (home-page "https://bottlepy.org/")
    (synopsis "WSGI framework for small web-applications")
    (description "@code{python-bottle} is a WSGI framework for small web-applications.")
    (license license:expat)))

(define-public python-wtforms
  (package
    (name "python-wtforms")
    (version "3.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "wtforms" version))
       (sha256
        (base32 "1fblnkzvs6339glwx8bskdjy7nhn2ap90y9g6b399713sy063mzq"))))
    (build-system pyproject-build-system)
    (native-inputs (list python-hatchling python-pytest))
    (propagated-inputs (list python-babel python-email-validator
                             python-markupsafe))
    (home-page "https://wtforms.readthedocs.io/")
    (synopsis
     "Form validation and rendering library for Python web development")
    (description
     "WTForms is a flexible forms validation and rendering library
for Python web development.  It is very similar to the web form API
available in Django, but is a standalone package.")
    (license license:bsd-3)))

(define-public python-wtforms-sqlalchemy
  (package
    (name "python-wtforms-sqlalchemy")
    (version "0.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "WTForms-SQLAlchemy" version))
       (sha256
        (base32 "1nx4x0ifanlbrzh3f9ns8ihnchlkzf54ilvqmgcgcz2j72vm43rp"))))
    (build-system pyproject-build-system)
    (native-inputs (list python-pytest python-setuptools python-wheel))
    (propagated-inputs (list python-sqlalchemy python-wtforms))
    (home-page "https://github.com/wtforms/wtforms-sqlalchemy/")
    (synopsis "SQLAlchemy tools for WTForms")
    (description
     "WTForms-SQLAlchemy is a fork of the @code{wtforms.ext.sqlalchemy}
package from WTForms.  The package has been renamed to
@code{wtforms_sqlalchemy} but otherwise should function the same as
@code{wtforms.ext.sqlalchemy} did.")
    (license license:bsd-3)))

(define-public python-paste
  (package
    (name "python-paste")
    (version "3.10.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "paste" version))
       (sha256
        (base32
         "0jjyl39r5ncx98rwi855x71qrwvwm59idgn7q0c7m2jyb8314g8w"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-pytest
           python-setuptools
           python-wheel))
    (home-page "https://pythonpaste.readthedocs.io/")
    (synopsis "Python web development tools, focusing on WSGI")
    (description
     "Paste provides a variety of web development tools and middleware which
can be nested together to build web applications.  Paste's design closely
follows ideas flowing from WSGI (Web Standard Gateway Interface).")
    (license license:expat)))

(define-public python-pastescript
  (package
    (name "python-pastescript")
    (version "3.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pastescript" version))
       (sha256
        (base32 "08959bmp62pb2rlwr4wpwij15y83jcf9wa9jgg32jlvfzf6h4vsk"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags #~(list "--ignore=tests/appsetup/testfiles")))
    (native-inputs
     (list python-pytest
           python-wheel))
    (propagated-inputs
     (list python-paste
           python-pastedeploy
           python-setuptools))
    (home-page "https://github.com/pasteorg/pastescript")
    (synopsis "Pluggable command line tool for serving web applications and more")
    (description
     "PasteScript is a plugin-friendly command line tool which provides a
variety of features, from launching web applications to bootstrapping project
layouts.")
    (license license:expat)))

(define-public python-urlgrabber
  (package
    (name "python-urlgrabber")
    (version "4.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "urlgrabber" version))
       (sha256
        (base32 "0fg16zlw49cw7pjq9dhpc5vd35d5zz1mlki55m464qxfmfpzhnh7"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key inputs outputs tests? #:allow-other-keys)
             (when tests?
               (add-installed-pythonpath inputs outputs)
               (invoke "python" "test/runtests.py")))))))
    (propagated-inputs
     (list python-pycurl python-setuptools python-six))
    (home-page "http://urlgrabber.baseurl.org/") ; no HTTPS
    (synopsis "High-level cross protocol url-grabber")
    (description
     "@code{urlgrabber} is a library that unifies access to files available on
the web, FTP or locally.  It supports HTTP, FTP and file:// protocols, it
supports features like HTTP keep-alive, reget, throttling and more.")
    (license license:lgpl2.1+)))

(define-public python-pycares
  (package
    (name "python-pycares")
    (version "4.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pycares" version))
       (sha256
        (base32
         "0i8d0433wmm7wi8i2l2hjiyhmy35b9s888qrk6fqx5xcdmpnjhn5"))))
    (build-system python-build-system)
    (arguments `(#:tests? #f))          ;tests require internet access
    (propagated-inputs (list python-cffi))
    (home-page "https://github.com/saghul/pycares")
    (synopsis "Python interface for @code{c-ares}")
    (description "@code{pycares} is a Python module which provides an
interface to @code{c-ares}, a C library that performs DNS requests and
name resolutions asynchronously.")
    (license license:expat)))

(define-public python-yarl
  (package
    (name "python-yarl")
    (version "1.18.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "yarl" version))
       (sha256
        (base32
         "1qfj6kvdxr8bzka4lp0bh0xfgr5mzx7zzvj2jb4vcxxzbk20265c"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags '(list "--ignore-glob=tests/test_*_benchmarks.py")
      #:phases
      '(modify-phases %standard-phases
         (add-after 'unpack 'patch-build-system
           (lambda _
             ;; XXX: I don't know how to tell it to build the extensions in
             ;; place.
             (substitute* "packaging/pep517_backend/_backend.py"
               (("build_inplace=False") "build_inplace=True")))))))
    (native-inputs
     (list python-covdefaults
           python-cython-3
           python-expandvars
           python-pytest
           python-pytest-cov
           python-pytest-xdist
           python-setuptools
           python-tomli
           python-wheel))
    (propagated-inputs
     (list python-packaging python-idna python-multidict python-propcache))
    (home-page "https://github.com/aio-libs/yarl/")
    (synopsis "Yet another URL library")
    (description "@code{yarl} module provides handy @code{URL} class
for URL parsing and changing.")
    (license license:asl2.0)))

(define-public python-canvasapi
  (package
    (name "python-canvasapi")
    (version "3.3.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/ucfopen/canvasapi")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1gg6h2w4bajsis35p7l5r6bx54h5j2nq16r90wq8fbb0d3gz3f6q"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-arrow python-pytz python-requests))
    (native-inputs (list python-requests-mock
                         python-setuptools
                         python-urllib3-next
                         python-wheel))
    (home-page "https://github.com/ucfopen/canvasapi")
    (synopsis "API wrapper for the Canvas LMS")
    (description
     "CanvasAPI is a Python library for accessing Instructure’s Canvas LMS API.
The library enables developers to programmatically manage Canvas courses,
users, gradebooks, and more.")
    (license license:expat)))

(define-public python-google
  (package
    (name "python-google")
    (version "3.0.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "google" version))
              (sha256
               (base32
                "1gncv3l11za0mpxvmpaf5n5j3jzp282rz62yml4ha4z55q930d8l"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f)) ; There are no tests.
    (propagated-inputs
     (list python-beautifulsoup4))
    (home-page "https://breakingcode.wordpress.com/")
    (synopsis "Python bindings to the Google search engine")
    (description "This package provides Python bindings for using the
Google search engine.  Its module is called @code{googlesearch}.")
    (license license:bsd-3)))

(define-public python-google-api-client
  (package
    (name "python-google-api-client")
    (version "2.102.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "google-api-python-client" version))
       (sha256
        (base32
         "07b9afz3g3lk976i974h6ikvl7wzwm0a0ws0iynxvmd4favgjvwh"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f))    ; tests require internet access
    (propagated-inputs
     (list python-google-api-core
           python-google-auth
           python-google-auth-httplib2
           python-httplib2
           python-pyparsing
           python-six
           python-uritemplate-3))
    (home-page "https://github.com/google/google-api-python-client")
    (synopsis "Core Python library for accessing Google APIs")
    (description "Python client library for Google's discovery based APIs")
    (license license:asl2.0)))

(define-public python-google-auth-httplib2
  (package
    (name "python-google-auth-httplib2")
    (version "0.1.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "google-auth-httplib2" version))
              (sha256
               (base32 "0agwwgw04nks7lzpm224fbvpnjfgzz67pkrfls77ipf6zmawajy6"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-google-auth python-httplib2 python-six))
    (native-inputs
     (list python-flask python-mock python-pytest python-pytest-localserver))
    (home-page
     "https://github.com/GoogleCloudPlatform/google-auth-library-python-httplib2")
    (synopsis "Google Authentication Library: httplib2 transport")
    (description "This package provides a Google Authentication Library plugin
for httplib2 transport.")
    (license license:asl2.0)))

(define-public python-google-auth-oauthlib
  (package
    (name "python-google-auth-oauthlib")
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "google-auth-oauthlib" version))
       (sha256
        (base32 "1yvsjd1vh440nsh9vpnig23sq4k1ia749x5g1dwm7r4110xqrsl3"))))
    (build-system python-build-system)
    (propagated-inputs (list python-google-auth python-requests-oauthlib))
    (native-inputs (list python-flask))
    (home-page
     "https://github.com/GoogleCloudPlatform/google-auth-library-python-oauthlib")
    (synopsis "Google Authentication Library: oauthlib")
    (description "This package provides a Google Authentication Library plugin
with oauthlib.")
    (license license:asl2.0)))

(define-public whoogle-search
  (package
    (name "whoogle-search")
    (version "0.8.4")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "whoogle-search" version))
              (sha256
               (base32
                "0hsy4l98nzj6i1lf63wvqd0yharkaz44zsxh863ymy47rxnwimz7"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; The tests need network access
      #:tests? #f))
    (propagated-inputs
     (list python-beautifulsoup4
           python-brotli
           python-cryptography
           python-cssutils
           python-defusedxml
           python-flask
           python-dotenv
           python-requests
           python-stem
           python-validators
           python-waitress))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://github.com/benbusby/whoogle-search")
    (synopsis "Self-hosted, ad-free, privacy-respecting metasearch engine")
    (description
     "Get Google results, but without any ads, javascript, AMP links,
cookies, or IP address tracking.  Quick and simple to implement as a primary
search engine replacement on both desktop and mobile.")
    (license license:expat)))

(define-public python-hawkauthlib
  (package
    (name "python-hawkauthlib")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "hawkauthlib" version))
       (sha256
        (base32
         "03ai47s4h8nfnrf25shbfvkm1b9n1ccd4nmmj280sg1fayi69zgg"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-requests python-webob))
    (home-page "https://github.com/mozilla-services/hawkauthlib")
    (synopsis "Hawk Access Authentication protocol")
    (description
     "This is a low-level Python library for implementing Hawk Access Authentication,
a simple HTTP request-signing scheme.")
    (license license:mpl2.0)))

(define-public python-pybrowserid
  (package
    (name "python-pybrowserid")
    (version "0.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "PyBrowserID" version))
       (sha256
        (base32
         "1qvi79kfb8x9kxkm5lw2mp42hm82cpps1xknmsb5ghkwx1lpc8kc"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-requests))
    (native-inputs
     (list python-mock))
    (home-page "https://github.com/mozilla/PyBrowserID")
    (synopsis "Python library for the BrowserID protocol")
    (description
     "This is a Python client library for the BrowserID protocol that
underlies Mozilla Persona.")
    (license license:mpl2.0)))

(define-public python-pyfxa
  (package
    (name "python-pyfxa")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "PyFxA" version))
       (sha256
        (base32
         "0axl16fyrz2r88gnw4b12mk7dpkqscv8c4wsc1y5hicl7bsbc4fm"))))
    (build-system python-build-system)
    (arguments '(#:tests? #f)) ; 17 tests require network access
    (propagated-inputs
     (list python-cryptography python-hawkauthlib python-pybrowserid
           python-requests python-six))
    (native-inputs
     (list python-grequests python-mock python-responses python-unittest2))
    (home-page "https://github.com/mozilla/PyFxA")
    (synopsis "Firefox Accounts client library for Python")
    (description
     "This is a Python library for interacting with the Firefox Accounts
ecosystem.")
    (license license:mpl2.0)))

(define-public python-hyperlink
  (package
    (name "python-hyperlink")
    (version "21.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "hyperlink" version))
        (sha256
         (base32
          "0sx50lkivsfjxx9zr4yh7l9gll2l9kvl0v0w8w4wk2x5v9bzjyj2"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-before 'check 'pretend-to-be-CI
                 (lambda _
                   ;; Pretend to be a CI system to skip flaky tests.
                   (setenv "CI" "true"))))))
    (native-inputs
     (list python-pytest python-setuptools python-wheel))
    (propagated-inputs
     (list python-idna))
    (home-page "https://github.com/python-hyper/hyperlink")
    (synopsis "Python module to create immutable URLs according to spec")
    (description "This package provides a Python module to create immutable, and
correct URLs for Python according to RFCs 3986 and 3987.")
    (license license:expat)))

(define-public python-treq
  (package
    (name "python-treq")
    (version "24.9.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "treq" version))
        (sha256
         (base32
          "1a7yrq2m7hf77fia4fc0c6zanvwnykpgirdbs1cyvr7k0k27znhm"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-httpbin
           python-pytest
           python-service-identity
           python-setuptools
           python-wheel))
    (propagated-inputs
     (list python-attrs
           python-hyperlink
           python-incremental
           python-requests
           python-twisted
           python-typing-extensions))
    (home-page "https://github.com/twisted/treq")
    (synopsis "Requests-like API built on top of twisted.web's Agent")
    (description "This package provides an HTTP library inspired by
@code{requests} but written on top of Twisted's @code{Agents}.  It offers a
high level API for making HTTP requests when using Twisted.")
    (license license:expat)))

(define-public python-autobahn
  (package
    (name "python-autobahn")
    (version "19.2.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "autobahn" version))
        (sha256
         (base32
          "1mm7j24ls01c7jb1ad5p5cpyxvzgydiyf8b04ihykh2v8g98j0x7"))))
    (build-system python-build-system)
    (arguments
      ;; The tests fail to run:
      ;; https://github.com/crossbario/autobahn-python/issues/1117
     `(#:tests? #f))
    (propagated-inputs
     (list python-cffi python-twisted python-txaio))
    (home-page "https://crossbar.io/autobahn/")
    (synopsis "Web Application Messaging Protocol implementation")
    (description "This package provides an implementation of the @dfn{Web Application
Messaging Protocol} (WAMP).  WAMP connects components in distributed
applications using Publish and Subscribe (PubSub) and routed Remote Procedure
Calls (rRPC).  It is ideal for distributed, multi-client and server applications
such as IoT applications or multi-user database-driven business applications.")
    (license license:expat)))

(define-public python-ws4py
  (package
    (name "python-ws4py")
    (version "0.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "ws4py" version))
       (sha256
        (base32
         "10slbbf2jm4hpr92jx7kh7mhf48sjl01v2w4d8z3f1p0ybbp7l19"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'python3.7-compatibility
           (lambda _
             (substitute* '("ws4py/server/tulipserver.py"
                            "ws4py/async_websocket.py")
               (("asyncio.async")
                "asyncio.ensure_future"))
             #t))
         ;; We don't have a package for cherrypy.
         (add-after 'unpack 'remove-cherrypy-support
           (lambda _
             (delete-file "ws4py/server/cherrypyserver.py")
             #t)))))
    (propagated-inputs
     (list python-gevent python-tornado))
    (home-page "https://github.com/Lawouach/WebSocket-for-Python")
    (synopsis "WebSocket client and server library")
    (description
     "This package provides a WebSocket client and server library for
Python.")
    (license license:bsd-3)))

(define-public python-slowapi
  (package
    (name "python-slowapi")
    (version "0.1.9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/laurents/slowapi")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1lc1n6lvh01dhhrc507qri9rz1w72l2riii03ixh7n7n1gwjpws7"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      ;; These tests fail because the timestamp contains more decimal points
      ;; than expected, so a simple equality comparison fails.
      '(list "-k" (string-append "not test_headers_no_breach"
                                 " and not test_headers_breach"))))
    (propagated-inputs (list python-limits python-redis))
    (native-inputs
     (list python-fastapi
           python-hiro
           python-mock
           python-poetry-core
           python-pytest))
    (home-page "https://github.com/laurents/slowapi")
    (synopsis "Rate limiting extension for Starlette and Fastapi")
    (description
     "This package provides a rate limiting extension for Starlette and
Fastapi.")
    (license license:expat)))

(define-public python-slugify
  (package
    (name "python-slugify")
    (version "5.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "python-slugify" version))
       (sha256
        (base32 "1aww2ncglyii4jkbfjxqhinivawf9zmwifcj32d69gpwp6h86czi"))))
    (propagated-inputs
     (list python-unidecode python-text-unidecode))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "python" "test.py")))))))
    (build-system python-build-system)
    (home-page "https://github.com/un33k/python-slugify")
    (synopsis "Python Slugify application that handles Unicode")
    (description "This package provides a @command{slufigy} command and
library to create slugs from unicode strings while keeping it DRY.")
    (license license:expat)))

(define-public python-branca
  (package
    (name "python-branca")
    (version "0.7.2")
    (source
     (origin
       (method git-fetch) ; no tests in PyPI
       (uri (git-reference
             (url "https://github.com/python-visualization/branca")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1vs94nqa7r6iwm8mj3m29hg090gmgz4ywnayxh8qiz9ij8jv96wa"))))
    (build-system pyproject-build-system)
    (arguments
     ;; This file requires Selenium.
     (list #:test-flags #~(list "--ignore" "tests/test_iframe.py"
                                ;; This test passes but is very slow.
                                "-k" "not test_color_brewer_extendability")
           #:phases
           #~(modify-phases %standard-phases
               (add-before 'build 'pretend-version
                 ;; The version string is usually derived via setuptools-scm,
                 ;; but without the git metadata available, the version string
                 ;; is set to '0.0.0'.
                 (lambda _
                   (setenv "SETUPTOOLS_SCM_PRETEND_VERSION"
                           #$(package-version this-package)))))))
    (propagated-inputs (list python-jinja2))
    (native-inputs
     (list python-numpy
           python-pytest
           python-setuptools
           python-setuptools-scm
           python-wheel))
    (home-page "https://github.com/python-visualization/branca")
    (synopsis "Generate complex HTML+JS pages with Python")
    (description "This library is a spinoff from @code{folium} that would host
the non-map-specific features.  It can be used to generate HTML + JS.")
    (license license:expat)))

(define-public python-tinycss2
  (package
    (name "python-tinycss2")
    (version "1.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Kozea/tinycss2")
             (commit (string-append "v" version))
             (recursive? #true)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "11m5vhpdz6zlzq2lqqklkhd0h4jxblpvcrdf9cc4s06nhr9acp0r"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-flit
           python-pytest
           python-pytest-cov))
    (propagated-inputs
     (list python-webencodings))
    (home-page "https://tinycss2.readthedocs.io/")
    (synopsis "Low-level CSS parser for Python")
    (description "@code{tinycss2} can parse strings, return Python objects
representing tokens and blocks, and generate CSS strings corresponding to
these objects.

Based on the CSS Syntax Level 3 specification, @code{tinycss2} knows the
grammar of CSS but doesn’t know specific rules, properties or values supported
in various CSS modules.")
    (license license:bsd-3)))

(define-public python-cssselect2
  (package
    (name "python-cssselect2")
    (version "0.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "cssselect2" version))
       (sha256
        (base32 "1j2fcr217rsvkipsg6zjq03rl64rxnvb5hqqpx0dv58fhspvkywk"))))
    (build-system pyproject-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'disable-linters
           ;; Their check fails; none of our business.
           (lambda _
             (substitute* '("setup.py" "pyproject.toml")
               (("'pytest-flake8',") "")
               (("'pytest-isort',") "")
               (("--flake8") "")
               (("--isort") "")))))))
    (propagated-inputs
     (list python-tinycss2))
    (native-inputs
     (list python-flit python-pytest-cov python-pytest-runner))
    (home-page "https://cssselect2.readthedocs.io/")
    (synopsis "CSS selectors for Python ElementTree")
    (description "@code{cssselect2} is a straightforward implementation of
CSS3 Selectors for markup documents (HTML, XML, etc.) that can be read by
ElementTree-like parsers (including cElementTree, lxml, html5lib, etc.).

Unlike the Python package @code{cssselect}, it does not translate selectors to
XPath and therefore does not have all the correctness corner cases that are
hard or impossible to fix in cssselect.")
    (license license:bsd-3)))

(define-public python-uvloop
  (package
    (name "python-uvloop")
    (version "0.21.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "uvloop" version))
       (sha256
        (base32 "1qq46ym3ymzfn4j6fnykfmr1f4qnb7x7p15dlw37hi38v87jpw9v"))
       (modules '((guix build utils)))
       (snippet
        '(begin (delete-file-recursively "vendor")
          (delete-file  "uvloop/loop.c")))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      ;; The tests are prone to get stuck. Use pytest-timeout’s --timeout flag
      ;; to get a meaningful idea about where.
      #~(list "--timeout=300"
              "--timeout-method=thread"
              "-k" (string-join
                    (list
                     ;; Timeout, because SIGINT cannot be sent to child.
                     "not test_signals_sigint_pycode_continue"
                     "test_signals_sigint_pycode_stop"
                     "test_signals_sigint_uvcode"
                     "test_signals_sigint_uvcode_two_loop_runs"
                     ;; This test is racy and prone to get stuck on
                     ;; various platforms, possibly a aiohttp issue:
                     ;;  https://github.com/MagicStack/uvloop/issues/412
                     "test_remote_shutdown_receives_trailing_data"
                     ;; It looks like pytest is preventing
                     ;; custom stdout/stderr redirection,
                     ;; even with -s.
                     "test_process_streams_redirect"
                     ;; FileNotFoundError: [Errno 2] No such file or
                     ;; directory
                     "test_process_env_2"
                     ;; socket.gaierror: [Errno -2] Name or service not known
                     "test_getaddrinfo_21"
                     ,@(if (target-riscv64?)
                           `("test_renegotiation")
                           `()))
                    " and not "))
      #:phases
       #~(modify-phases %standard-phases
         (add-after 'unpack 'preparations
           (lambda _
             ;; Use packaged libuv.
             (substitute* "setup.py" (("self.use_system_libuv = False")
                                      "self.use_system_libuv = True"))
             ;; Replace hardcoded shell command.
             (substitute* "uvloop/loop.pyx"
               (("b'/bin/sh'") (string-append "b'" (which "sh") "'")))))
         ,@(if (target-riscv64?)
               `((add-after 'unpack 'adjust-test-timeouts
                   (lambda _
                     (substitute* '("tests/test_tcp.py"
                                    "tests/test_unix.py")
                       (("SSL_HANDSHAKE_TIMEOUT = 15\\.0")
                        "SSL_HANDSHAKE_TIMEOUT = 30.0")))))
               '())
         (add-before 'check 'pre-check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               ;; Remove Python module, which conflicts with the installed
               ;; version, but lacks the built C module.
               (delete-file-recursively "uvloop")))))))
    (native-inputs
     (list python-aiohttp
           python-cython-3
           python-psutil
           python-pyopenssl
           python-pytest
           python-pytest-timeout
           python-setuptools
           python-wheel))
    (inputs
     (list libuv))
    (home-page "https://github.com/MagicStack/uvloop")
    (synopsis "Fast implementation of asyncio event loop on top of libuv")
    (description
     "@code{uvloop} is a fast, drop-in replacement of the built-in asyncio
event loop.  It is implemented in Cython and uses libuv under the hood.")
    (license license:expat)))

(define-public gunicorn
  (package
    (name "gunicorn")
    (version "21.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "gunicorn" version))
       (sha256
        (base32 "0cqhr2g880snpi45qvvch288m4fdyd5vqrcz3gk9hkv33pzqpv48"))))
    (outputs '("out" "doc"))
    (build-system pyproject-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'build 'build-doc
           (lambda _
             (invoke "make" "-C" "docs" "PAPER=a4" "html" "info")
             (delete-file "docs/build/texinfo/Makefile")
             (delete-file "docs/build/texinfo/Gunicorn.texi")))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (if tests?
                 (begin
                   (invoke "pytest" "-vv"
                           ;; Disable the geventlet tests because eventlet uses
                           ;; dnspython, which does not work in the build
                           ;; container due to lack of /etc/resolv.conf, etc.
                           "--ignore=tests/workers/test_geventlet.py"))
                 (format #t "test suite not run~%"))))
         (add-after 'install 'install-doc
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((doc (string-append (assoc-ref outputs "doc")
                                        "/share/doc/" ,name "-" ,version))
                    (html (string-append doc "/html"))
                    (info (string-append doc "/info"))
                    (examples (string-append doc "/examples")))
               (mkdir-p html)
               (mkdir-p info)
               (mkdir-p examples)
               (copy-recursively "docs/build/html" html)
               (copy-recursively "docs/build/texinfo" info)
               (copy-recursively "examples" examples)
               (for-each (lambda (file)
                           (copy-file file (string-append doc "/" file)))
                         '("README.rst" "NOTICE" "LICENSE" "THANKS")))))
         ;; XXX: The wrap phase includes native inputs on PYTHONPATH, (see
         ;; <https://bugs.gnu.org/25235>), leading to an inflated closure
         ;; size.  Override it to only add the essential entries.
         (replace 'wrap
           (lambda* (#:key native-inputs inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (python (assoc-ref (or native-inputs inputs) "python"))
                    (sitedir (string-append "/lib/python"
                                            (python-version python)
                                            "/site-packages")))
               (wrap-program (string-append out "/bin/gunicorn")
                 `("PYTHONPATH" ":" prefix
                   ,(map (lambda (output)
                           (string-append output sitedir))
                         (list python out))))))))))
    (inputs (list bash-minimal))
    (native-inputs
     (list binutils ;; for ctypes.util.find_library()
           python-aiohttp
           python-gevent
           python-pytest
           python-pytest-cov
           python-sphinx
           texinfo))
    (propagated-inputs (list python-packaging python-setuptools python-wheel))
    (home-page "https://gunicorn.org/")
    (synopsis "Python WSGI HTTP Server for UNIX")
    (description "Gunicorn ‘Green Unicorn’ is a Python WSGI HTTP
Server for UNIX.  It’s a pre-fork worker model ported from Ruby’s
Unicorn project.  The Gunicorn server is broadly compatible with
various web frameworks, simply implemented, light on server resources,
and fairly speedy.")
  (license license:expat)))

(define-public gunicorn-next
  (package
    (inherit gunicorn)
    (name "gunicorn")
    (version "22.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "gunicorn" version))
       (sha256
        (base32
         "0qzc3ghayc137hlwrqqwkkhaf8f5h9ja21qwy4rznxpz75i462sa"))))
    ;; CVE-2024-1135 is fixed in version 22.0.0.
    (properties `((lint-hidden-cve . ("CVE-2024-1135"))))))

;; break cyclic dependency for python-aiohttp, which depends on gunicorn for
;; its tests
(define-public gunicorn-bootstrap
  (package
    (inherit gunicorn)
    (name "gunicorn")
    (arguments `(#:tests? #f))
    (properties '((hidden? . #t)))
    (native-inputs `())))

(define-public python-httptools
  (package
    (name "python-httptools")
    (version "0.1.1")
    (source
     (origin
       ;; PyPI tarball comes with a vendored http-parser and no tests.
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/MagicStack/httptools")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0g08128x2ixsiwrzskxc6c8ymgzs39wbzr5mhy0mjk30q9pqqv77"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'preparations
           (lambda _
             ;; Skip a failing test (AssertionError).  Bug report:
             ;; https://github.com/MagicStack/httptools/issues/10.
             (substitute* "tests/test_parser.py"
               (("    def test_parser_response_1")
                (string-append
                 "    @unittest.skip(\"Disabled.\")\n"
                 "    def test_parser_response_1")))
             ;; Use packaged http-parser.
             (substitute* "setup.py" (("self.use_system_http_parser = False")
                                      "self.use_system_http_parser = True"))
             ;; This path is hardcoded.  Hardcode our own.
             (substitute* "httptools/parser/cparser.pxd"
               (("../../vendor/http-parser")
                (string-append (assoc-ref %build-inputs "http-parser")
                               "/include")))
             ;; Don't force Cython version.
             (substitute* "setup.py" (("Cython==") "Cython>="))
             #t)))))
    (native-inputs
     (list python-cython python-pytest))
    (inputs
     (list http-parser))
    (home-page "https://github.com/MagicStack/httptools")
    (synopsis "Collection of framework independent HTTP protocol utils")
    (description
     "@code{httptools} is a Python binding for the nodejs HTTP parser.")
    (license license:expat)))

(define-public python-uvicorn
  (package
    (name "python-uvicorn")
    (version "0.34.0")
    (source
     (origin
       ;; PyPI tarball has no tests.
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/encode/uvicorn")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "05lkxnpzmr0kik81kdcvavjvvc4d1lgmw88mr4vbwsqk147wgqbc"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list "-o" "asyncio_mode=auto"
              ;; For some reason tests stacked in infinity re-invocation loop:
              ;; AssertionError where is_alive =
              ;; <uvicorn.supervisors.multiprocess.Process object at
              ;; 0x7ffff39b6110>.is_alive.
              ;; Maybe this <https://github.com/encode/uvicorn/issues/2466>.
              "--ignore=tests/supervisors/test_multiprocess.py"

              #$@(cond
                  ((or (target-aarch64?)
                       (target-riscv64?))
                   '("-k not test_send_binary_data_to_server_bigger_than_default_on_websockets"))
                  (#t '())))))
    (native-inputs
     (list nss-certs-for-test
           python-a2wsgi
           python-hatchling
           python-httpx-bootstrap
           python-pytest
           python-pytest-asyncio
           python-pytest-mock
           python-requests
           python-trustme
           python-wsproto))
    (propagated-inputs
     (list python-a2wsgi
           python-click
           python-dotenv
           python-h11
           python-httptools
           python-pyyaml
           python-uvloop
           python-watchfiles
           python-websockets))
    (home-page "https://github.com/encode/uvicorn")
    (synopsis "Fast ASGI server implementation")
    (description
     "@code{uvicorn} is a fast ASGI server implementation, using @code{uvloop}
and @code{httptools}.  It currently supports HTTP/1.1 and WebSockets.  Support
for HTTP/2 is planned.")
    (license license:bsd-3)))

(define-public python-translation-finder
  (package
    (name "python-translation-finder")
    (version "1.7")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "translation-finder" version))
        (sha256
         (base32
          "1pcy9z8gmb8x41gjhw9x0lkr0d2mv5mdxcs2hwg6q8mxs857j589"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'remove-failing-test
           (lambda _
             (delete-file "translation_finder/test_api.py")
             #t)))))
    (propagated-inputs
     (list python-chardet python-pathlib2 python-ruamel.yaml python-six))
    (native-inputs
     (list python-codecov python-codacy-coverage python-pytest-cov
           python-pytest-runner python-twine))
    (home-page "https://weblate.org/")
    (synopsis "Translation file finder for Weblate")
    (description "This package provides a function to find translation file in
the source code of a project.  It supports many translation file formats and
is part of the Weblate translation platform.")
    (license license:gpl3+)))

(define-public python-gitlab
  (package
    (name "python-gitlab")
    (version "3.2.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "python-gitlab" version))
              (sha256
               (base32
                "1gi4lp2g4k99zqcx2bgqx940bpmpbc1w9qkl5gy33hpy148yhvlg"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f))                             ;tests require network access
    (propagated-inputs (list python-requests python-requests-toolbelt))
    (home-page "https://github.com/python-gitlab/python-gitlab")
    (synopsis "Interact with GitLab API")
    (description "This package provides an extended library for interacting
with GitLab instances through their API.")
    (license license:lgpl3+)))

(define-public python-path-and-address
  (package
    (name "python-path-and-address")
    (version "2.0.1")
    (source
     (origin
       ;; The source distributed on PyPI doesn't include tests.
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/joeyespo/path-and-address")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0b0afpsaim06mv3lhbpm8fmawcraggc11jhzr6h72kdj1cqjk5h6"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (add-installed-pythonpath inputs outputs)
             (invoke "py.test"))))))
    (native-inputs
     (list python-pytest))
    (home-page "https://github.com/joeyespo/path-and-address")
    (synopsis "Functions for command-line server tools used by humans")
    (description "Path-and-address resolves ambiguities of command-line
interfaces, inferring which argument is the path, and which is the address.")
    (license license:expat)))

(define-public grip
  (package
    (name "grip")
    (version "4.6.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/joeyespo/grip")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0vhimd99zw7s1fihwr6yfij6ywahv9gdrfcf5qljvzh75mvzcwh8"))))
    (build-system python-build-system)
    (propagated-inputs (list python-docopt
                             python-flask
                             python-markdown
                             python-path-and-address
                             python-pygments
                             python-requests))
    (native-inputs (list python-pytest python-responses))
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (replace 'check
                 (lambda* (#:key inputs outputs tests? #:allow-other-keys)
                   (when tests?
                     (add-installed-pythonpath inputs outputs)
                     (setenv "PATH"
                             (string-append (getenv "PATH") ":"
                                            #$output "/bin"))
                     (invoke "py.test" "-m" "not assumption")))))))
    (home-page "https://github.com/joeyespo/grip")
    (synopsis "Preview Markdown files using the GitHub API")
    (description
     "Grip is a command-line server application written in Python
that uses the GitHub Markdown API to render a local Markdown file.  The styles
and rendering come directly from GitHub, so you'll know exactly how it will
appear.  Changes you make to the file will be instantly reflected in the browser
without requiring a page refresh.")
    (license license:expat)))

(define-public python-port-for
  (package
    (name "python-port-for")
    (version "0.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "port-for" version))
       (sha256
        (base32
         "1pncxlj25ggw99r0ijfbkq70gd7cbhqdx5ivsxy4jdp0z14cpda7"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'use-urllib3
           (lambda _
             (substitute* "port_for/_download_ranges.py"
               (("urllib2") "urllib3")))))))
    (propagated-inputs
     (list python-urllib3))
    (native-inputs
     (list python-mock))
    (home-page "https://github.com/kmike/port-for/")
    (synopsis "TCP localhost port finder and association manager")
    (description
     "This package provides a utility that helps with local TCP ports
management.  It can find an unused TCP localhost port and remember the
association.")
    (license license:expat)))

(define-public python-livereload
  (package
    (name "python-livereload")
    (version "2.6.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "livereload" version))
       (sha256
        (base32
         "0scqjnhg3ap81v36ghp0pik774dnfdkwqsx5j1jfbzarbs32yvvp"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-six python-tornado))
    (home-page "https://github.com/lepture/python-livereload")
    (synopsis "Python LiveReload")
    (description
     "Python LiveReload provides a command line utility, @command{livereload},
for starting a web server in a directory.  It can trigger arbitrary commands
and serve updated contents upon changes to the directory.")
    (license license:bsd-3)))

(define-public python-vf-1
  (package
    (name "python-vf-1")
    (version "0.0.11")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "VF-1" version))
       (sha256
        (base32
         "0xlqsaxsiayk1sd07kpz8abbcnab582y29a1y4882fq6j4gma5xi"))))
    (build-system python-build-system)
    (home-page "https://github.com/solderpunk/VF-1")
    (synopsis "Command line gopher client")
    (description "@code{VF-1} is a command line gopher client with
@acronym{TLS, Transport Layer Security} support.")
    (license license:bsd-2)))

(define-public python-httpcore
  (package
    (name "python-httpcore")
    (version "1.0.7")
    (source
     (origin
       ;; PyPI tarball does not contain tests.
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/encode/httpcore")
             (commit  version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1wz54snks59m1mz2mv0i4p37zz2rrzd99gpg8sh3qkpck5h8lhc4"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list nss-certs-for-test
           python-hatch-fancy-pypi-readme
           python-hatchling
           python-pytest
           python-pytest-asyncio
           python-pytest-cov
           python-pytest-httpbin
           python-pytest-trio
           python-uvicorn
           python-trustme))
    (propagated-inputs
     (list python-anyio
           python-certifi
           python-h11
           python-h2
           python-sniffio
           python-socksio
           python-trio
           python-trio-typing))
    (home-page "https://github.com/encode/httpcore")
    (synopsis "Minimal, low-level HTTP client")
    (description
     "HTTP Core provides a minimal and low-level HTTP client, which does one
thing only: send HTTP requests.

Some things HTTP Core does do:

@itemize
@item Sending HTTP requests.
@item Provides both sync and async interfaces.
@item Supports HTTP/1.1 and HTTP/2.
@item Async backend support for asyncio and trio.
@item Automatic connection pooling.
@item HTTP(S) proxy support.
@end itemize")
    (license license:bsd-3)))

;;; Variant used to break a cycle between python-httpx and python-uvicorn.
(define-public python-httpcore-bootstrap
  (hidden-package
   (package/inherit python-httpcore
     (name "python-httpcore-bootstrap")
     (arguments (list #:tests? #f))
     (native-inputs (list python-hatchling
                          python-hatch-fancy-pypi-readme)))))

(define-public python-httpx
  (package
    (name "python-httpx")
    (version "0.28.1")
    (source
     (origin
       ;; PyPI tarball does not contain tests.
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/encode/httpx")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "00d6rb21xwazwb0sj68zhra0qxxcwkd0vv7bg3k1jg94dmk2w7xl"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      '(list "-vv" "-o" "asyncio_mode=auto"
             "-k"
             ;; These tests try to open an outgoing connection.
             (string-append
              "not test_connect_timeout"
              " and not test_that_send_cause_async_client_to_be_not_closed"
              " and not test_that_async_client_caused_warning_when_being_deleted"
              " and not test_that_send_cause_client_to_be_not_closed"
              " and not test_async_proxy_close"
              " and not test_sync_proxy_close"
              ;; This test is apparently incompatible with
              ;; python-click 8, fails with " AttributeError:
              ;; 'function' object has no attribute 'name'".
              " and not test_main"))))
    (native-inputs
     (list nss-certs-for-test
           python-cryptography
           python-hatchling
           python-hatch-fancy-pypi-readme
           python-pytest
           python-pytest-asyncio
           python-pytest-trio
           python-trio
           python-trio-typing
           python-trustme
           python-uvicorn
           python-setuptools
           python-wheel
           python-zstandard))
    (propagated-inputs
     (list python-charset-normalizer
           python-brotli
           python-certifi
           python-chardet
           python-httpcore
           python-idna
           python-rfc3986
           python-sniffio))
    (home-page "https://www.python-httpx.org/")
    (synopsis "HTTP client for Python")
    (description
     "HTTPX is a fully featured HTTP client for Python 3, which provides sync
and async APIs, and support for both HTTP/1.1 and HTTP/2.

HTTPX builds on the well-established usability of requests, and gives you:

@itemize
@item A broadly requests-compatible API.
@item Standard synchronous interface, but with async support if you need it.
@item HTTP/1.1 and HTTP/2 support.
@item Ability to make requests directly to WSGI applications or ASGI applications.
@item Strict timeouts everywhere.
@item Fully type annotated.
@item 99% test coverage.
@end itemize

Plus all the standard features of requests:

@itemize
@item International Domains and URLs
@item Keep-Alive & Connection Pooling
@item Sessions with Cookie Persistence
@item Browser-style SSL Verification
@item Basic/Digest Authentication
@item Elegant Key/Value Cookies
@item Automatic Decompression
@item Automatic Content Decoding
@item Unicode Response Bodies
@item Multipart File Uploads
@item HTTP(S) Proxy Support
@item Connection Timeouts
@item Streaming Downloads
@item .netrc Support
@item Chunked Requests
@end itemize")
    (license license:bsd-3)))

;;; Variant used to break a cycle with python-uvicorn.
(define-public python-httpx-bootstrap
  (hidden-package
   (package/inherit python-httpx
     (name "python-httpx-bootstrap")
     (arguments (list #:tests? #f))
     (native-inputs
      (list python-hatchling
            python-hatch-fancy-pypi-readme))
     (propagated-inputs
      (modify-inputs (package-propagated-inputs python-httpx)
        (replace "python-httpcore"  python-httpcore-bootstrap))))))

(define-public python-wsgiprox
  (package
    (name "python-wsgiprox")
    (version "1.5.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "wsgiprox" version))
       (sha256
        (base32
         "11fsm199pvwbmqx2lccznvws65aam1rqqv0w79gal8hispwgd5rs"))))
    (build-system python-build-system)
    (arguments
     ;; The test suite hangs (see:
     ;; https://github.com/webrecorder/wsgiprox/issues/6).
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-pytest-argument
           (lambda _
             ;; See: https://github.com/webrecorder/wsgiprox/issues/7.
             (substitute* "setup.py"
               (("--doctest-module")
                "--doctest-modules")))))))
    (propagated-inputs
     (list python-certauth python-gevent python-websocket-client))
    (native-inputs
     (list python-mock python-pytest-cov python-waitress))
    (home-page "https://github.com/webrecorder/wsgiprox")
    (synopsis "HTTP/S proxy with WebSockets over WSGI")
    (description "@code{wsgiprox} is a Python WSGI (Web Server Gateway
Interface) middle-ware for adding HTTP and HTTPS proxy support to a WSGI
application.  The library accepts HTTP and HTTPS proxy connections, and routes
them to a designated prefix.")
    (license license:asl2.0)))

(define-public python-warcio
  ;; The PyPI release is missing some test support files (see:
  ;; https://github.com/webrecorder/warcio/issues/132).
  (package
    (name "python-warcio")
    (version "1.7.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/webrecorder/warcio")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0r3kijsm8wsbipi5pxsrqpg5nn4w8iaw5i8010b0ligmfxnxamlb"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags  ; These tests fail due to networking requirements.
      '(list "-k" (format #f "not ~a"
                          (string-join
                           '("test_post_chunked"
                             "test_remote"
                             "test_capture_http_proxy"
                             "test_capture_https_proxy"
                             "test_capture_https_proxy_same_session")
                           " and not "))
             ; The following test requires the hookdns package.
             "--ignore=test/test_capture_http_proxy.py")))
    (native-inputs
     ;; These inputs are required for the test suite.
     (list python-httpbin
           python-pytest-cov
           python-requests
           python-setuptools
           python-wheel
           python-wsgiprox))
    (home-page "https://github.com/webrecorder/warcio")
    (synopsis "Streaming web archival archive (WARC) library")
    (description "warcio is a Python library to read and write the WARC format
commonly used in Web archives. It is designed for fast, low-level access to
web archival content, oriented around a stream of WARC records rather than
files.")
    (license license:asl2.0)))

(define-public python-websockets
  (package
    (name "python-websockets")
    (version "13.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/aaugustin/websockets")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1dx3rf6fmg4ir9cfcn1fil446qz879cwrjq2rcz5zvl71xkw6hb3"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-before 'check 'extend-test-timeout
                 (lambda _
                   ;; Some architectures need an even longer timeout.  Make it
                   ;; long enough that it should never fail due to timeout.
                   #$@(if (target-riscv64?)
                          #~((setenv "WEBSOCKETS_TESTS_TIMEOUT_FACTOR" "100"))
                          #~((setenv "WEBSOCKETS_TESTS_TIMEOUT_FACTOR" "10"))))))))
    (native-inputs
     (list python-setuptools
           python-wheel))
    (home-page "https://github.com/aaugustin/websockets")
    (synopsis
     "Python implementation of the WebSocket Protocol (RFC 6455 & 7692)")
    (description
     "@code{websockets} is a library for building WebSocket servers and clients
in Python with a focus on correctness and simplicity.

Built on top of @code{asyncio}, Python's standard asynchronous I/O framework,
it provides an elegant coroutine-based API.")
    (license license:bsd-3)))

(define-public python-selenium
  (package
    (name "python-selenium")
    (version "3.141.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "selenium" version))
       (sha256
        (base32
         "039hf9knvl4s3hp21bzwsp1g5ri9gxsh504dp48lc6nr1av35byy"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-urllib3))
    (home-page
     "https://github.com/SeleniumHQ/selenium/")
    (synopsis "Python bindings for Selenium")
    (description "Selenium enables web browser automation.
Selenium specifically provides infrastructure for the W3C WebDriver specification
— a platform and language-neutral coding interface compatible with all
major web browsers.")
    (license license:asl2.0)))

(define-public python-rapidjson
  (package
    (name "python-rapidjson")
    (version "1.10")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "python-rapidjson" version))
        (sha256
         (base32
          "0h1m9m4a5rlf5hw6ak7z3qbgdhmqarzxw9d140mcf7mrxpswpzmc"))
        (modules '((guix build utils)))
        (snippet
         '(delete-file-recursively "rapidjson"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags '(list "tests")
      #:phases
      #~(modify-phases %standard-phases
          ;; We cannot seem to use #:configure-flags with the
          ;; pyproject-build-system to override rj_include_dir.
          (add-after 'unpack 'override-rapidjson-sources
            (lambda _
              (substitute* "setup.py"
                (("^rj_include_dir =.*")
                 (string-append "rj_include_dir = '"
                                #$(this-package-native-input "rapidjson")
                                "/include/rapidjson" "'"))
                (("if not os.path.isdir.*") "if False:")))))))
    (native-inputs
     (list rapidjson python-pytest python-pytz python-setuptools python-wheel))
    (home-page "https://github.com/python-rapidjson/python-rapidjson")
    (synopsis "Python wrapper around rapidjson")
    (description "This package provides a python wrapper around rapidjson.")
    (license license:expat)))

(define-public python-venusian
  (package
    (name "python-venusian")
    (version "3.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "venusian" version))
       (sha256
        (base32 "0f7f67dkgxxcjfhpdd5frb9pszkf04lyzzpn5069q0xi89r2p17n"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-pytest" ,python-pytest)
       ("python-runner" ,python-pytest-runner)
       ("python-pytest-cov" ,python-pytest-cov)))
    (arguments '(#:test-target "pytest"))
    (home-page "https://docs.pylonsproject.org/projects/venusian")
    (synopsis "Library for deferring decorator actions")
    (description
     "Venusian is a library which allows framework authors to defer decorator
actions.  Instead of taking actions when a function (or class) decorator is
executed at import time, you can defer the action usually taken by the
decorator until a separate scan phase.")
    (license license:repoze)))

(define-public python-zope-deprecation
  (package
    (name "python-zope-deprecation")
    (version "4.4.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "zope.deprecation" version))
              (sha256
               (base32
                "1pz2cv7gv9y1r3m0bdv7ks1alagmrn5msm5spwdzkb2by0w36i8d"))))
    (build-system python-build-system)
    (native-inputs `())
    (propagated-inputs `())
    (home-page "https://zopedeprecation.readthedocs.io/")
    (synopsis "Function for marking deprecations")
    (description "The @code{zope.deprecation} module provides a function for
marking modules, classes, functions, methods and properties as deprecated,
displaying warnings when usaged in application code.")
    (license license:zpl2.1)))

(define-public python-translationstring
  (package
    (name "python-translationstring")
    (version "1.3")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "translationstring" version))
              (sha256
               (base32
                "0bdpcnd9pv0131dl08h4zbcwmgc45lyvq3pa224xwan5b3x4rr2f"))))
    (build-system python-build-system)
    (home-page "https://docs.pylonsproject.org/projects/translationstring")
    (synopsis "Internationalization tooling for the Pylons project")
    (description "This package provides a library used by various Pylons
project packages for internationalization (i18n) duties related to
translation.")
    (license license:repoze)))

(define-public python-plaster
  (package
    (name "python-plaster")
    (version "1.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "plaster" version))
              (sha256
               (base32
                "1hy8k0nv2mxq94y5aysk6hjk9ryb4bsd13g83m60hcyzxz3wflc3"))))
    (build-system python-build-system)
    (native-inputs
     (list python-pytest))
    (home-page "https://docs.pylonsproject.org/projects/plaster/en/latest/")
    (synopsis "Configuration loader for multiple config file formats")
    (description
     "Plaster is a loader interface around multiple config file formats.  It
exists to define a common API for applications to use when they wish to load
configuration.  The library itself does not aim to handle anything except a
basic API that applications may use to find and load configuration settings.
Any specific constraints should be implemented in a pluggable loader which can
be registered via an entrypoint.")
    (license license:repoze)))

(define-public python-plaster-pastedeploy
  (package
    (name "python-plaster-pastedeploy")
    (version "0.7")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "plaster_pastedeploy" version))
              (sha256
               (base32
                "1zg7gcsvc1kzay1ry5p699rg2qavfsxqwl17mqxzr0gzw6j9679r"))))
    (build-system python-build-system)
    (native-inputs
     (list python-pytest))
    (propagated-inputs
     (list python-plaster python-pastedeploy))
    (home-page "https://github.com/Pylons/plaster_pastedeploy")
    (synopsis "Plugin for python-plaster adding PasteDeploy syntax")
    (description
     "This plugin for @code{python-plaster} adds support for PasteDeploy
syntax, it provides a plaster @code{Loader} object that can parse ini files
according to the standard set by PasteDeploy")
    (license license:expat)))

(define-public python-hupper
  (package
    (name "python-hupper")
    (version "1.10.3")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "hupper" version))
              (sha256
               (base32
                "1nbc648d110jx6ziji980cdmzsd14p8fqrcarsdvr1vm5jvm2vyd"))))
    (build-system python-build-system)
    (arguments '(#:test-target "pytest"))
    (native-inputs
     (list python-mock
           python-pytest
           python-pytest-cov
           python-pytest-runner
           python-setuptools
           python-watchdog
           python-wheel))
    (propagated-inputs
     (list python-pytz))
    (home-page "https://readthedocs.org/projects/hupper")
    (synopsis "Integrated process monitor tracking changes to imported Python files")
    (description
     "Hupper is an integrated process monitor that will track changes to any
imported Python files in sys.modules as well as custom paths.  When files are
changed the process is restarted.")
    (license license:expat)))

(define-public python-pyowm
  (package
    (name "python-pyowm")
    (version "3.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/csparpa/pyowm")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1ha4pp96y3jk33qnyir5851cnj4dc06q6wqn1b0w54l3fsds28vi"))))
    (build-system pyproject-build-system)
    (arguments (list #:test-flags #~(list "tests/unit")))
    (native-inputs (list python-pytest python-setuptools python-wheel))
    (propagated-inputs
     (list python-geojson-for-pyowm
           python-pysocks
           python-requests))
    (home-page "https://github.com/csparpa/pyowm")
    (synopsis "Python wrapper around OpenWeatherMap web APIs")
    (description
     "This package provides a Python wrapper around OpenWeatherMap web APIs.")
    (license license:expat)))

(define-public python-pyramid
  (package
    (name "python-pyramid")
    (version "2.0.2")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "pyramid" version))
              (sha256
               (base32
                "1phqdz068hl3zda263qkjp5am8fmvmpcxp3nrhsna8g472kkh89p"))))
    (build-system pyproject-build-system)
    (propagated-inputs
     (list python-hupper
           python-plaster
           python-plaster-pastedeploy
           python-setuptools
           python-translationstring
           python-venusian
           python-webob
           python-zope-component
           python-zope-deprecation
           python-zope-interface))
    (native-inputs (list python-pytest python-webtest python-wheel))
    (home-page "https://trypyramid.com/")
    (synopsis "Python web-framework suitable for small and large sites")
    (description
     "Pyramid makes it easy to write web applications.  From minimal
request/response web apps to larger, grown applications.")
    (license license:repoze)))

(define-public python-random-user-agent
  (package
    (name "python-random-user-agent")
    (version "1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "random_user_agent" version))
       (sha256
        (base32
         "04nhzdh2ki7ybhjrmghxci6hcm6i03vvin2q2ynj87fbr1pa534g"))))
    (build-system python-build-system)
    (home-page "https://github.com/Luqman-Ud-Din/random_user_agent")
    (synopsis "List of user agents")
    (description
     "This package provides a list of user agents, from a collection of more
than 326,000 known user-agents.  Users can pick a random one, or select one
based on filters.")
    (license license:expat)))

(define-public python-rangehttpserver
  (package
    (name "python-rangehttpserver")
    (version "1.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/danvk/RangeHTTPServer")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1dx533fsbq1mbz3by7qkm5bh2c253nwvm8214pqbwbfq07kqkwf2"))))
    (build-system pyproject-build-system)
    (native-inputs (list python-coverage python-coveralls python-pytest
                         python-setuptools python-wheel))
    (home-page "https://github.com/danvk/RangeHTTPServer")
    (synopsis "SimpleHTTPServer with support for range requests")
    (description
     "This tool provides a simple HTTP server with support for Range
requests.")
    (license license:asl2.0)))

(define-public python-flask-restx
  (package
    (name "python-flask-restx")
    (version "1.3.0")
    (source
     ;; We fetch from the Git repo because there are no tests in the PyPI
     ;; archive.
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/python-restx/flask-restx")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1qmm3i1cdv0bvzsc1gn4ql2dsf1fbx85fk69vcmzpsdxzczmw508"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-aniso8601 python-flask python-jsonschema python-pytz))
    (native-inputs
     (list python-blinker
           python-faker
           python-pytest
           python-pytest-benchmark
           python-pytest-flask
           python-pytest-mock))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (invoke "pytest" "--benchmark-skip" "-k"
                     ;; Those tests need internet access
                     (string-join
                      '("not test_check"
                        "not test_valid_value_check"
                        "not test_override_app_level"
                        "not test_redirect"
                        "not test_swagger")
                      " and ")))))))
    (home-page "https://github.com/python-restx/flask-restx")
    (synopsis
     "Framework for fast, easy and documented API development with Flask")
    (description
     "Flask-RESTX is an extension for Flask that adds support for quickly building
REST APIs.  Flask-RESTX encourages best practices with minimal setup.  If you are familiar
 with Flask, Flask-RESTX should be easy to pick up.  It provides a coherent collection of
decorators and tools to describe your API and expose its documentation properly using
Swagger.")
    (license license:bsd-3)))

(define-public python-flask-restplus
  (deprecated-package "python-flask-restplus" python-flask-restx))

(define-public python-flask-socketio
  (package
    (name "python-flask-socketio")
    (version "5.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "flask_socketio" version))
       (sha256
        (base32
         "1i0yqn39b4v4r2whppx47sg7iicvqzssd1clx7cclk07l52cjinr"))))
    (build-system pyproject-build-system)
    (propagated-inputs
     (list python-flask python-socketio))
    (native-inputs
     (list python-pytest python-redis python-setuptools python-wheel))
    (home-page "https://github.com/miguelgrinberg/Flask-SocketIO/")
    (synopsis "Socket.IO integration for Flask applications")
    (description "Socket.IO integration for Flask applications")
    (license license:expat)))

(define-public python-manuel
  (package
    (name "python-manuel")
    (version "1.13.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "manuel" version))
        (sha256
         (base32
          "0nl8psji3fizbxw34gdg22dndlg9lf5f56mnh1vr4vgsx06i4qsx"))))
    (build-system pyproject-build-system)
    (propagated-inputs
     (list python-setuptools))
    (native-inputs
     (list python-setuptools python-wheel python-zope-testing))
    (home-page "https://pypi.org/project/manuel/")
    (synopsis "Build tested documentation")
    (description
     "Manuel lets you mix and match traditional doctests with custom test syntax.")
    (license license:asl2.0)))

(define-public python-pecan
  (package
    (name "python-pecan")
    (version "1.4.1")
    (source
     (origin
       (method git-fetch)               ;no tests in pypi release
       (uri (git-reference
             (url "https://github.com/pecan/pecan")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "10lz0cqafx3j24m52vv9ph0bxrzyx6wv2dgz7g9kfm60lhaskqkh"))))
    (build-system python-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'relax-requirements
            (lambda _
              (substitute* "test-requirements.txt"
                ;; Drop extraneous virtualenv requirement.
                ((".*virtualenv.*") ""))))
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (invoke "pytest" "-vv" "pecan")))))))
    (native-inputs
     (list gunicorn
           python-genshi
           python-jinja2
           python-pytest
           python-sqlalchemy
           uwsgi))
    (propagated-inputs
     (list python-logutils
           python-mako
           python-six
           python-webob
           python-webtest))
    (home-page "http://github.com/pecan/pecan")
    (synopsis "WSGI object-dispatching web framework")
    (description "This package provides a WSGI object-dispatching web
framework, designed to be lean and fast, with few dependencies.")
    (license license:bsd-3)))

(define-public python-persistent
  (package
    (name "python-persistent")
    (version "4.6.4")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "persistent" version))
        (sha256
         (base32
          "0imm9ji03lhkpcfmhid7x5209ix8g2rlgki9ik1qxks4b8sm8gzq"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-cffi python-zope-interface))
    (native-inputs
     (list python-manuel python-zope-testrunner))
    (home-page "https://github.com/zopefoundation/persistent/")
    (synopsis "Translucent persistent objects")
    (description "This package contains a generic persistence implementation for
Python.  It forms the core protocol for making objects interact
\"transparently\" with a database such as the ZODB.")
    (license license:zpl2.1)))

(define-public python-btrees
  (package
    (name "python-btrees")
    (version "4.7.2")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "BTrees" version))
        (sha256
         (base32
          "0iiq0g9k1g6qgqq84q9h6639vlvzznk1rgdm0rfcnnqkbkmsbr3w"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-persistent python-zope-interface))
    (native-inputs
     (list python-persistent python-transaction python-zope-testrunner))
    (home-page "https://github.com/zopefoundation/BTrees")
    (synopsis "Scalable persistent object containers")
    (description
     "This package contains a set of persistent object containers built around a
modified BTree data structure.  The trees are optimized for use inside ZODB's
\"optimistic concurrency\" paradigm, and include explicit resolution of
conflicts detected by that mechanism.")
    (license license:zpl2.1)))

(define-public python-transaction
  (package
    (name "python-transaction")
    (version "3.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "transaction" version))
        (sha256
         (base32
          "0bdaks31bgfh78wnj3sij24bfysmqk25crsis6amz8kzrc0d82iv"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-zope-interface))
    (native-inputs
     (list python-coverage python-mock python-nose))
    (home-page "https://github.com/zopefoundation/transaction")
    (synopsis "Transaction management for Python")
    (description "This package contains a generic transaction implementation
for Python.  It is mainly used by the ZODB.")
    (license license:zpl2.1)))

(define-public python-robot-detection
  (package
    (name "python-robot-detection")
    (version "0.4")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "robot-detection" version))
        (sha256
         (base32
          "1xd2jm3yn31bnk1kqzggils2rxj26ylxsfz3ap7bhr3ilhnbg3rx"))))
    (build-system python-build-system)
    (arguments '(#:tests? #f)) ; Tests not shipped in pypi release.
    (propagated-inputs (list python-six))
    (home-page "https://github.com/rory/robot-detection")
    (synopsis "Detect web crawlers")
    (description
     "@code{robot_detection} is a python module to detect if a given HTTP User
Agent is a web crawler.  It uses the list of registered robots from
@url{http://www.robotstxt.org}.")
    (license license:gpl3+)))

(define-public python-robotframework-requests
  (package
    (name "python-robotframework-requests")
    (version "0.9.7")
    (source
     (origin
       (method git-fetch)               ; no tests in PyPI release
       (uri (git-reference
             (url "https://github.com/MarketSquare/robotframework-requests")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "077j8p5k41v53slyv8h32fcmqfi7m6z3r4gmyqqaawm5szfmy61m"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-pytest
           python-setuptools
           python-wheel))
    (propagated-inputs
     (list python-requests
           python-robotframework))
    (home-page "https://github.com/MarketSquare/robotframework-requests")
    (synopsis "Robot Framework keyword library wrapper around requests")
    (description
     "@code{RequestsLibrary} is a @url{https://robotframework.org/, Robot
Framework} library aimed to provide HTTP API testing functionalities by
wrapping the @code{requests} Python library.")
    (license license:expat)))

(define-public python-pysolr
  (package
    (name "python-pysolr")
    (version "3.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pysolr" version))
       (sha256
        (base32 "02gl1sma5cnpd7srpri1g68lpsg3s5n3lr462p5ll8v9s4nllyqj"))))
    (build-system pyproject-build-system)
    (arguments
     '(#:tests? #f)) ; Tests require network access.
    (native-inputs
     (list python-setuptools
           python-setuptools-scm
           python-wheel))
    (propagated-inputs
     (list python-requests))
    (home-page "https://github.com/django-haystack/pysolr/")
    (synopsis "Lightweight python wrapper for Apache Solr")
    (description
     "This module provides an interface that queries the Apache Solr server
using a pure Python implementation.")
    (license license:bsd-3)))

(define-public python-pyjsparser
  (package
    (name "python-pyjsparser")
    (version "2.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pyjsparser" version))
       (sha256
        (base32 "0ycmf9fsvwliqmm1n6sfz7x71y7i2kbfgn39d8lsbiccfxmxlq5y"))))
    (build-system python-build-system)
    (home-page "https://github.com/PiotrDabkowski/pyjsparser")
    (synopsis "Fast JavaScript parser")
    (description "This package provides a fast JavaScript parser (based on
esprima.js)")
    (license license:expat)))

(define-public python-js2py
  (package
    (name "python-js2py")
    (version "0.71")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "Js2Py" version))
       (sha256
        (base32 "1kkzkys6dfcbdv51vqxr9cmak350ab4mmykb8dysx60lvl4i06x4"))))
    (build-system python-build-system)
    (arguments '(#:tests? #false)) ; none included
    (propagated-inputs
     (list python-pyjsparser python-six python-tzlocal))
    (home-page "https://github.com/PiotrDabkowski/Js2Py")
    (synopsis "JavaScript to Python translator")
    (description
     "This package provides a JavaScript to Python translator and a JavaScript
interpreter written in pure Python.")
    (license license:expat)))

(define-public python-http-ece
  (package
    (name "python-http-ece")
    (version "1.2.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/web-push-libs/encrypted-content-encoding")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "11v9cpmx4jc0nk25af96plzjz3mg1ysm1m001nx6jns06hil1gpy"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'change-directory
           (lambda _ (chdir "python") #t)))))
    (propagated-inputs
     (list python-cryptography))
    (native-inputs
     (list python-coverage python-flake8 python-mock python-nose))
    (home-page "https://github.com/web-push-libs/encrypted-content-encoding")
    (synopsis "Encrypted Content Encoding for HTTP")
    (description
     "This package provides a simple implementation of Encrypted Content
Encoding for HTTP.")
    (license license:expat)))

(define-public python-cloud-init
  (package
    (name "python-cloud-init")
    (version "24.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/canonical/cloud-init")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "18872z2y9wkh558y1bx5r0rksb4i584jbc8z4g8marwawhwxq506"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      '(list
        ;; This requires usermod
        "--ignore=tests/unittests/distros/test_create_users.py"
        ;; This writes to /var
        "--ignore=tests/unittests/net/test_dhcp.py"
        "-k"
        (string-append
         ;; This test messes with PATH, so it cannot find mkdir
         "not test_path_env_gets_set_from_main"
         " and not test_apt_configure_sources_list_"
         ;; These all fail because /bin/sh doesn't exist.  We cannot patch
         ;; this because the generated scripts must use /bin/sh as they are
         ;; supposed to be run on minimal systems.
         " and not test_handler_creates_and_runs_bootcmd_script_with_instance_id"
         " and not test_handler_runs_bootcmd_script_with_error"
         " and not test_subp_combined_stderr_stdout"
         " and not test_handle_part"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-references
            (lambda _
              (substitute* "tests/unittests/cmd/test_clean.py"
                (("#!/bin/sh") (string-append "#!" (which "sh"))))))
          (add-after 'install 'move-files
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (for-each (lambda (dir)
                          (let ((source (string-append (site-packages inputs outputs) "/" dir))
                                (target (string-append #$output "/" (basename dir))))
                            (copy-recursively source target)
                            (delete-file-recursively source)))
                        (list "etc" "lib" "usr/lib" "usr/share")))))))
    (propagated-inputs
     (list python-configobj
           python-jinja2
           python-jsonpatch
           python-jsonschema
           python-netifaces
           python-oauthlib
           python-pyserial
           python-pyyaml
           python-responses))
    (native-inputs
     (list procps ;for ps when running tests
           python-pytest
           python-pytest-cov
           python-pytest-mock
           python-passlib
           python-setuptools
           python-wheel))
    (home-page "https://github.com/canonical/cloud-init")
    (synopsis "Cloud instance initialization tools")
    (description
     "Cloud-init is the multi-distribution method for cross-platform cloud
instance initialization.  It is supported across all major public cloud
providers, provisioning systems for private cloud infrastructure, and
bare-metal installations.")
    ;; Either license can be chosen
    (license (list license:asl2.0 license:gpl3))))

(define-public python-cloudscraper
  (package
    (name "python-cloudscraper")
    (version "1.2.60")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/VeNoMouS/cloudscraper")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "00cmxgwdm0x1j4a4ipwvpzih735hdzidljbijk1b3laj3dgvnvsm"))
       (modules '((guix build utils)))
       (snippet
        '(with-directory-excursion "cloudscraper"
           (for-each delete-file
                     '("captcha/9kw.py"
                       "captcha/anticaptcha.py"))
           (substitute* "__init__.py"
             ;; Perhaps it's a joke, but don't promote proprietary software.
             (("([Th]is feature is not available) in the .*'" _ prefix)
              (string-append prefix ".'")))))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; XXX: Dependencies, that have not yet been packaged
         ;;      and cause an import error when included.
         (add-after 'unpack 'drop-unsupported-sources
           (lambda _
             (with-directory-excursion "cloudscraper"
               (for-each delete-file
                         '("interpreters/v8.py")))))
         (add-after 'unpack 'fix-references
           (lambda _
             (substitute* "cloudscraper/interpreters/nodejs.py"
               (("'node'")
                (string-append "'" (which "node") "'")))))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "pytest" "-vv"
                       "-k" "not test_getCookieString_challenge_js_challenge1_16_05_2020")))))))
    (inputs
     (list node-lts))
    (propagated-inputs
     (list python-js2py
           python-polling2
           python-requests
           python-requests-toolbelt
           python-responses
           python-pyparsing))
    (native-inputs
     (list python-pytest))
    (home-page "https://github.com/venomous/cloudscraper")
    (synopsis "Cloudflare anti-bot bypass")
    (description
     "This module acts as a webbrowser solving Cloudflare's Javascript
challenges.")
    (license license:expat)))

(define-public python-imap-tools
  (package
    (name "python-imap-tools")
    (version "1.9.0")
    (source
     (origin
       (method git-fetch)               ; no tests in PyPI release
       (uri (git-reference
             (url "https://github.com/ikvk/imap_tools")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0iawc6shxn8p0ysiqiahfic5rp1r16f7rfn11c8y2fl8falckynr"))))
    (arguments
     (list
      #:test-flags
      ;; Tests require a network connection
      #~(list "-k" (string-append "not test_action"
                                  " and not test_attributes"
                                  " and not test_connection"
                                  " and not test_folders"
                                  " and not test_idle"
                                  " and not test_live"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-pytest python-setuptools python-wheel))
    (home-page "https://github.com/ikvk/imap_tools")
    (synopsis "Work with email and mailbox by IMAP")
    (description
      "This Python library provides tools to deal with email and mailboxes
over IMAP:

@itemize
@item Parsed email message attributes
@item Query builder for searching emails
@item Work with emails in folders (copy, delete, flag, move, seen)
@item Work with mailbox folders (list, set, get, create, exists, rename, delete, status)
@end itemize")
    (license license:asl2.0)))

(define-public python-giturlparse
  (package
    (name "python-giturlparse")
    (version "0.10.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "giturlparse" version))
        (sha256
         (base32 "0dxk7sqy8ibaqdl6jzxd0ac1p7shzp4p9f3lhkd7qw9h3llsp595"))))
    (build-system python-build-system)
    (home-page "https://github.com/nephila/giturlparse")
    (synopsis "Git URL parsing module")
    (description "This package provides a git URL parsing module which supports
parsing and rewriting of remote git URLs from various hosting providers.")
    (license license:asl2.0)))

(define-public python-hstspreload
  (package
    (name "python-hstspreload")
    (version "2020.10.20")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "hstspreload" version))
       (sha256
        (base32
         "1qah80p2xlib1rhivvdj9v5y3girxrj7dwp1mnh8mwaj5wy32y8a"))))
    (build-system python-build-system)
    (home-page
     "https://github.com/sethmlarson/hstspreload")
    (synopsis
     "Chromium HSTS Preload list as a Python package")
    (description
     "@code{python-hstspreload} contains Chromium HSTS Preload list
as a Python package.")
    (license license:bsd-3)))

(define-public python-sanic-routing
  (package
    (name "python-sanic-routing")
    (version "23.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "sanic-routing" version))
       (sha256
        (base32 "014bhkharcgyprdvlk7wjxhjp1pr0cva1nwj4f2qrr23qiidrjhx"))))
    (build-system pyproject-build-system)
    (arguments
     ;; PyPi sources does not contain tests, recursive dependency on
     ;; python-sanic.
     (list #:tests? #f))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://github.com/sanic-org/sanic-routing/")
    (synopsis "Routing component for Sanic")
    (description "Internal package for @code{python-sanic}, which provides
the @code{BasicRouter}.")
    (license license:expat)))

(define-public python-sanic-testing
  (package
    (name "python-sanic-testing")
    (version "23.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "sanic-testing" version))
       (sha256
        (base32 "0gqkzv90sbj9rw7yfly9c58lq9dq30g5hhcgjl3ihzjb66vm571b"))))
    (build-system pyproject-build-system)
    (arguments
     ;; PyPi sources does not contain tests, recursive dependency on
     ;; python-sanic.
     (list #:tests? #f))
    (propagated-inputs (list python-httpx python-sanic-bootstrap))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://github.com/sanic-org/sanic-testing/")
    (synopsis "Test clients for Sanic")
    (description "Internal package for @code{python-sanic}, which is
meant to be the core testing utility and clients for testing Sanic
applications.")
    (license license:expat)))

(define-public python-sanic
  (package
    (name "python-sanic")
    (version "23.12.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "sanic" version))
       (sha256
        (base32
         "115vnir4qijv89139g5h0i4l0n4w3bgh1ickgnk8xidxsa0wla15"))))
    (build-system pyproject-build-system)
    (arguments
      (list
       #:test-flags
       '(list "-k"
              (string-append
               ;; PyPi sources lack examples module.
               "not test_gunicorn_"
               ;; Does not expect brotli and reordered headers.
               " and not test_raw_headers"
               ;; These look like buggy testcases.
               " and not test_zero_downtime"
               " and not test_non_default_uvloop_config_raises_warning"
               " and not test_listeners_triggered"
               " and not test_keep_alive_connection_context"
               " and not test_keep_alive_client_timeout"
               ;; Unclear why they fail since core-updates merge.
               " and not test_missing_sni"
               " and not test_no_matching_cert"
               " and not test_wildcards"
               ;; These tests fail because subprotocols appear to be
               ;; parameterized as None
               " and not test_websocket_route_with_subprotocols"
               ;; AF_UNIX path too long
               " and not test_setup_and_teardown_unix"
               " and not test_configure_socket"
               ;; Freezes
               " and not test_server_run_with_repl"))
       #:phases
       #~(modify-phases %standard-phases
           (replace 'check
             (lambda* (#:key tests? test-flags #:allow-other-keys)
               (when tests?
                 (apply invoke "pytest" "-vv" "./tests"
                        test-flags)))))))
    (propagated-inputs
     (list python-aiofiles
           python-aioquic
           python-html5tagger
           python-httptools
           python-multidict
           python-sanic-routing
           python-tracerite
           python-typing-extensions
           python-ujson
           python-uvloop
           python-websockets))
    (native-inputs
     (list python-bandit
           python-beautifulsoup4
           python-chardet
           python-coverage
           python-cryptography
           python-docutils
           python-mypy
           python-pygments
           python-pytest
           python-pytest-benchmark
           python-pytest-sanic
           python-sanic-testing
           python-setuptools
           python-slotscheck
           python-towncrier
           python-tox
           python-types-ujson
           python-uvicorn
           python-wheel))
    (home-page "https://github.com/sanic-org/sanic/")
    (synopsis "Async Python web server/framework")
    (description
     "Sanic is a Python web server and web framework that's written to go
fast.  It allows the usage of the @code{async/await} syntax added in Python
3.5, which makes your code non-blocking and speedy.")
    (license license:expat)))

(define-public python-sanic-bootstrap
  (package
     (inherit python-sanic)
     (name "python-sanic-bootstrap")
     (arguments (list #:tests? #f))
     (native-inputs (list python-setuptools python-wheel))))

(define-public python-socketio
  (package
    (name "python-socketio")
    (version "5.12.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "python_socketio" version))
       (sha256
        (base32
         "175sd3fdld3s477i6b3v49yhf1alsbm1vaxzq44nqrqb8wgzz682"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:test-flags
           '(list "--timeout=60"
                  ;; These tests freeze.
                  "--ignore-glob=tests/async/*")))
    (propagated-inputs
     (list python-bidict python-engineio python-msgpack))
    (native-inputs
     (list python-pytest
           python-pytest-asyncio
           python-pytest-timeout
           python-setuptools
           python-uvicorn
           python-wheel))
    (home-page "https://github.com/miguelgrinberg/python-socketio/")
    (synopsis "Python Socket.IO server")
    (description
     "This package provides a Python implementation of the @code{Socket.IO}
realtime client and server.")
    (license license:expat)))

(define-public python-socks
  (package
    (name "python-socks")
    (version "2.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "python_socks" version))
       (sha256
        (base32
         "17pcyd1gg2a7k2i4lvnqi9095brhf7pbpkqfcjmhpzkgmfd94hwp"))))
    (build-system pyproject-build-system)
    (propagated-inputs
     (list python-trio))
    (native-inputs
     (list python-anyio
           python-async-timeout
           python-flask
           python-pytest
           python-pytest-asyncio
           python-setuptools
           python-tiny-proxy
           python-trustme
           python-wheel
           python-yarl))
    (home-page "https://github.com/romis2012/python-socks")
    (synopsis
     "Core proxy (SOCKS4, SOCKS5, HTTP tunneling) functionality for Python")
    (description
     "Socks is a library providing core proxy (SOCKS4, SOCKS5, HTTP tunneling)
 functionality.")
    (license license:asl2.0)))

(define-public python-socksio
  (package
    (name "python-socksio")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "socksio" version))
       (sha256
        (base32 "1b64j45w35jzhjhnq44bnyad9ydh1iyyd7a6j2c8phxmllyyp2zq"))))
    (build-system python-build-system)
    (home-page "https://github.com/sethmlarson/socksio")
    (synopsis "Sans-I/O implementation of SOCKS4, SOCKS4A, and SOCKS5")
    (description "The @code{socksio} Python module is a client-side sans-I/O
SOCKS proxy implementation.  It supports SOCKS4, SOCKS4A, and SOCKS5.
@code{socksio} is a sans-I/O library similar to @code{h11} or @code{h2}; this
means the library itself does not handle the actual sending of the bytes
through the network, it only deals with the implementation details of the
SOCKS protocols.  It can be paired with any I/O library.")
    (license license:expat)))

(define-public python-simple-websocket
  (package
    (name "python-simple-websocket")
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "simple_websocket" version))
       (sha256
        (base32 "1r1all55d5dimiw3qwdhj53cxj9yjgnskcysmcscarx0g9726fbr"))))
    (build-system pyproject-build-system)
    (arguments
     ;; Requires Internet access to resolve example.com
     (list #:test-flags '(list "--ignore=tests/test_client.py")))
    (propagated-inputs (list python-wsproto))
    (native-inputs (list python-flake8
                         python-pytest
                         python-pytest-cov
                         python-setuptools
                         python-tox
                         python-wheel))
    (home-page "https://github.com/miguelgrinberg/simple-websocket")
    (synopsis "Simple WebSocket server and client for Python")
    (description "This package provides a simple @code{WebSocket} server and
client for Python.")
    (license license:expat)))

(define-public python-siosocks
  (package
    (name "python-siosocks")
    (version "0.3.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "siosocks" version))
              (sha256
               (base32
                "0qqxy8wl5mrmlkblzjq9nsg0cbm5jwgj409mhnhq6gd1ypvbndms"))))
    (build-system python-build-system)
    (arguments
     (list #:phases #~(modify-phases %standard-phases
                        (replace 'check
                          (lambda* (#:key tests? #:allow-other-keys)
                            (when tests?
                              (invoke "pytest" "-vvv")))))))
    (native-inputs (list python-pytest python-pytest-asyncio python-pytest-cov
                         python-pytest-trio))
    (propagated-inputs (list python-trio))
    (home-page "https://github.com/pohmelie/siosocks")
    (synopsis "SOCKSv4 & SOCKSv5 TCP proxy protocol implementation in Python")
    (description
     "This package provides a Python module and framework for sans-io socks proxy
client/server with couple io backends.

Features:
@itemize
@item Only TCP connect (no BIND, no UDP)
@item Both client and server
@item SOCKS versions: 4, 4a, 5
@item SOCKSv5 auth: no auth, username/password
@item Couple io backends: @code{asyncio}, @code{trio}, @code{socketserver}
@item One-shot socks server (@code{python -m siosocks})
@end itemize")
    (license license:expat)))

(define-public python-tiny-proxy
  (package
    (name "python-tiny-proxy")
    (version "0.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "tiny_proxy" version))
       (sha256
        (base32 "05nq62fn3p7s1a6dkdvnrzmk39d9ddrzb3pqbbli025qs3m9j1bq"))))
    (build-system pyproject-build-system)
    ;; There are only a handful of tests and they require the as yet
    ;; unpackaged httpx_socks.
    (arguments (list #:tests? #false))
    (propagated-inputs (list python-anyio))
    (native-inputs
     (list python-httpx
           python-pytest
           python-setuptools
           python-wheel))
    (home-page "https://github.com/romis2012/tiny-proxy")
    (synopsis "Simple proxy server (SOCKS4(a), SOCKS5(h), HTTP tunnel)")
    (description "This package provides a simple proxy server (SOCKS4(a),
SOCKS5(h), HTTP tunnel).")
    (license license:asl2.0)))

(define-public python-azure-nspkg
  (package
    (name "python-azure-nspkg")
    (version "3.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "azure-nspkg" version ".zip"))
       (sha256
        (base32
         "1l4xwdh0fcnvrv0mzig4g2kgqkfbsy64zjm1ggc6grk3mykcxlz7"))))
    (build-system python-build-system)
    (native-inputs (list unzip))
    (home-page "https://github.com/Azure/azure-sdk-for-python")
    (synopsis "Azure namespace internals")
    (description
     "This package is an internal Azure namespace package.")
    (license license:expat)))

(define-public python-azure-storage-nspkg
  (package
    (name "python-azure-storage-nspkg")
    (version "3.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "azure-storage-nspkg" version))
       (sha256
        (base32
         "049qcmgshz7dj9yaqma0fwcgbxwddgwyfcw4gmv45xfmaa3bwfvg"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-azure-nspkg))
    (home-page "https://github.com/Azure/azure-storage-python")
    (synopsis "Microsoft Azure Storage Namespace package")
    (description
     "This project provides a client library in Python that makes it easy to
communicate with Microsoft Azure Storage services.")
    (license license:expat)))

(define-public python-azure-common
  (package
    (name "python-azure-common")
    (version "1.1.28")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "azure-common" version ".zip"))
       (sha256
        (base32 "18q4cy1xl2zly3rk7a1sc14w932x59r8c9j4d8dnlsz32hrcvh2a"))))
    (build-system python-build-system)
    (propagated-inputs (list python-azure-nspkg))
    (native-inputs (list unzip))
    (home-page "https://github.com/Azure/azure-sdk-for-python")
    (synopsis "Microsoft Azure Client library for Python")
    (description "This package provides the Microsoft Azure Client library for
Python.")
    (license license:expat)))

(define-public python-azure-core
  (package
    (name "python-azure-core")
    (version "1.32.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "azure_core" version))
       (sha256
        (base32 "1r9hqyqr5fxiiai0irr0n98gwgzj5f8y46vc1yci9bidddfw7cr2"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      `(list ;; This fails because devtools_testutils doesn't exist.
        "--ignore=tests/test_connection_string_parsing.py"
        ;; These all need network access.
        "--ignore=samples"
        "--ignore=tests/async_tests/test_streaming_async.py"
        "--ignore=tests/test_streaming.py"
        "-m" "not asyncio and not live_test_only"
        "-k" ,(string-append
               "not test_decompress_plain_no_header"
               " and not test_compress_plain_no_header"
               " and not test_decompress_compressed_no_header"
               " and not test_requests_socket_timeout"))
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'add-test-pythonpath
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (setenv "PYTHONPATH"
                        (string-append
                         (getcwd) "/tests/testserver_tests/coretestserver:"
                         (getenv "GUIX_PYTHONPATH")))))))))
    (propagated-inputs
     (list python-aiohttp
           python-requests
           python-six
           python-typing-extensions))
    (native-inputs
     (list python-flask
           python-pytest
           python-pytest-aiohttp
           python-pytest-asyncio
           python-pytest-trio
           python-setuptools
           python-wheel))
    (home-page "https://github.com/Azure/azure-sdk-for-python")
    (synopsis "Microsoft Azure Core library for Python")
    (description "This package provides the Microsoft Azure Core library for
Python.")
    (license license:expat)))

(define-public python-azure-storage-blob
  (package
    (name "python-azure-storage-blob")
    (version "12.22.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "azure-storage-blob" version))
       (sha256
        (base32 "0vkkngiybx5372j9vc9p4wn6hakpv99l0ipsf4kw7ccazss4p05k"))))
    (build-system pyproject-build-system)
    ;; To activate tests python-pytest is required and missing
    ;; <https://raw.githubusercontent.com/Azure/azure-sdk-for-python/refs/
    ;; heads/main/tools/azure-sdk-tools/devtools_testutils/README.md>
    (native-inputs
     (list python-setuptools
           python-wheel))
    (propagated-inputs
     (list python-azure-core
           python-cryptography
           python-isodate
           python-typing-extensions))
    (home-page "https://github.com/Azure/azure-sdk-for-python/")
    (synopsis "Microsoft Azure Blob Storage client library for Python")
    (description "This package provides the Microsoft Azure Blob Storage
Client Library for Python.")
    (license license:expat)))

(define-public python-google-crc32c
  (package
    (name "python-google-crc32c")
    (version "1.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "google-crc32c" version))
       (sha256
        (base32 "1ps4qaccij6cixs91772y9f9r1n702yfzy4qqmds6x5h7qkycv97"))))
    (build-system python-build-system)
    (native-inputs (list python-pytest))
    (home-page "https://github.com/googleapis/python-crc32c")
    (synopsis "Python wrapper of Google CRC32C")
    (description
     "This package provides a Python wrapper of the C library implementation
of the CRC32C hashing algorithm.")
    (license license:asl2.0)))

(define-public python-google-auth
  (package
    (name "python-google-auth")
    (version "2.37.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "google_auth" version))
       (sha256
        (base32 "002xvp1139lrw2c2s2yllg5499ghfxzg9lv35i4q770zpwx64m00"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      ;; This one test uses crypto.sign with an outdated calling convention.
      '(list "--ignore=tests/transport/test__mtls_helper.py")))
    (propagated-inputs
     (list python-cachetools
           python-pyasn1-modules
           python-pyjwt
           python-rsa))
    (native-inputs
     (list nss-certs-for-test
           python-aioresponses
           python-cryptography
           python-flask
           python-freezegun
           python-mock
           python-pyopenssl
           python-pytest
           python-pytest-asyncio
           python-pytest-localserver
           python-pyu2f
           python-requests
           python-responses
           python-setuptools
           python-wheel))
    (home-page "https://github.com/googleapis/google-auth-library-python")
    (synopsis "Google Authentication Library")
    (description "This library simplifies using Google's various
server-to-server authentication mechanisms to access Google APIs.")
    (license license:asl2.0)))

(define-public python-google-resumable-media
  (package
    (name "python-google-resumable-media")
    (version "2.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "google-resumable-media" version))
       (sha256
        (base32 "04qm6rd4mpbbym8ci5xrb6fymc3mmm8x2z9f43q5iwbr3s5lx4h6"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               ;; The system tests fail to find test_utils.retry.
               (delete-file-recursively "tests/system/")
               (invoke "pytest")))))))
    (propagated-inputs (list python-google-crc32c))
    (native-inputs
     (list python-google-auth
           python-mock
           python-pytest
           python-requests
           python-test-utils))
    (home-page "https://github.com/googleapis/google-resumable-media-python")
    (synopsis "Utilities for Google Media Downloads and Resumable Uploads")
    (description "This package provides utilities for Google Media Downloads
and Resumable Uploads.")
    (license license:asl2.0)))

(define-public python-googleapis-common-protos
  (package
    (name "python-googleapis-common-protos")
    (version "1.56.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "googleapis-common-protos" version))
       (sha256
        (base32 "05s4dszqd5pjwjh4bdic40v1v447k0a3dynsrgypqf3rfb276n62"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
        (modify-phases %standard-phases
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (invoke "pytest")))))))
    (propagated-inputs (list python-protobuf))
    (native-inputs
     (list python-pytest))
    (home-page "https://github.com/googleapis/python-api-common-protos")
    (synopsis "Common protobufs used in Google APIs")
    (description "This package contains Python classes generated from the
common protos in the @code{googleapis/api-common-protos} repository.")
    (license license:asl2.0)))

(define-public python-google-api-core
  (package
    (name "python-google-api-core")
    (version "2.7.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "google-api-core" version))
       (sha256
        (base32 "0ydwvg9gzp75cd11s62db5w3jhj643yrw095rv95psfb0h3pz58p"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               ;; These don't work because it doesn't find AsyncMock even when
               ;; we add it to the inputs.
               (for-each delete-file
                         '("tests/asyncio/test_page_iterator_async.py"
                           "tests/asyncio/test_retry_async.py"))
               (invoke "pytest")))))))
    (propagated-inputs
     (list python-google-auth
           python-googleapis-common-protos
           python-protobuf
           python-proto-plus
           python-requests))
    (native-inputs
     (list python-mock
           python-pytest
           python-pytest-asyncio))
    (home-page "https://github.com/googleapis/python-api-core")
    (synopsis "Google API client core library")
    (description "This library defines common helpers used by all Google API
clients.")
    (license license:asl2.0)))

(define-public python-google-cloud-core
  (package
    (name "python-google-cloud-core")
    (version "2.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "google-cloud-core" version))
       (sha256
        (base32 "0sa66kidgr32dfq9ngha9l362xnqvnqqmssn5my1gd3lc6g65apx"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "pytest")))))))
    (propagated-inputs (list python-google-api-core python-google-auth))
    (native-inputs
     (list python-grpcio python-pytest))
    (home-page "https://github.com/googleapis/python-cloud-core")
    (synopsis "Google Cloud API client core library")
    (description "This library defines common helpers (e.g. base @code{Client}
classes) used by all of the @code{google-cloud-*} packages.")
    (license license:asl2.0)))

(define-public python-google-cloud-storage
  (package
    (name "python-google-cloud-storage")
    (version "2.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "google-cloud-storage" version))
       (sha256
        (base32 "0nwg9ic29s70kpvi71gmjv1y4w5a3vc9gj6d16f8w8hpbvgb1jzl"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (for-each delete-file-recursively
                         (list
                          ;; The system tests fail to find test_utils.retry.
                          "tests/system/"
                          ;; Needs docker.
                          "tests/conformance/"))
               (invoke "pytest")))))))
    (native-inputs
     (list python-mock python-pytest python-test-utils))
    (propagated-inputs
     (list python-google-api-core
           python-google-auth
           python-google-cloud-core
           python-google-resumable-media
           python-protobuf
           python-requests))
    (home-page "https://github.com/googleapis/python-storage")
    (synopsis "Google Cloud Storage API client library")
    (description "Google Cloud Storage allows you to store data on Google
infrastructure, and it can be used to distribute large data objects to users
via direct download.  This package provides a Google Cloud Storage API client
library for Python.")
    (license license:asl2.0)))

(define-public python-smart-open
  (package
    (name "python-smart-open")
    (version "6.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "smart_open" version))
       (sha256
        (base32 "1c12ilanx9hgpcc5chjkaqnx1hx14iazyindy7syvjhbdywhc0fn"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #false ;none included
       #:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "pytest")))))))
    (propagated-inputs
     (list python-azure-common
           python-azure-core
           python-azure-storage-blob
           python-boto3
           python-google-cloud-storage
           python-requests))
    (native-inputs
     (list python-flask
           python-flask-cors
           python-graphql-core
           python-moto
           python-paramiko
           python-pathlib2
           python-pytest
           python-pytest-rerunfailures
           python-responses))
    (home-page "https://github.com/piskvorky/smart_open")
    (synopsis "Utilities for streaming large files")
    (description
     "This package provides utilities for streaming large files (S3, HDFS,
GCS, Azure Blob Storage, gzip, bz2, etc.)")
    (license license:expat)))

(define-public python-w3lib
  (package
    (name "python-w3lib")
    (version "2.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "w3lib" version))
       (sha256
        (base32
         "1cd4b3w5g3pfccsg79kjj27fwi216ip927rjq7isp8pfjzlp8nzd"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-pytest python-setuptools python-wheel))
    (home-page "https://github.com/scrapy/w3lib")
    (synopsis "Python library of web-related functions")
    (description
     "This is a Python library of web-related functions, such as: remove
comments, or tags from HTML snippets, extract base url from HTML snippets,
translate entities on HTML strings, among other things.")
    (license license:bsd-3)))

(define-public python-webcolors
  (package
    (name "python-webcolors")
    (version "24.11.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "webcolors" version))
       (sha256
        (base32 "1xl0vn4xa03vjwx6fj19q9kgb94g65gvdf3p0ivsy0i2ydldgczc"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      '(modify-phases %standard-phases
         (add-after 'unpack 'patch-pyproject
           (lambda _
             ;; XXX Our version of python-pdm-backend does not recognize
             ;; "dependency-groups", but is fine with the bogus
             ;; "tool.whatever".
             (substitute* "pyproject.toml"
               (("\\[dependency-groups\\]") "[tool.whatever]")))))))
    (native-inputs
     (list python-pdm-backend python-pytest))
    (home-page "https://github.com/ubernostrum/webcolors")
    (synopsis "HTML/CSS color definitions library")
    (description "@code{python-webcolors} is a module for working with
HTML/CSS color definitions.  Normalizing and converting between the following
formats is supported.
@itemize
@item Specification-defined color names
@item Six-digit hexadecimal
@item Three-digit hexadecimal
@item Integer rgb() triplet
@item Percentage rgb() triplet
@end itemize
Only the RGB colorspace is supported.  Conversion to/from the HSL colorspace
can be handled by the @code{colorsys} module in the Python standard library.")
    (license license:bsd-3)))

(define-public python-webcolors-24
  (package
    (inherit python-webcolors)
    (name "python-webcolors")
    (version "24.11.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "webcolors" version))
       (sha256
        (base32 "1xl0vn4xa03vjwx6fj19q9kgb94g65gvdf3p0ivsy0i2ydldgczc"))))
    (build-system pyproject-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-build-error
           (lambda _
             ;; pdm wants optional-dependencies instead of dependency-groups.
             ;; See <https://pdm-project.org/en/latest/usage/dependency/>.
             (substitute* "pyproject.toml"
              (("\\[dependency-groups\\]")
               "[project.optional-dependencies]"))))
         (replace 'check
           (lambda* (#:key tests? inputs outputs #:allow-other-keys)
             (when tests?
               (add-installed-pythonpath inputs outputs)
               ;; Our python-nox version is incompatible,
               ;; so use pytest instead.
               (invoke "pytest")))))))
    (native-inputs (list python-pdm-backend python-pytest))
    (license license:bsd-3)))

(define-public python-woob
  (package
    (name "python-woob")
    (version "3.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "woob" version))
       (sha256
        (base32 "09hpxy5zhn2b8li0xjf3zd7s46lawb0315p5mdcsci3bj3s4v1j7"))))
    (build-system python-build-system)
    ;; A small number of tests for optional applications fails due to missing
    ;; inputs.
    (arguments `(#:tests? #f))
    (propagated-inputs
     (list python-babel
           python-colorama
           python-cssselect
           python-dateutil
           python-feedparser
           python-html2text
           python-lxml
           python-pillow
           python-prettytable
           python-pyqt
           python-pyyaml
           python-requests
           python-six
           python-unidecode))
    (native-inputs
     (list python-coverage python-flake8 python-nose python-selenium
           python-xunitparser))
    (home-page "https://woob.tech/")
    (synopsis "Woob, Web Outside Of Browsers")
    (description "Woob is a collection of applications able to interact with
websites, without requiring the user to open them in a browser.  It also
provides well-defined APIs to talk to websites lacking one.")
    (license license:lgpl3+)))

(define-public python-flask-combo-jsonapi
  (package
    (name "python-flask-combo-jsonapi")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/AdCombo/flask-combo-jsonapi")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "07fhcjiyif80z1vyh35za29sqx1mmqh568jrbrrs675j4a797sj1"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'loosen-requirements
           (lambda _
             ;; Don't pin specific versions of dependencies.
             (substitute* "requirements.txt"
               (("^sqlalchemy[=<>].*") "sqlalchemy\n")
               (("^marshmallow[=<>].*") "marshmallow\n")
               (("^Flask[=<>].*") "Flask\n"))))
         (replace 'check
           (lambda _
             (invoke "pytest" "-vv" "-k"
                     ;; The following test fails for unknown reasons (see:
                     ;; https://github.com/AdCombo/flask-combo-jsonapi/issues/66).
                     "not test_get_list_with_simple_filter_\
relationship_custom_qs_api"))))))
    (propagated-inputs
     (list python-flask
           python-marshmallow
           python-marshmallow-jsonapi
           python-simplejson
           python-sqlalchemy
           python-apispec
           python-simplejson
           python-six))
    (native-inputs
     (list python-coverage
           python-coveralls
           python-pytest
           python-pytest-runner))
    (home-page "https://github.com/AdCombo/flask-combo-jsonapi")
    (synopsis "Flask extension to quickly create JSON:API 1.0 REST Web APIs")
    (description
     "Flask-COMBO-JSONAPI is a Python Flask extension for building REST Web APIs
compliant with the @uref{https://jsonapi.org, JSON:API 1.0} specification.

It tries to combine the power of Flask-Restless with the flexibility of
Flask-RESTful to quickly build APIs that fit the complexity of existing
real-life projects with legacy data and diverse storage providers.")
    (license license:expat)))

(define-public python-mwparserfromhell
  (package
    (name "python-mwparserfromhell")
    (version "0.6.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "mwparserfromhell" version))
       (sha256
        (base32 "0zh9zaqbac18s7mivqk8am9xw97lfkgcj9hhxj0d4208pkqpkmqs"))))
    (build-system python-build-system)
    (native-inputs
     (list python-pytest python-pytest-runner))
    (home-page "https://github.com/earwig/mwparserfromhell")
    (synopsis "Python parser for MediaWiki wikicode")
    (description
     "The MediaWiki Parser From Hell is a python library package that provides
a parser for MediaWiki.

It exposes parses as normal string objects with additional methods giving
access to the special Wikicode features it contains (hyperlinks, tags,
templates…).  The parser doesn't interpolate strings at all, it remains at a
purely formal level.

Full documentation may be found at
@uref{https://mwparserfromhell.readthedocs.io, ReadTheDocs}")
    (license license:expat)))

(define-public python-tweepy
  (package
    (name "python-tweepy")
    (version "4.4.0")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/tweepy/tweepy")
         (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0jl3j20iqvzqqw5q5ldval5wrc2pdx94zff3b6b87j51yjx3qjhr"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f         ;XXX: need /etc/ssl/certs/ca-certificates.crt
       #:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "python" "-m" "unittest" "discover" "tests")))))))
    (propagated-inputs
     (list python-aiohttp python-requests python-requests-oauthlib))
    (native-inputs
     (list python-vcrpy))
    (home-page "https://www.tweepy.org/")
    (synopsis "Twitter library for Python")
    (description "This package provides @code{Tweepy}, an easy-to-use Python
library for accessing the Twitter API.")
    (license license:expat)))

(define-public python-quart
  (package
    (name "python-quart")
    (version "0.19.9")
    (source
     (origin
       (method git-fetch)               ; no tests in PyPI release
       (uri (git-reference
             (url "https://github.com/pallets/quart")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1h5ifv8g9dc9m07vj4v7dfalam83v38545d845vvgys1gan1pscd"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'remove-coverage-pytest-options
            (lambda _
              (substitute* "pyproject.toml"
                (("--no-cov-on-fail") "")))))))
    (native-inputs
     (list python-poetry-core
           python-pytest
           python-pytest-asyncio))
    (propagated-inputs
     (list hypercorn
           python-aiofiles
           python-blinker
           python-click
           python-dotenv
           python-flask
           python-itsdangerous
           python-jinja2
           python-markupsafe
           python-toml
           python-werkzeug))
    (home-page "https://gitlab.com/pgjones/quart/")
    (synopsis "Python ASGI web microframework with the same API as Flask")
    (description
     "This package provides a Python ASGI web microframework with the same API
as Flask.")
    (license license:expat)))

(define-public python-quart-trio
  (package
    (name "python-quart-trio")
    (version "0.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "quart_trio" version))
       (sha256
        (base32 "130r8hdk7xxjp380z1r3m28lw86q2a6g9rf0mgp9gs6hk04q9v02"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'remove-coverage-pytest-options
            (lambda _
              (substitute* "pyproject.toml"
                (("--no-cov-on-fail") "")))))))
    (native-inputs
     (list python-pdm-backend
           python-pytest
           python-pytest-sugar
           python-pytest-trio))
    (propagated-inputs
     (list python-exceptiongroup
           python-hypercorn
           python-quart
           python-trio))
    (home-page "https://github.com/pgjones/quart-trio")
    (synopsis "Extension for Quart to support the Trio event loop")
    (description
     "This package provides a Quart extension to provide Trio support.  This
is an alternative to using the asyncio event loop present in the Python
standard library and supported by default in Quart.")
    (license license:expat)))

(define-public python-ajsonrpc
  (package
    (name "python-ajsonrpc")
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "ajsonrpc" version))
       (sha256
        (base32 "17x1a4r4l428mhwn53abki9gzdzq3halyr4lj48fw3dzy0caq6vr"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-quart
           python-sanic
           python-tornado))
    (home-page "https://github.com/pavlov99/ajsonrpc")
    (synopsis "Async JSON-RPC 2.0 protocol and server")
    (description
     "This package provides a Python JSON-RPC 2.0 protocol and server powered
by asyncio.")
    (license license:expat)))

(define-public python-protego
  (package
    (name "python-protego")
    (version "0.2.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "Protego" version))
        (sha256
          (base32 "1wigcjyhz8zbk562zhgfbkm733dcn65j1swzvki79dys0i1nsrnz"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
        (modify-phases %standard-phases
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (invoke "pytest")))))))
    (propagated-inputs (list python-six))
    (native-inputs (list python-pytest))
    (home-page "https://github.com/scrapy/protego")
    (synopsis
      "Pure-Python robots.txt parser with support for modern conventions")
    (description
      "Pure-Python robots.txt parser with support for modern conventions.")
    (license license:bsd-3)))

(define-public python-parsel
  (package
    (name "python-parsel")
    (version "1.8.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "parsel" version))
        (sha256
          (base32 "0f8yh30y3961a7kqwcnp4j3s7044ilakykiavc0skwdkr5l8xwmg"))))
    (build-system pyproject-build-system)
    (propagated-inputs
      (list python-cssselect
            python-jmespath
            python-lxml
            python-typing-extensions
            python-w3lib))
    (native-inputs
      (list python-psutil python-pytest python-setuptools python-wheel))
    (home-page "https://github.com/scrapy/parsel")
    (synopsis "Extract data from HTML and XML using XPath and CSS selectors")
    (description "Parsel is a library to extract and remove data from
HTML and XML using XPath and CSS selectors, optionally combined with
regular expressions.")
    (license license:bsd-3)))

(define-public python-scrapy
  (package
    (name "python-scrapy")
    (version "2.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "scrapy" version))
       (sha256
        (base32 "13vqykvjv9d0hj02l0s025r107dncfj7as0r0iv484lv01v6wvfn"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:test-flags
           ;; Tests requiring a display.
           #~(list "-k" (string-append
                         "not " (string-join
                                 (list "test_pformat"
                                       "test_pformat_old_windows"
                                       "test_pformat_windows"
                                       ;; Connection refused.
                                       "test_persist")
                                 " and not "))
                   ;; Connection refused to some local FTP server.
                   "--ignore=tests/test_feedexport.py")
           #:phases
           #~(modify-phases %standard-phases
               (add-before 'check 'prepare-test-environment
                 (lambda _
                   (setenv "HOME" "/tmp"))))))
    (propagated-inputs
     (list python-cryptography
           python-cssselect
           python-defusedxml
           python-itemadapter
           python-itemloaders
           python-lxml
           python-packaging
           python-parsel
           python-protego
           python-pydispatcher
           python-pyopenssl
           python-queuelib
           python-service-identity
           python-tldextract
           python-typing-extensions
           python-twisted
           python-w3lib
           python-zope-interface))
    (native-inputs
     (list nss-certs-for-test
           python-mypy
           python-pexpect
           python-pytest
           python-pytest-xdist
           python-setuptools
           python-sybil
           python-testfixtures
           python-wheel))
    (home-page "https://scrapy.org")
    (synopsis "High-level Web crawling and Web scraping framework")
    (description "Scrapy is a fast high-level web crawling and web
scraping framework, used to crawl websites and extract structured data
from their pages.  It can be used for a wide range of purposes, from data
mining to monitoring and automated testing.")
    (license license:bsd-3)))

(define-public python-jstyleson
  (package
    (name "python-jstyleson")
    (version "0.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "jstyleson" version))
       (sha256
        (base32 "13ihw6jqwkg3ai4xb83kw39pvh73b2wg6ld3wvj5jaasn7rh6038"))))
    (build-system python-build-system)
    (arguments (list #:tests? #f))      ;no tests in pypi release
    (home-page "https://github.com/linjackson78/jstyleson")
    (synopsis "JSON parser supporting js-style comments")
    (description "@code{jstyleson} is a Python library to parse JSON.
Contrary to the standard Python @code{json} library, it understands js-style
comments.  Trailing comma is also supported.")
    (license license:expat)))

(define-public python-html-text
  (package
    (name "python-html-text")
    (version "0.5.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "html_text" version))
       (sha256
        (base32 "1v9x171l3bmyayc1144nrkn9410lp4lhlrrjii54j7b5f2xipmmg"))))
    (build-system python-build-system)
    (native-inputs (list python-pytest))
    (propagated-inputs (list python-lxml))
    (home-page "https://github.com/TeamHG-Memex/html-text")
    (synopsis "Extract text from HTML")
    (description "HTML to Text is a Python library for extract text from HTML.
Contrary to other solution such as LXML or Beautiful Soup, the text extracted
with @code{html_text} does not contain elements such as JavaScript or inline
styles not normally visible to users.  It also normalizes white space
characters in a smarter, more visually pleasing style.")
    (license license:expat)))

(define-public python-mf2py
  (package
    (name "python-mf2py")
    (version "1.1.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/microformats/mf2py")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "00pzfc5sl6ywlcr6f2k37n3f2bb7w488p2k95ixzjwx6w3yh747n"))))
    (build-system python-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (invoke "pytest" "-vv" "test")))))))
    (native-inputs (list python-mock python-pytest))
    (propagated-inputs
     (list python-beautifulsoup4 python-html5lib python-requests))
    (home-page "https://github.com/microformats/mf2py")
    (synopsis "Python Microformats2 parser")
    (description "This Python library provides a Microformats2 parser
implementing the full Microformats2 (mf2) specification, including backward
compatibility with Microformats1 (mf1).")
    (license license:expat)))

(define-public python-extruct
  (package
    (name "python-extruct")
    (version "0.16.0")
    (source (origin
              (method git-fetch)        ;for tests
              (uri (git-reference
                    (url "https://github.com/scrapinghub/extruct")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1s05sz6nghrap1gjkg3vsqz6djld6lczd6w3r1542ir8n7binl7a"))))
    (build-system python-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (invoke "pytest" "-vv" "tests")))))))
    (native-inputs (list python-pytest))
    (propagated-inputs
     (list python-html-text
           python-jstyleson
           python-lxml
           python-mf2py
           python-pyrdfa3
           python-rdflib
           python-w3lib))
    (home-page "https://github.com/scrapinghub/extruct")
    (synopsis "Extract embedded metadata from HTML markup")
    (description "@code{extruct} is a Python library for extracting embedded
metadata from HTML markup.  Currently, extruct supports:
@itemize
@item W3C's HTML Microdata
@item embedded JSON-LD
@item Microformat via mf2py
@item Facebook's Open Graph
@item (experimental) RDFa via rdflib
@item Dublin Core Metadata (DC-HTML-2003)
@end itemize")
    (license license:bsd-3)))

(define-public python-wadllib
  (package
    (name "python-wadllib")
    (version "1.3.6")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "wadllib" version))
       (sha256
        (base32 "1z65crvdsjxh9nahz1g6q021ijmv85ixmq88l96d61qh5imavndc"))))
    (build-system python-build-system)
    (propagated-inputs (list python-lazr-uri))
    (home-page "https://launchpad.net/wadllib")
    (synopsis "Web Application Description Language (WADL) navigation library")
    (description "The @code{wadllib} Python library allows navigating HTTP
resources using Web Application Description Language (WADL) files as guides.")
    (license license:lgpl3)))

(define-public python-zeep
  (package
    (name "python-zeep")
    (version "4.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "zeep" version))
       (sha256
        (base32 "0vm0asfak0pxfa9i753imcy72hk8gkxbj6pmw185b7dhw7lqalzl"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      '(modify-phases %standard-phases
         (add-after 'unpack 'compatibility
           (lambda _
             ;; httpx removed the "proxies" keyword.  It's now either "mounts"
             ;; or "proxy".  See https://github.com/encode/httpx/pull/2879.
             (substitute* "src/zeep/transports.py"
               (("proxies=") "proxy="))))
         ;; One test requires write access
         (add-before 'check 'set-HOME
           (lambda _ (setenv "HOME" (getcwd)))))))
    (propagated-inputs
     (list python-attrs
           python-isodate
           python-lxml
           python-platformdirs
           python-pytz
           python-requests
           python-requests-file
           python-requests-toolbelt))
    (native-inputs
     (list nss-certs-for-test
           python-coverage
           python-flake8
           python-flake8-blind-except
           python-flake8-debugger
           python-freezegun
           python-isort
           python-pretend
           python-pytest
           python-pytest-asyncio
           python-pytest-cov
           python-pytest-httpx
           python-requests-mock
           python-setuptools
           python-wheel))
    (home-page "https://docs.python-zeep.org/en/stable/")
    (synopsis "Python SOAP client based on lxml / requests")
    (description "Zeep is a Python SOAP client.  Highlights:

@itemize
@item Build on top of @code{lxml} and @code{requests}.
@item Support for Soap 1.1, Soap 1.2 and HTTP bindings.
@item Support for WS-Addressing headers.
@item Support for WSSE (UserNameToken / x.509 signing).
@item Support for @code{asyncio} via @code{httpx}.
@item Experimental support for XOP messages.
@end itemize")
    (license license:expat)))

(define-public python-pysimplesoap
  (package
    (name "python-pysimplesoap")
    (version "1.16.2")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "PySimpleSOAP" version))
              (sha256
               (base32
                "1qb7dn8m1cjwzql7vqj9i1hsscb7nyhimmlp45jrpzxds38g9fxi"))))
    (build-system python-build-system)
    (arguments
     (list #:tests? #f))               ;tests fail due to attempted web access
    (home-page "https://github.com/pysimplesoap/pysimplesoap")
    (synopsis "Simple and lightweight SOAP library for Python")
    (description
     "This package provides a simple and lightweight Python SOAP library for
client and server webservices interfaces, aimed to be as small and easy as
possible, supporting most common functionality.")
    (license license:lgpl3+)))

(define-public python-http-client
  (package
    (name "python-http-client")
    (version "3.3.7")
    (home-page "https://github.com/sendgrid/python-http-client")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0z0ziw3f4zw5fj4spiwhhs2x8qs3i5999ry2p6a5sc8b1lkkj2zi"))
              (snippet #~(begin
                           (use-modules (guix build utils))
                           (delete-file "tests/profile.py")))))
    (build-system python-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'fix-tests
                 (lambda _
                   ;; The test expects the copyright to be updated each year.
                   (substitute* "tests/test_daterange.py"
                     (("time\\.strftime\\(\"%Y\"\\)") "2022")))))))
    (synopsis "HTTP REST client for Python")
    (description
     "This package provides access to any RESTful or RESTful-like API.")
    (license license:expat)))

(define-public python-sendgrid
  (package
    (name "python-sendgrid")
    (version "6.9.7")
    (home-page "https://github.com/sendgrid/sendgrid-python/")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0kvp4gm3bpcsj2mkv05pgvlcv1jlsfhcljcv61wz5kq9d273h7rg"))))
    (build-system python-build-system)
    (arguments
     (list #:tests? #f))       ;241/340 tests fail due to attempted web access
    (propagated-inputs (list python-http-client python-starkbank-ecdsa))
    (synopsis "SendGrid API library for Python")
    (description
     "The @code{sendgrid} Python library allows access to the
SendGrid Web API v3.  Version 3+ of the library provides full support for all
SendGrid Web API v3 endpoints, including the new v3 /mail/send.")
    (license license:expat)))

(define-public python-starlette
  (package
    (name "python-starlette")
    (version "0.42.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/encode/starlette")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0r46v6b687pqr4sjcv0rwd1ln248fnx0xkawbn945r04pg7c999q"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-anyio
                             python-typing-extensions
                             ;; [all] extra dependencies:
                             python-itsdangerous
                             python-jinja2
                             python-multipart
                             python-pyyaml
                             python-requests))
    (native-inputs (list python-hatchling
                         python-httpx
                         python-pytest
                         python-typing-extensions))
    (home-page "https://github.com/encode/starlette")
    (synopsis "Little ASGI library")
    (description
     "Starlette is a lightweight ASGI (Asynchronous Server Gateway
Interface) framework/toolkit for building async web services in Python.")
    (license license:bsd-3)))

(define-public python-starsessions
  (package
    (name "python-starsessions")
    (version "1.3.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/alex-oleshkevich/starsessions")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "00dkdhp22vfmcn0w4y7f2ii8m1xj5i7409x58g3l8lvk6v5ng2nf"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      ;; These tests require a running redis server
      '(list "--ignore=tests/backends/test_redis.py"
             ;; XXX: this failure indicates a real compatibility problem, but
             ;; it seems restricted to the tests only.
             ;; AttributeError: 'Cookies' object has no attribute
             ;; 'clear_session_cookies'
             "-k"
             (string-append "not test_session_clears_on_tab_close"
                            ;; This fails because an error is not raised.
                            " and not test_session_wants_secret_key"))
      #:phases
      '(modify-phases %standard-phases
         (add-after 'unpack 'compatibilitiy
           (lambda _
             ;; aioredis has been renamed
             (substitute* "starsessions/backends/redis.py"
               (("aioredis") "redis")))))))
    (propagated-inputs
     (list python-redis
           python-itsdangerous
           python-starlette))
    (native-inputs
     (list python-black
           python-flake8
           python-httpx
           python-mypy
           python-poetry-core
           python-pytest
           python-pytest-asyncio
           python-requests))
    (home-page "https://github.com/alex-oleshkevich/starsessions")
    (synopsis "Pluggable session support for Starlette and FastAPI")
    (description
     "This package adds pluggable session backends and ships default
@code{InMemoryBackend} and @code{CookieBackend} implementations for Starlette
and FastAPI.")
    (license license:expat)))

(define-public python-suds
  (package
    (name "python-suds")
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "suds" version))
       (sha256
        (base32 "1byyg7b2ixpr8hc849a6dd0qn5daxqawz6lb3php8lrmsb1n5cc3"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-pytest
           python-setuptools
           python-wheel))
    (home-page "https://github.com/suds-community/suds")
    (synopsis "Lightweight SOAP python client for consuming Web Services")
    (description
     "Suds is a lightweight SOAP-based web service client for Python.")
    (license license:lgpl3+)))

(define-public python-fastapi
  (package
    (name "python-fastapi")
    (version "0.115.6")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "fastapi" version))
              (sha256
               (base32
                "0m36nyldk0640mbsysm446bz6rfynpjsm5lajmra8kn1vmx6zi4y"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      ;; Argument() missing 1 required positional argument: 'default'
      '(list "--ignore=tests/test_fastapi_cli.py"
             ;; cannot import name 'StaticPool' from 'sqlalchemy'
             "--ignore-glob=tests/test_tutorial/*"
             ;; FIXME: Unclear why these 8 tests fail.
             "--ignore=tests/test_dependency_contextmanager.py")
      #:phases
      '(modify-phases %standard-phases
         (add-after 'unpack 'relax-requirements
           (lambda _
             (substitute* "pyproject.toml"
               (("<0.42.0") "<=0.42.0")))))))
    (propagated-inputs (list python-email-validator
                             python-fastapi-cli
                             python-httpx
                             python-itsdangerous
                             python-jinja2
                             python-multipart
                             python-orjson
                             python-pydantic-2
                             python-pydantic-settings
                             python-pyyaml
                             python-starlette
                             python-typing-extensions
                             python-ujson
                             python-uvicorn))
    (native-inputs
     (list python-dirty-equals
           python-flask
           python-inline-snapshot
           python-pdm-backend
           python-pyjwt
           python-pytest
           python-sqlalchemy))
    (home-page "https://github.com/tiangolo/fastapi")
    (synopsis "Web framework based on type hints")
    (description "FastAPI provides a web API framework based on pydantic and
starlette.")
    (license license:expat)))

(define-public python-fastapi-csrf-protect
  (package
    (name "python-fastapi-csrf-protect")
    (version "0.3.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/aekasitt/fastapi-csrf-protect")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "181yawlbpd4709mamsradr9zj0jrbp4qvwfvmggs80pd36iiwyy8"))))
    (build-system pyproject-build-system)
    (propagated-inputs
     (list python-fastapi python-itsdangerous
           python-pydantic-2))
    (native-inputs
     (list python-poetry-core
           python-pytest))
    (home-page "https://github.com/aekasitt/fastapi-csrf-protect")
    (synopsis "Cross-Site Request Forgery (XSRF) protection")
    (description
     "This package provides a stateless implementation of @dfn{Cross-Site
Request Forgery} (XSRF) Protection by using the Double Submit Cookie mitigation
pattern.")
    (license license:expat)))

(define-public python-fastapi-pagination-minimal
  (package
    (name "python-fastapi-pagination-minimal")
    (version "0.12.34")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/uriyyo/fastapi-pagination")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "004w9h4ana0p766n7f0z0n06v1472yjci1hvbys9gncpclcc49gj"))))
    (build-system pyproject-build-system)
    ;; We run the tests in python-fastapi-pagination only.
    (arguments (list #:tests? #false))
    ;; These are the only required dependencies,
    ;; please add all optional dependencies to the
    ;; full python-fastapi-pagination below.
    (propagated-inputs (list python-fastapi python-pydantic-2))
    (native-inputs (list python-poetry-core))
    (home-page "https://github.com/uriyyo/fastapi-pagination")
    (synopsis "FastAPI pagination library")
    (description "@code{fastapi-pagination} is a Python library designed to
simplify pagination in FastAPI applications.  It provides a set of utility
functions and data models to help you paginate your database queries and
return paginated responses to your clients.")
    (license license:expat)))

(define-public python-fastapi-pagination
  (package
    (inherit python-fastapi-pagination-minimal)
    (name "python-fastapi-pagination")
    (arguments
     (list
      #:test-flags
      '(list "--unit-tests" "--ignore=tests/ext")
      #:phases
      '(modify-phases %standard-phases
         (add-after 'unpack 'patch-tests
           (lambda _
             ;; We don't have a package for python-cassandra or motor, but we
             ;; also don't need them as we only run the unit tests anyway.
             (substitute* "tests/conftest.py"
               (("from cassandra.cluster.*") "")
               (("from motor.*") "")))))))
    (propagated-inputs
     (modify-inputs (package-propagated-inputs
                     python-fastapi-pagination-minimal)
       (prepend python-asyncpg
                python-databases
                python-django
                python-fastapi
                python-pydantic-2
                python-sqlalchemy
                (package
                  (inherit python-tortoise-orm)
                  (arguments
                   (substitute-keyword-arguments (package-arguments
                                                  python-tortoise-orm)
                     ((#:phases phases '%standard-phases)
                      `(modify-phases ,phases
                         (delete 'sanity-check)))))
                  (propagated-inputs
                   (modify-inputs (package-propagated-inputs python-tortoise-orm)
                     (replace "python-aiosqlite" python-aiosqlite)))))))
    (native-inputs
     (list python-aiosqlite
           python-asgi-lifespan
           python-asyncpg
           python-faker
           python-httpx
           python-poetry-core
           python-pytest
           python-pytest-asyncio
           python-sqlalchemy
           python-typing-extensions))
    (description
     (string-append (package-description python-fastapi-pagination-minimal)
                    "

This package, as opposed to @code{python-fastapi-pagination-minimal}, depends on
all available optional dependencies supported by mainline
@code{fastapi-pagination}."))))

(define-public python-fastapi-cli
  (package
    (name "python-fastapi-cli")
    (version "0.0.7")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "fastapi_cli" version))
       (sha256
        (base32 "0pk0ldmymc25ys4v6mslzd5vsjh9r69hfylh2ljl29pmarcvdcq2"))))
    (build-system pyproject-build-system)
    ;; Tests require fastapi, which depends on this package.
    (arguments (list #:tests? #false))
    (propagated-inputs (list python-rich-toolkit python-typer python-uvicorn))
    (native-inputs
     (list python-inline-snapshot python-pdm-backend python-pytest))
    (home-page "https://pypi.org/project/fastapi-cli/")
    (synopsis
     "Run and manage FastAPI apps from the command line with FastAPI CLI")
    (description
     "FastAPI CLI is a command line program fastapi that you can use to serve
your FastAPI app, manage your FastAPI project, and more.")
    (license license:expat)))

(define-public python-pyactiveresource
  (package
    (name "python-pyactiveresource")
    (version "2.2.2")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "pyactiveresource" version))
              (sha256
               (base32
                "1n7xvzh1j8fxal0gx9bgg1ayrp3q9mb1a2vb12d6s86wa93880rg"))))
    (build-system python-build-system)
    (native-inputs (list python-dateutil python-pyyaml))
    (propagated-inputs (list python-six))
    (home-page "https://github.com/Shopify/pyactiveresource/")
    (synopsis "ActiveResource for Python")
    (description "This package provides a Python port of the ActiveResource
project.

Active Resource attempts to provide a coherent wrapper object-relational
mapping for REST web services.  It follows the same philosophy as Active
Record, in that one of its prime aims is to reduce the amount of code needed
to map to these resources.  This is made possible by relying on a number of
code- and protocol-based conventions that make it easy for Active Resource to
infer complex relations and structures.")
    (license license:expat)))

(define-public python-shopifyapi
  (package
    (name "python-shopifyapi")
    (version "12.0.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "ShopifyAPI" version))
              (sha256
               (base32
                "03np9mkycqbw5l5vk0srmq353q3vmbycbbarzv0cg3z703p81gnb"))))
    (build-system python-build-system)
    (native-inputs (list python-mock))
    (propagated-inputs (list python-pyactiveresource python-pyjwt
                             python-pyyaml python-six))
    (home-page "https://github.com/Shopify/shopify_python_api")
    (synopsis "Shopify API for Python")
    (description "This package provides the Shopify API for Python.

The ShopifyAPI library allows Python developers to programmatically access the
admin section of stores using an ActiveResource-like interface similar the
ruby Shopify API gem.  The library makes HTTP requests to Shopify in order to
list, create, update, or delete resources (e.g. Order, Product, Collection).")
    (license license:expat)))

(define-public python-whatthepatch
  (package
    (name "python-whatthepatch")
    (version "1.0.2")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "whatthepatch" version))
              (sha256
               (base32
                "17zspm5sxhvggkdhwq4s0fzf4mkc825xshn734g2j2iy2xcylh65"))))
    (build-system python-build-system)
    (home-page "https://github.com/cscorley/whatthepatch")
    (synopsis "Patch parsing and application")
    (description
     "This package provides a library to parse and apply patches.")
    (license license:expat)))

(define-public python-pybadges
  (package
    (name "python-pybadges")
    (version "3.0.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/google/pybadges")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               "1zgb9idz7m3mzf8wvik0gwmyrxp753axqjv2pab326cr5myj1s4b")))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~'("-k"
          ;; Disable network dependent tests.
          "not test_changes and not test_not_image_url and not test_http_url")))
    (propagated-inputs (list python-jinja2 python-requests))
    (native-inputs (list python-flask
                         python-fonttools
                         python-nox
                         python-pillow
                         python-pytest
                         python-xmldiff))
    (home-page "https://github.com/google/pybadges")
    (synopsis "Generate Github-style badges on the command-line")
    (description
     "This package provides @code{python-pybadges}: a library and command-line
tool for generating Github-style badges as SVG images.")
    (license license:asl2.0)))

(define-public python-grid5000
  (package
    (name "python-grid5000")
    (version "1.2.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.inria.fr/msimonin/python-grid5000")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               "097pm8b68ihk29xz9zv29b1x0bhgjb4lfj8zxk2grbsh7wr9dipg")))
    (build-system python-build-system)
    (native-inputs (list python-wheel))
    (propagated-inputs (list python-requests python-ipython python-pyyaml))
    (arguments
     (list #:tests? #f)) ; No tests.
    (home-page "https://pypi.org/project/python-grid5000/")
    (synopsis "Grid5000 python client")
    (description
     "python-grid5000 is a python package wrapping the Grid5000 REST API.
You can use it as a library in your python project or you can explore the
Grid5000 resources interactively using the embedded shell.")
    (license license:gpl3+)))

(define-public python-enoslib
  (package
    (name "python-enoslib")
    (version "8.0.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.inria.fr/discovery/enoslib")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               "0vs6b0bnlv95mzv0rjbxqwrhzkgjkn91gqipgwdf7y4ffpz8nybg")))
    (build-system python-build-system)
    (native-inputs (list python-wheel python-pytest python-ddt
                         python-freezegun))
    (propagated-inputs (list ansible
                             python-cryptography
                             python-grid5000
                             python-jsonschema
                             python-netaddr
                             python-packaging
                             python-requests
                             python-rich
                             python-sshtunnel
                             python-pytz))
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (replace 'check
                    (lambda* (#:key tests? #:allow-other-keys)
                      (when tests?
                        ;; Otherwise Ansible fails to create its config directory.
                        (setenv "HOME" "/tmp")
                        ;; Ignoring the tests requiring an extra dependency (iotlabcli)
                        (invoke "pytest" "enoslib/tests/unit"
                                "--ignore"
                                "enoslib/tests/unit/infra/test_utils.py"
                                "--ignore-glob"
                                "enoslib/tests/unit/infra/enos_iotlab/*"))))
                  ;; Disable the sanity check, which fails with the following error:
                  ;;
                  ;; ContextualVersionConflict(rich 12.4.1
                  ;; (/gnu/store/...-python-rich-12.4.1/lib/python3.9/site-packages),
                  ;; Requirement.parse('rich[jupyter]~=12.0.0'), {'enoslib'})
                  ;;
                  ;; The optional jupyter dependency of rich isn't critical for
                  ;; EnOSlib to work
                  (delete 'sanity-check))))

    (home-page "https://discovery.gitlabpages.inria.fr/enoslib/index.html")
    (synopsis "Deploy distributed testbeds on a variety of platforms")
    (description
     "EnOSlib is a library to build experimental frameworks on various
scientific testbeds.  It lets you deploy networks of machines on actual
hardware on Grid'5000 or via OpenStack, to Vagrant, Chameleon, and more.")
    (license license:gpl3+)))

(define-public python-pynetbox
  (package
    (name "python-pynetbox")
    (version "7.4.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "pynetbox" version))
              (sha256
               (base32
                "1hq0mqykwdsmcrj61fxxzdkpc7wnld4grhkcxy560ym79jbbb0iz"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      ;; Integration tests depend on docker.
      '(list "--ignore=tests/integration")))
    (propagated-inputs (list python-packaging python-requests))
    (native-inputs
     (list python-pytest
           python-setuptools
           python-setuptools-scm
           python-wheel))
    (home-page "https://github.com/netbox-community/pynetbox")
    (synopsis "NetBox API client library")
    (description "Python module to query and edit data stored in a
@url{https://netbox.dev,NetBox} instance.")
    (license license:asl2.0)))

(define-public python-waybackpack
  (package
    (name "python-waybackpack")
    (version "0.6.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "waybackpack" version))
       (sha256
        (base32 "16lcr2hv7gmp199fh3psrnv68j20pfxria8v6gchrpl1jqx9f923"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:tests? #f)) ; would require network (and SSL certificates)
    (propagated-inputs (list python-requests))
    (native-inputs (list python-pytest python-setuptools python-wheel))
    (home-page "https://github.com/jsvine/waybackpack")
    (synopsis
     "Download the entire Wayback Machine archive for a given URL")
    (description
     "This package provides a library and a command-line tool that lets
you download the entire Wayback Machine archive for a given URL.")
    (license license:expat)))
