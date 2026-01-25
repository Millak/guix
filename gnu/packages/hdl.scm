;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2026 Cayetano Santos <csantosb@inventati.org>
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

(define-module (gnu packages hdl)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system copy)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (gnu packages electronics)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz))

(define-public ieee-p1076
  (package
    (name "ieee-p1076")
    (version "2019")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://opensource.ieee.org/vasg/Packages/")
              (commit (string-append "1076-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1va626i5ww2ziw3dghw0d2mq7mrj5dwcn0h019h77866yw2pq9xn"))))
    (build-system copy-build-system)
    (native-inputs (list python-minimal-wrapper nvc python-vunit))
    (arguments
     (list
      ;; Not all 2019 features are supported by nvc compiler.
      ;; pass 1055 of 1648
      #:tests? #f
      #:install-plan
      #~'(("ieee" "share/ieee-p1076/ieee" #:include ("vhdl"))
          ("std" "share/ieee-p1076/std" #:include ("vhdl")))))
    (native-search-paths
     (list (search-path-specification
             (variable "FW_IEEE_p1076")
             (separator #f)
             (files (list "share/ieee-p1076")))))
    (home-page "https://IEEE-P1076.gitlab.io")
    (synopsis "VHDL libraries corresponding to the IEEE 1076 standard")
    (description
     "Open source materials intended for reference by the IEEE standard 1076,
as approved and published by the @acronym{VHDL, Very High Speed Hardware
Description Language} Analysis and Standardization Group.")
    (license license:asl2.0)))

(define-public neorv32
  (package
    (name "neorv32")
    (version "1.12.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/stnolting/neorv32")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "030djnf5ww4b2w6lhb9lpprq2iawf8v93rmmgfgnbr56k4blyyk6"))))
    (outputs
     '("out" "neorv32"))
    (build-system copy-build-system)
    (arguments
     (list
      #:tests? #f            ;FIXME: nvc checksum errors inside the build env
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (substitute* '("rtl/file_list_cpu.f" "rtl/file_list_soc.f")
                  (("NEORV32_RTL_PATH_PLACEHOLDER") "rtl"))
                (for-each
                 (lambda (f)
                   (invoke
                    "sh" "-c"
                    (format #f "cat rtl/~a.f | xargs nvc --work=neorv32 -a" f)))
                 '("file_list_soc" "file_list_cpu"))
                (for-each
                 (lambda (f)
                   (invoke "nvc" "--work=neorv32" "-a" f))
                 `("sim/sim_uart_rx.vhd"
                   ,@(find-files "sim" "x.*\\.vhd$")
                   "sim/neorv32_tb.vhd"))
                (invoke "nvc" "--work=neorv32" "-e" "neorv32_tb")
                (invoke "nvc" "--work=neorv32" "-r" "--stop-time=10ms"
                        "neorv32_tb")))))
      #:install-plan
      #~'(("rtl" "share/neorv32/work/rtl"
           #:exclude-regexp (".*\\.f$" ".*\\.sh$"))
          ("rtl" "share/neorv32/neorv32/rtl"
           #:exclude-regexp (".*\\.f$" ".*\\.sh$")
           #:output "neorv32"))))
    (native-inputs (list nvc))
    (native-search-paths
     (list (search-path-specification
             (variable "FW_NEORV32")
             (separator #f)
             (files (list "share/neorv32")))))
    (home-page "https://stnolting.github.io/neorv32/")
    (synopsis "RISC-V soft core CPU in VHDL")
    (description
     "Neorv32 is a small, customizable 32 bits microcontroller-like system on
chip written in platform-independent VHDL.")
    (properties
     `((output-synopsis "out" "Instance this design library as work")
       (output-synopsis "neorv32" "Instance this design library as neorv32")))
    (license license:bsd-3)))

(define-public open-logic
  (package
    (name "open-logic")
    (version "4.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/open-logic/open-logic/")
              (commit version)
              ;; Required by the en_cl_fix submodule.
              (recursive? #t)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1792a6i9jq2yawipmk0nr01z092kx3kkav9v5sjf34khk3biav6q"))))
    (outputs
     '("out" "olo"))
    (properties
     `((output-synopsis "out" "Instance this design library as work")
       (output-synopsis "olo" "Instance this design library as olo")))
    (build-system copy-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'check
            (lambda* (#:key tests? inputs #:allow-other-keys)
              (when tests?
                (with-directory-excursion "3rdParty/en_cl_fix/sim"
                  (invoke "python3" "run.py" "--simulator" "nvc"
                          "--simulator-path"
                          (dirname (search-input-file inputs "bin/nvc"))))
                (with-directory-excursion "sim"
                  (substitute* "run.py"
                    ;; This is required to comply with current VUnit, see:
                    ;; https://github.com/VUnit/vunit/issues/777
                    (("compile_builtins=False, ")
                     ""))
                  (invoke "python3" "run.py" "--nvc" "-v"))))))
      #:install-plan
      #~'(;; Library work.
          ("src" "share/open-logic/work/src"
           #:include ("vhd"))
          ("3rdParty" "share/open-logic/work/3rdParty"
           #:include ("vhd"))
          ;; Library olo.
          ("src" "share/open-logic/olo/src"
           #:include ("vhd") #:output "olo")
          ("3rdParty" "share/open-logic/olo/3rdParty"
           #:include ("vhd") #:output "olo"))))
    (native-inputs
     (list nvc python-matplotlib python-minimal python-vunit))
    (native-search-paths
     (list (search-path-specification
             (variable "FW_OPEN_LOGIC")
             (separator #f)
             (files (list "share/open-logic")))))
    (home-page "https://github.com/open-logic/open-logic/")
    (synopsis "Open library of VHDL standard components")
    (description "Open Logic implements commonly used design units in a
reusable and vendor/tool-independent way.  It is written following the VHDL
2008 standard, but can also be used from System Verilog.")
    (license (list license:lgpl2.1
                   license:expat)))) ;en_cl_fix uses Expat license
