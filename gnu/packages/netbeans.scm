;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2026 Danny Milosavljevic <dannym@friendly-machines.com>
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

(define-module (gnu packages netbeans)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix build-system ant)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages java)
  #:use-module (gnu packages java-graphics)
  #:use-module (gnu packages java-xml)
  #:use-module (gnu packages web))

(define-public java-jemmy-2.3.1.1
  (package
    (name "java-jemmy")
    (version "2.3.1.1")
    (source
     (origin
       (method url-fetch)
       ;; This is the exact source payload referenced by NetBeans' own
       ;; harness/jemmy/external/binaries-list.
       (uri (string-append
             "https://netbeans.osuosl.org/binaries/"
             "49197106637CCA8C337AF16CC01BB5D9DEC7E179-jemmy-"
             version "-src.zip"))
       (sha256
        (base32 "183qppmxlnzdniw2qg92ng9g42hyn2823gbnkjxrps4kdy9blwz1"))))
    (build-system ant-build-system)
    (arguments
     (list
      #:jdk openjdk17
      #:tests? #f
      #:modules
      '((guix build utils)
        (guix build ant-build-system)
        (guix build java-utils)
        (srfi srfi-13))
      #:phases
      #~(modify-phases %standard-phases
          (replace 'build
            (lambda _
              (let ((source-files (find-files "." "\\.java$")))
                (mkdir-p "build/classes")
                (for-each
                 (lambda (file)
                   (unless (string-suffix? ".java" file)
                     (let ((destination
                            (string-append "build/classes/" file)))
                       (mkdir-p (dirname destination))
                       (copy-file file destination))))
                 (find-files "." ".*"))
                (apply invoke "javac" "-d" "build/classes" source-files)
                (invoke "jar" "-cf" "build/jemmy-2.3.1.1.jar"
                        "-C" "build/classes" ".")
                (apply invoke "javadoc"
                       "-quiet"
                       "-Xdoclint:none"
                       "-d" "build/doc"
                       source-files)
                (with-directory-excursion "build/doc"
                  (invoke "zip" "-qrX" "../jemmy-2.3.1.1-doc.zip" "."))
                (invoke "zip" "-qrX" "build/jemmy-2.3.1.1-src.zip" ".")
                #t)))
          (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((share-java
                     (string-append (assoc-ref outputs "out")
                                    "/share/java")))
                (mkdir-p share-java)
                (for-each
                 (lambda (file)
                   (install-file file share-java))
                 '("build/jemmy-2.3.1.1.jar"
                   "build/jemmy-2.3.1.1-doc.zip"
                   "build/jemmy-2.3.1.1-src.zip"))
                #t))))))
    (home-page "https://web.archive.org/web/20120822233211/http://java.net/projects/jemmy")
    (synopsis "Jemmy Swing UI testing library")
    (description
     "Jemmy is a Java library for automating and testing Swing and AWT user
interfaces.")
    (native-inputs
     (list unzip))
    (license license:cddl1.0)))
