;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Ludovic Courtès <ludo@gnu.org>
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

(define-module (tests-openpgp)
  #:use-module (guix openpgp)
  #:use-module (gcrypt hash)
  #:use-module (gcrypt pk-crypto)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 match)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-64)
  #:use-module (srfi srfi-71))

(define %radix-64-sample
  ;; Example of Radix-64 encoding from Section 6.6 of RFC4880.
  "\
-----BEGIN PGP MESSAGE-----
Version: OpenPrivacy 0.99

yDgBO22WxBHv7O8X7O/jygAEzol56iUKiXmV+XmpCtmpqQUKiQrFqclFqUDBovzS
vBSFjNSiVHsuAA==
=njUN
-----END PGP MESSAGE-----\n")

(define %radix-64-sample/crc-mismatch
  ;; This time with a wrong CRC24 value.
  "\
-----BEGIN PGP MESSAGE-----

yDgBO22WxBHv7O8X7O/jygAEzol56iUKiXmV+XmpCtmpqQUKiQrFqclFqUDBovzS
vBSFjNSiVHsuAA==
=AAAA
-----END PGP MESSAGE-----\n")

(define %civodul-fingerprint
  "3CE4 6455 8A84 FDC6 9DB4  0CFB 090B 1199 3D9A EBB5")

(define %civodul-key-id #x090B11993D9AEBB5)       ;civodul.key

;; Test keys.  They were generated in a container with:
;;    guix environment -C --ad-hoc gnupg pinentry
;; then setting 'pinentry-program' in ~/.gnupg/gpg-agent.conf, then:
;;    gpg --quick-gen-key '<ludo+test-rsa@chbouib.org>' rsa
;; or similar.
(define %rsa-key-id      #xAE25DA2A70DEED59)      ;rsa.key
(define %dsa-key-id      #x587918047BE8BD2C)      ;dsa.key
(define %ed25519-key-id  #xD5AAA83D6DD5DDAF)      ;ed25519.key

(define %hello-signature/rsa
  ;; Signature of the ASCII string "Hello!\n".
  "\
-----BEGIN PGP SIGNATURE-----

iQEzBAABCAAdFiEEOF+Gz8hrZlpcFl5rriXaKnDe7VkFAl4SRF0ACgkQriXaKnDe
7VlIyQf/TU5rGUK42/C1ULoWvvm25Mjwh6xxoPPkuBxvos8bE6yKr/vJZePU3aSE
mjbVFcO7DioxHMqLd49j803bUtdllJVU18ex9MkKbKjapkgEGkJsuTTzqyONprgk
7xtZGBWuxkP1M6hJICJkA3Ys+sTdKalux/pzr5OWAe+gxytTF/vr/EyJzdmBxbJv
/fhd1SeVIXSw4c5gf2Wcvcgfy4N5CiLaUb7j4646KBTvDvmUMcDZ+vmKqC/XdQeQ
PrjArGKt40ErVd98fwvNHZnw7VQMx0A3nL3joL5g7/RckDOUb4mqKoqLsLd0wPHP
y32DiDUY9s3sy5OMzX4Y49em8vxvlg==
=ASEm
-----END PGP SIGNATURE-----")


(define %hello-signature/dsa
  "\
-----BEGIN PGP SIGNATURE-----

iHUEABEIAB0WIQQohKmAQiMwpPM92X9YeRgEe+i9LAUCXhJFpQAKCRBYeRgEe+i9
LDAaAQC0lXPQepvZBANAUtRLMZuOwL9NQPkfhIwUXtLEBBzyFQD/So8DcybXpRBi
JKOiyAQQjMs/GJ6qMEQpRAhyyJRAock=
=iAEc
-----END PGP SIGNATURE-----")


(define %hello-signature/ed25519
  "\
-----BEGIN PGP SIGNATURE-----

iHUEABYIAB0WIQRUSdoRdeRAgGZWA7vVqqg9bdXdrwUCXhJJkQAKCRDVqqg9bdXd
r6APAPwMChp3T5Ve9EJzyU2C5XmZzCeNDLnJ7olroOhdytWoiQD7BQMzI2NRx9g6
Wc6xUJ5UZuLN62A0ahjU+tm0JgdCewY=
=xlKH
-----END PGP SIGNATURE-----")


(test-begin "openpgp")

(test-assert "read-radix-64"
  (bytevector? (call-with-input-string %radix-64-sample read-radix-64)))

(test-assert "read-radix-64, CRC mismatch"
  (not (call-with-input-string %radix-64-sample/crc-mismatch
         read-radix-64)))

(test-assert "get-openpgp-keyring"
  (let* ((key (search-path %load-path "tests/civodul.key"))
         (keyring (get-openpgp-keyring
                   (open-bytevector-input-port
                    (call-with-input-file key read-radix-64)))))
    (match (lookup-key-by-id keyring %civodul-key-id)
      (((? openpgp-public-key? primary) packets ...)
       (and (= (openpgp-public-key-id primary) %civodul-key-id)
            (not (openpgp-public-key-subkey? primary))
            (string=? (openpgp-format-fingerprint
                       (openpgp-public-key-fingerprint primary))
                      %civodul-fingerprint)
            (string=? (openpgp-user-id-value (find openpgp-user-id? packets))
                      "Ludovic Courtès <ludo@gnu.org>"))))))

(test-equal "verify-openpgp-signature, missing key"
  `(missing-key ,%rsa-key-id)
  (let* ((keyring   (get-openpgp-keyring (%make-void-port "r")))
         (signature (get-openpgp-packet
                     (open-bytevector-input-port
                      (call-with-input-string %hello-signature/rsa
                        read-radix-64)))))
    (let-values (((status key)
                  (verify-openpgp-signature signature keyring
                                            (open-input-string "Hello!\n"))))
      (list status key))))

(test-equal "verify-openpgp-signature, good signatures"
  `((good-signature ,%rsa-key-id)
    (good-signature ,%dsa-key-id)
    (good-signature ,%ed25519-key-id))
  (map (lambda (key signature)
         (let* ((key       (search-path %load-path key))
                (keyring   (get-openpgp-keyring
                            (open-bytevector-input-port
                             (call-with-input-file key read-radix-64))))
                (signature (get-openpgp-packet
                            (open-bytevector-input-port
                             (call-with-input-string signature
                               read-radix-64)))))
           (let-values (((status key)
                         (verify-openpgp-signature signature keyring
                                                   (open-input-string "Hello!\n"))))
             (list status (openpgp-public-key-id key)))))
       (list "tests/rsa.key" "tests/dsa.key"
             "tests/ed25519.key")
       (list %hello-signature/rsa %hello-signature/dsa
             %hello-signature/ed25519)))

(test-equal "verify-openpgp-signature, bad signature"
  `((bad-signature ,%rsa-key-id)
    (bad-signature ,%dsa-key-id)
    (bad-signature ,%ed25519-key-id))
  (let ((keyring (fold (lambda (key keyring)
                         (let ((key (search-path %load-path key)))
                           (get-openpgp-keyring
                            (open-bytevector-input-port
                             (call-with-input-file key read-radix-64))
                            keyring)))
                       %empty-keyring
                       '("tests/rsa.key" "tests/dsa.key"
                         "tests/ed25519.key"))))
    (map (lambda (signature)
           (let ((signature (get-openpgp-packet
                             (open-bytevector-input-port
                              (call-with-input-string signature
                                read-radix-64)))))
             (let-values (((status key)
                           (verify-openpgp-signature signature keyring
                                                     (open-input-string "What?!"))))
               (list status (openpgp-public-key-id key)))))
         (list %hello-signature/rsa %hello-signature/dsa
               %hello-signature/ed25519))))

(test-end "openpgp")
