;;; GNU Guix --- Tests for the node-build-system procedures.
;;; Copyright © 2026 Maxim Cournoyer <maxim@guixotic.coop>
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

(use-modules (json)
             (guix build node-build-system)
             (guix tests)
             (srfi srfi-64))

(define (package.json)
  "Sample package.json file."
  (call-with-output-string
    (lambda (out)
      (scm->json
       '(("devDependencies"
          ("typescript-eslint" . "^8.61.0")
          ("typescript" . "^5.9.3")
          ("typedoc" . "^0.28.19")
          ("htmlparser2" . "^10.1.0")
          ("eslint" . "^10.4.1")
          ("@types/node" . "^25.9.2"))
         ("dependencies"
          ("domelementtype" . "^3.0.0")
          ("boolbase" . ">=2.0.0"))
         ("peerDependencies"
          ("mkdirp" . ">=1.0.0")
          ("react" . "^16.8.0"))
         ("engines" ("node" . ">=20.19.0"))
         ("repository"
          ("url" . "https://example.com/dummy/dummy.git")
          ("type" . "git"))
         ("scripts"
          ("test" . "npm run test")
          ("lint" . "eslint .")
          ("build" . "tsc"))
         ("files" . #("dist" "src" "!**/*.spec.ts"))
         ("sideEffects" . #f)
         ("exports"
          ("." ("default" . "./dist/index.js") ("types" . "./dist/index.d.ts")))
         ("types" . "dist/index.d.ts")
         ("main" . "dist/index.js")
         ("license" . "GPL-3.0")
         ("description" . "Dummy package.json")
         ("version" . "6.0.1")
         ("name" . "dummy")
         ("type" . "module"))
       out #:pretty #t))))

(define* (modify-json* #:rest all-arguments)
  "Wrap modify-json to use our package.json test sample and avoid file I/O.
It also returns the data as an alist directly."
  (mock ((guix build utils) with-atomic-file-replacement
         (lambda (_ proc)
           (call-with-input-string (package.json)
             (lambda (in)
               (call-with-output-string
                 (lambda (out)
                   (proc in out)))))))
        (json-string->scm (apply modify-json all-arguments)
                          #:ordered #t)))


(test-begin "node related tests")

(test-equal "delete-dependencies"
  '(("domelementtype" . "^3.0.0"))
  (assoc-ref (modify-json*
              (delete-dependencies '("boolbase")))
             "dependencies"))

(test-equal "delete-dependencies/except"
  '(("boolbase" . ">=2.0.0"))
  (assoc-ref (modify-json*
              (delete-dependencies/except '("boolbase")))
             "dependencies"))

(test-equal "delete-dev-dependencies/except"
  '(("typescript" . "^5.9.3")
    ("@types/node" . "^25.9.2"))
  (assoc-ref (modify-json*
              (delete-dev-dependencies/except
               '("typescript" "@types/node")))
             "devDependencies"))

(test-end)
