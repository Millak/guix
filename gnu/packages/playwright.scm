;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2025 Danny Milosavljevic <dannym@friendly-machines.com>
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

(define-module (gnu packages playwright)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix build-system node)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages chromium)
  #:use-module (gnu packages node)
  #:use-module (gnu packages node-xyz)
  #:use-module (gnu packages python)
  #:use-module (gnu packages web))

;; Playwright is a browser automation framework.  Upstream provides patched
;; versions of Firefox and WebKit browsers, but those require building
;; custom browser forks.  This package only supports Chromium-based browsers
;; (using the system ungoogled-chromium) since Chromium's DevTools Protocol
;; is sufficient for automation without patches.
;;
;; Firefox support would require Playwright's "Juggler" protocol patches.
;; WebKit support would require Playwright's WebKit automation patches.
;; Both are invasive changes to browser source code.
;;
;; In the future, WebDriver BiDi (a W3C standard) may allow Playwright to
;; work with unpatched browsers.

(define-public node-playwright-core
  (package
    (name "node-playwright-core")
    (version "1.50.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/microsoft/playwright")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0xymivpn2c4srbaqdix3qx2zcr6cx5zgs2a20drvi4dmkspn4jz6"))
       (modules '((guix build utils)))
       (snippet
        '(delete-file-recursively "packages/playwright-core/bundles"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f  ; Tests require browser binaries.
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'break-monorepo-structure
            (lambda _
              ;; This is a monorepo with npm workspaces. Delete root package.json
              ;; and lock files to prevent npm from reading workspace config.
              (delete-file "package.json")
              (for-each delete-file
                        (find-files "." "package-lock\\.json$"))))
          (add-after 'break-monorepo-structure 'change-to-package-directory
            (lambda _
              (chdir "packages/playwright-core")))
          (add-after 'unpack 'skip-browser-download
            (lambda _
              (setenv "PLAYWRIGHT_SKIP_BROWSER_DOWNLOAD" "1")))
          (add-after 'unpack 'unbundle-dependencies
            (lambda _
              (substitute* "packages/playwright-core/src/zipBundle.ts"
                (("require\\('./zipBundleImpl'\\)\\.yazl")
                 "require('yazl')")
                (("require\\('./zipBundleImpl'\\)\\.yauzl")
                 "require('yauzl')")
                (("require\\('./zipBundleImpl'\\)\\.extract")
                 "require('extract-zip')"))
              (substitute* "packages/playwright-core/src/utilsBundle.ts"
                (("require\\('./utilsBundleImpl'\\)\\.colors")
                 "require('colors/safe')")
                (("require\\('./utilsBundleImpl'\\)\\.debug")
                 "require('debug')")
                (("require\\('./utilsBundleImpl'\\)\\.diff")
                 "require('diff')")
                (("require\\('./utilsBundleImpl'\\)\\.dotenv")
                 "require('dotenv')")
                (("require\\('./utilsBundleImpl'\\)\\.getProxyForUrl")
                 "require('proxy-from-env').getProxyForUrl")
                (("require\\('./utilsBundleImpl'\\)\\.HttpsProxyAgent")
                 "require('https-proxy-agent').HttpsProxyAgent")
                (("require\\('./utilsBundleImpl'\\)\\.jpegjs")
                 "require('jpeg-js')")
                (("require\\('./utilsBundleImpl'\\)\\.lockfile")
                 "require('proper-lockfile')")
                (("require\\('./utilsBundleImpl'\\)\\.mime")
                 "require('mime')")
                (("require\\('./utilsBundleImpl'\\)\\.minimatch")
                 "require('minimatch')")
                (("require\\('./utilsBundleImpl'\\)\\.open")
                 "require('open')")
                (("require\\('./utilsBundleImpl'\\)\\.PNG")
                 "require('pngjs').PNG")
                (("require\\('./utilsBundleImpl'\\)\\.program")
                 "require('commander').program")
                (("require\\('./utilsBundleImpl'\\)\\.progress")
                 "require('progress')")
                (("require\\('./utilsBundleImpl'\\)\\.SocksProxyAgent")
                 "require('socks-proxy-agent').SocksProxyAgent")
                (("require\\('./utilsBundleImpl'\\)\\.yaml")
                 "require('yaml')")
                (("require\\('./utilsBundleImpl'\\)\\.wsServer")
                 "require('ws').WebSocketServer")
                (("require\\('./utilsBundleImpl'\\)\\.wsReceiver")
                 "require('ws').Receiver")
                (("require\\('./utilsBundleImpl'\\)\\.wsSender")
                 "require('ws').Sender")
                (("require\\('./utilsBundleImpl'\\)\\.ws\\b")
                 "require('ws')")
                (("require\\('./utilsBundleImpl'\\)\\.StackUtils")
                 "require('stack-utils')"))))
          (add-after 'unbundle-dependencies 'add-unbundled-dependencies
            (lambda _
              ;; Add dependencies that were unbundled from utilsBundle/zipBundle.
              ;; Must run before patch-dependencies which minifies package.json.
              (modify-json
               #:file "packages/playwright-core/package.json"
               (lambda (pkg)
                 (acons "dependencies"
                        '(("colors" . "*")
                          ("commander" . "*")
                          ("debug" . "*")
                          ("diff" . "*")
                          ("dotenv" . "*")
                          ("extract-zip" . "*")
                          ("https-proxy-agent" . "*")
                          ("jpeg-js" . "*")
                          ("mime" . "*")
                          ("minimatch" . "*")
                          ("open" . "*")
                          ("pngjs" . "*")
                          ("progress" . "*")
                          ("proper-lockfile" . "*")
                          ("proxy-from-env" . "*")
                          ("socks-proxy-agent" . "*")
                          ("stack-utils" . "*")
                          ("ws" . "*")
                          ("yaml" . "*")
                          ("yazl" . "*")
                          ("yauzl" . "*"))
                        pkg)))))
          (add-before 'build 'generate-injected-scripts
            (lambda _
              ;; Run generate_injected.js to create src/generated/*.ts files.
              (invoke "node" "../../utils/generate_injected.js")))
          (add-after 'generate-injected-scripts 'add-build-script
            (lambda _
              ;; Upstream uses Babel, but Babel isn't packaged. Use esbuild.
              ;; Compile TypeScript and copy non-TS files (JS, JSON, PNG).
              (modify-json
               (lambda (pkg)
                 (acons "scripts"
                        '(("build" . "find src -name '*.ts' | xargs esbuild --outdir=lib --format=cjs --platform=node && cp -r src/third_party lib/ && find src \\( -name '*.json' -o -name '*.png' \\) -exec sh -c 'mkdir -p lib/$(dirname ${0#src/}) && cp $0 lib/${0#src/}' {} \\;"))
                        pkg)))))
          (add-before 'install 'set-cc
            (lambda _
              (setenv "CC" #$(cc-for-target))))
          (add-after 'install 'wrap-playwright-cli
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (cli (string-append out "/lib/node_modules/playwright-core/cli.js")))
                (wrap-program cli
                  ;; Avoid vite dependency.
                  `("PW_CODEGEN_NO_INSPECTOR" = ("1"))
                  `("PLAYWRIGHT_SKIP_BROWSER_DOWNLOAD" = ("1")))))))))
    (inputs
     (list node-esbuild
           ;; Dependencies (unbundled from utilsBundle).
           node-colors
           node-commander
           node-debug
           node-diff
           node-dotenv
           node-graceful-fs
           node-https-proxy-agent
           node-jpeg-js
           node-mime
           node-minimatch-3
           node-ms
           node-open
           node-pngjs
           node-progress
           node-proper-lockfile
           node-proxy-from-env
           node-retry
           node-signal-exit
           node-socks-proxy-agent
           node-stack-utils
           node-ws
           node-yaml
           ;; Dependencies (unbundled from zipBundle).
           node-yazl
           node-yauzl
           node-extract-zip))
    (native-inputs
     (list esbuild python))
    (native-search-paths
     (list
      (search-path-specification
       (variable "PLAYWRIGHT_CHROMIUM_EXECUTABLE_PATH")
       (file-type 'regular)
       (separator #f)              ;single entry
       (files '("bin/chromium")))
      (search-path-specification
       (variable "PWTEST_CLI_EXECUTABLE_PATH")
       (file-type 'regular)
       (separator #f)              ;single entry
       (files '("bin/chromium")))))
    (home-page "https://playwright.dev/")
    (synopsis "Browser automation library (Chromium-only)")
    (description
     "Playwright is a framework for browser automation and end-to-end testing.
This package provides @code{playwright-core}, the library without bundled
browsers.

@strong{Important}: This package only supports Chromium-based browsers.
Firefox and WebKit are not supported because Playwright requires custom-patched
versions of those browsers that are not packaged for Guix.

To use Playwright with the system Chromium, either:
@enumerate
@item
Set the environment variable @env{PLAYWRIGHT_CHROMIUM_EXECUTABLE_PATH} to the
Chromium binary path, or
@item
Use the @code{executablePath} option in your Playwright code:
@example
const browser = await chromium.launch(@{
  executablePath: '/run/current-system/profile/bin/chromium'
@});
@end example

For the command line variant $code{playwright-core}, set the environment
variable $env{PWTEST_CLI_EXECUTABLE_PATH} to the Chromium binary path.
@end enumerate")
    (license license:asl2.0)))
