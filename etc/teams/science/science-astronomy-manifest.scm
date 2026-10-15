;; Subset of Science team collection coverting Astro* related packages.

(use-modules (guix diagnostics)
             (guix packages)
             (ice-9 regex))

(define (package-in-module? package modules)
  (let ((modules-re (string-append "^gnu/packages/("
                                   (string-join modules "|") ")\\.scm$")))
    (and (package-definition-location package)
         (string-match modules-re
                       (location-file (package-definition-location package))))))

(manifest
 (map package->manifest-entry
      (fold-packages
       (lambda (package lst)
         (if (package-in-module? package '("astronomy"))
             (cons package lst)
             lst))
       '())))
