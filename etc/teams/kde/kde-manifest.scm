(use-modules (guix diagnostics)
             (guix packages)
             (ice-9 regex))

(manifest
 (map package->manifest-entry
      (fold-packages
       (lambda (package lst)
         (if (and (package-definition-location package)
                  (string-match "^gnu/packages/(kde)(-.+|)\\.scm$"
                                (location-file (package-definition-location package))))
             (cons package lst)
             lst))
       '())))
