(use-modules (gnu home)
             (gnu home services)
             (gnu home services shells)
             (gnu services)
             (gnu packages admin)
             (guix gexp))


(home-environment
  (packages (list htop))
  (services
   (append (list
            (service home-bash-service-type
                     (home-bash-configuration
                      (guix-defaults? #t)
                      (variables
                       `(("HISTFILE" . "$XDG_CACHE_HOME/.bash_history")
                         ("HISTSIZE" . "50000")))))

            (simple-service 'test-config
                            home-xdg-configuration-files-service-type
                            (list `("test.conf"
                                    ,(plain-file "tmp-file.txt"
                                                 "the content of
                                          ~/.config/test.conf")))))
           %base-home-services)))

