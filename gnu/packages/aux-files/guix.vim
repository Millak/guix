" This appends all applicable vim paths to the end of packagepath.  Once we
" have told vim the packagepath vim will add it to the runtimepath for us.
for directory in ["/run/current-system/profile", $HOME . "/.guix-profile", $HOME ."/.guix-home/profile", $GUIX_PROFILE, $GUIX_ENVIRONMENT]
    let vimplugins = directory . "/share/vim/vimfiles"
    if isdirectory(vimplugins)
        let &pp = join([&pp,vimplugins], ',')
    endif
endfor
