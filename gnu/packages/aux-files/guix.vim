" This appends all of the vim plugins to the end of Vim's runtimepath.
for directory in ["/run/current-system/profile", $HOME . "/.guix-profile", $HOME ."/.guix-home/profile", $GUIX_PROFILE, $GUIX_ENVIRONMENT]
    let vimplugins = directory . "/share/vim/vimfiles"
    if isdirectory(vimplugins)
        let &rtp = join([&rtp,vimplugins], ',')
    endif
endfor
" Unconditionally add */after directories last, as intended by upstream
" TODO: Remove duplicate */after directories
for directory in [$VIM . "/vimfiles", $HOME ."/.vim"]
    let vimplugins = directory . "/after"
    let &rtp = join([&rtp,vimplugins], ',')
endfor
