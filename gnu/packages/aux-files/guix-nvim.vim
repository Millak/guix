lua << EOF
  -- This appends all applicable neovim paths to the runtimepath.
  for _, directory in pairs({"/run/current-system/profile", "~/.guix-profile", "~/.guix-home/profile", vim.env.GUIX_PROFILE, vim.env.GUIX_ENVIRONMENT}) do
    local rtp = vim.fs.abspath(directory .. "/share/nvim/site")
    local stat = vim.uv.fs_stat(rtp)
    if stat and stat.type == "directory" then
      vim.opt.rtp:append(rtp)
    end
  end
EOF
