((haskell-mode
  . ((eval . (setq intero-stack-executable
                   (with-temp-buffer
                     (cd (locate-dominating-file default-directory ".dir-locals.el"))
                     (substring
                      (shell-command-to-string
                       "nix-shell --quiet --pure --run 'command -v intero-nix-shim' 2>&1 | tail -n 1")
                      0 -1)))))))
