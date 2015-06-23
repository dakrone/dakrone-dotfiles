(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/bookmarks")
 '(c-default-style
   (quote
    ((java-mode . "intellij")
     (awk-mode . "awk")
     (other . "gnu"))))
 '(comint-buffer-maximum-size 20000)
 '(comint-completion-addsuffix t)
 '(comint-get-old-input (lambda nil "") t)
 '(comint-input-ignoredups t)
 '(comint-input-ring-size 5000)
 '(comint-move-point-for-output nil)
 '(comint-prompt-read-only nil)
 '(comint-scroll-show-maximum-output t)
 '(comint-scroll-to-bottom-on-input t)
 '(compilation-message-face (quote default))
 '(compilation-scroll-output t)
 '(compilation-skip-threshold 2)
 '(custom-safe-themes
   (quote
    ("3119b66b441eaa36acad473952dfdf901a5924b1fbc995b58477f031e12547c4" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "53e1387d9dc9b0794f2f148699bfc9fb2c5662e0faf68e4e832e98057d2cd539" "c3232d379e847938857ca0408b8ccb9d0aca348ace6f36a78f0f7b4c5df0115c" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "357d5abe6f693f2875bb3113f5c031b7031f21717e8078f90d9d9bc3a14bcbd8" default)))
 '(diredp-hide-details-initially-flag nil)
 '(esv-key "IP")
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-type (quote cabal-repl))
 '(haskell-tags-on-save t)
 '(helm-external-programs-associations (quote (("odt" . "open"))))
 '(logstash-indent 2)
 '(magit-set-upstream-on-push (quote dontask))
 '(magit-use-overlays nil)
 '(org-export-backends (quote (ascii html latex md reveal)))
 '(org-modules
   (quote
    (org-bbdb org-bibtex org-docview org-gnus org-habit org-info org-mhe org-rmail org-w3m)))
 '(protect-buffer-bury-p nil)
 '(safe-local-variable-values
   (quote
    ((eval column-marker-1 200)
     (global-color-identifiers-mode . -1)
     (color-identifiers-mode . -1)
     (eval ispell-change-dictionary "en_US")
     (flyspell-default-dictionary . "en_US")
     (eval when
           (and
            (buffer-file-name)
            (file-regular-p
             (buffer-file-name))
            (string-match-p "^[^.]"
                            (buffer-file-name)))
           (emacs-lisp-mode)
           (when
               (fboundp
                (quote flycheck-mode))
             (flycheck-mode -1))
           (unless
               (featurep
                (quote package-build))
             (let
                 ((load-path
                   (cons ".." load-path)))
               (require
                (quote package-build))))
           (package-build-minor-mode))
     (fill-column 140)
     (whitespace-line-column 140)
     (tab-width 4)
     (c-basic-offset 2)
     (c-basic-offset 4)
     (tab-width 2))))
 '(sp-cancel-autoskip-on-backward-movement nil)
 '(yas-prompt-functions (quote (my-yas/prompt))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
