(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
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
    ("357d5abe6f693f2875bb3113f5c031b7031f21717e8078f90d9d9bc3a14bcbd8" default)))
 '(delete-selection-mode t)
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
    (org-bbdb org-bibtex org-docview org-gnus org-habit org-info org-irc org-mhe org-rmail org-w3m)))
 '(paradox-automatically-star t)
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
