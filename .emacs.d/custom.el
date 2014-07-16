(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(compilation-scroll-output t)
 '(compilation-skip-threshold 2)
 '(custom-safe-themes
   (quote
    ("6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f" "756597b162f1be60a12dbd52bab71d40d6a2845a3e3c2584c6573ee9c332a66e" "025354235e98db5e7fd9c1a74622ff53ad31b7bde537d290ff68d85665213d85" "6fe6ab4abe97a4f13533e47ae59fbba7f2919583f9162b440dd06707b01f7794" "76b226dd750d085eaaf7efa5eb07a3282223d74f327a0f4319512c0a59f6df39" "9a217ee1dcefd5e83f78381c61e25e9c4d25c7b80bf032f44d7d62ca68c6a384" "fc6e906a0e6ead5747ab2e7c5838166f7350b958d82e410257aeeb2820e8a07a" default)))
 '(delete-selection-mode t)
 '(org-export-backends (quote (ascii html latex md gfm odt)))
 '(org-modules
   (quote
    (org-bbdb org-bibtex org-docview org-gnus org-habit org-info org-irc org-mhe org-rmail org-w3m)))
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
 '(default ((t (:background nil)))))
