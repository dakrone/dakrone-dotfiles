(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(comint-buffer-maximum-size 20000)
 '(comint-completion-addsuffix t)
 '(comint-get-old-input (lambda nil "") t)
 '(comint-input-ignoredups t)
 '(comint-input-ring-size 5000)
 '(comint-move-point-for-output nil)
 '(comint-prompt-read-only nil)
 '(comint-scroll-show-maximum-output t)
 '(comint-scroll-to-bottom-on-input t)
 '(compilation-scroll-output t)
 '(compilation-skip-threshold 2)
 '(custom-safe-themes
   (quote
    ("3a727bdc09a7a141e58925258b6e873c65ccf393b2240c51553098ca93957723" "53e29ea3d0251198924328fd943d6ead860e9f47af8d22f0b764d11168455a8e" "96b023d1a6e796bab61b472f4379656bcac67b3af4e565d9fb1b6b7989356610" "696927ed40057050d219989dd473cc08fab27995e4febd6cf14429a448364bc6" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "e8ff60c7811d4532ee9f756b654d2f13d455e04851ee60c5e033e1b6a17e968f" "e515bdcabbbd48e8dbcb086353f29c8c6ca1f04f8b61c287056db66c87559ed5" "75c9f0b0499ecdd0c856939a5de052742d85af81814e84faa666522c2bba7e85" "6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f" "756597b162f1be60a12dbd52bab71d40d6a2845a3e3c2584c6573ee9c332a66e" "025354235e98db5e7fd9c1a74622ff53ad31b7bde537d290ff68d85665213d85" "6fe6ab4abe97a4f13533e47ae59fbba7f2919583f9162b440dd06707b01f7794" "76b226dd750d085eaaf7efa5eb07a3282223d74f327a0f4319512c0a59f6df39" "9a217ee1dcefd5e83f78381c61e25e9c4d25c7b80bf032f44d7d62ca68c6a384" "fc6e906a0e6ead5747ab2e7c5838166f7350b958d82e410257aeeb2820e8a07a" default)))
 '(delete-selection-mode t)
 '(magit-set-upstream-on-push (quote dontask))
 '(org-export-backends (quote (ascii html latex md gfm odt)))
 '(org-modules
   (quote
    (org-bbdb org-bibtex org-docview org-gnus org-habit org-info org-irc org-mhe org-rmail org-w3m)))
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
