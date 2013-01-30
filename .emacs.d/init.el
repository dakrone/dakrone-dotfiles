;; initalize all ELPA packages
(package-initialize)
(add-to-list 'load-path "~/.emacs.d/")
(add-to-list 'load-path (concat "~/.emacs.d/" (user-login-name)))
(setq esk-user-dir "esk")

;; loads ~/.emacs.d/hinmanm/<file>.el safely
(     load "safe-load" nil t)
(safe-load "imports" nil t)
(safe-load "clojure-stuff" nil t)
(safe-load "elmer" nil t)
(safe-load "org-stuff" nil t)
(safe-load "sonian" nil t)
(safe-load "mail" nil t)
(safe-load "erc-stuff" nil t)
(safe-load "misc" nil t)
(safe-load "bindings" nil t)
(safe-load "color-theme-dakrone" nil t)
(safe-load "mode-line" nil t)

;; Emacs custom set vars
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(magit-set-upstream-on-push (quote dontask)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(helm-visible-mark ((t (:background "IndianRed4" :foreground "midnight blue")))))
(put 'narrow-to-region 'disabled nil)
