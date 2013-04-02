;; initalize all ELPA packages
(require 'package)

(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)

;; load environment value
(let ((shellenv (concat user-emacs-directory "shellenv.el")))
  (when (file-exists-p shellenv)
    (load-file shellenv)
    (dolist (path (reverse (split-string (getenv "PATH") ":")))
      (add-to-list 'exec-path path))))

;; init-loader
(require 'init-loader)
(init-loader-load (concat user-emacs-directory "init.d"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(yas-prompt-functions (quote (my-yas/prompt))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
