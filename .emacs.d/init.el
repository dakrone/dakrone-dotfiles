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

;; keep customize settings in their own file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
(put 'upcase-region 'disabled nil)
