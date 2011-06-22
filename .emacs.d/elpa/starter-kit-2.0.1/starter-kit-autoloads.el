;;; starter-kit-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "starter-kit" "starter-kit.el" (19970 28469))
;;; Generated autoloads from starter-kit.el

(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode)) (when (fboundp mode) (funcall mode -1)))

(dolist (l '(uniquify starter-kit-defuns starter-kit-misc starter-kit-eshell)) (require l))

(setq esk-system-config (concat user-emacs-directory system-name ".el") esk-user-config (concat user-emacs-directory user-login-name ".el") esk-user-dir (concat user-emacs-directory user-login-name))

(add-to-list 'load-path esk-user-dir)

(when (file-exists-p esk-system-config) (load esk-system-config))

(when (file-exists-p esk-user-config) (load esk-user-config))

(when (file-exists-p esk-user-dir) (dolist (l (directory-files esk-user-dir nil "^[^#].*el$")) (load l)))

;;;***

;;;### (autoloads nil nil ("starter-kit-defuns.el" "starter-kit-eshell.el"
;;;;;;  "starter-kit-misc.el" "starter-kit-pkg.el") (19970 28469
;;;;;;  399529))

;;;***

(provide 'starter-kit-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; starter-kit-autoloads.el ends here
