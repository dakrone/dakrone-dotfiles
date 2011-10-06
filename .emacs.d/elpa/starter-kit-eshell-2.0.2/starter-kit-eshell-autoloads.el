;;; starter-kit-eshell-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (eshell/cds) "starter-kit-eshell" "starter-kit-eshell.el"
;;;;;;  (20109 52841))
;;; Generated autoloads from starter-kit-eshell.el

(eval-after-load 'esh-opt '(progn (require 'em-prompt) (require 'em-term) (require 'em-cmpl) (setenv "PAGER" "cat") (set-face-attribute 'eshell-prompt nil :foreground "turquoise1") (add-hook 'eshell-mode-hook '(lambda nil (define-key eshell-mode-map "" 'eshell-bol))) (add-to-list 'eshell-output-filter-functions 'eshell-handle-ansi-color) (setq eshell-cmpl-cycle-completions nil) (add-to-list 'eshell-visual-commands "ssh") (add-to-list 'eshell-visual-commands "tail") (add-to-list 'eshell-command-completions-alist '("gunzip" "gz\\'")) (add-to-list 'eshell-command-completions-alist '("tar" "\\(\\.tar|\\.tgz\\|\\.tar\\.gz\\)\\'"))))

(autoload 'eshell/cds "starter-kit-eshell" "\
Change directory to the project's root.

\(fn)" nil nil)

(when (not (functionp 'eshell/find)) (defun eshell/find (dir &rest opts) (find-dired dir (mapconcat (lambda (arg) (if (get-text-property 0 'escaped arg) (concat "\"" arg "\"") arg)) opts " "))))

(when (not (functionp 'eshell/rgrep)) (defun eshell/rgrep (&rest args) "Use Emacs grep facility instead of calling external grep." (eshell-grep "rgrep" args t)))

;;;***

;;;### (autoloads nil nil ("starter-kit-eshell-pkg.el") (20109 52841
;;;;;;  472217))

;;;***

(provide 'starter-kit-eshell-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; starter-kit-eshell-autoloads.el ends here
