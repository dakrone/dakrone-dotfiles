;;; starter-kit-js-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "starter-kit-js" "starter-kit-js.el" (19970
;;;;;;  28522))
;;; Generated autoloads from starter-kit-js.el

(add-to-list 'auto-mode-alist '("\\.json$" . js-mode))

(eval-after-load 'js '(progn (define-key js-mode-map "{" 'paredit-open-curly) (define-key js-mode-map "}" 'paredit-close-curly-and-newline) (add-hook 'js-mode-hook 'esk-paredit-nonlisp) (add-hook 'js-mode-hook 'run-coding-hook) (setq js-indent-level 2) (define-key js-mode-map (kbd ",") 'self-insert-command) (font-lock-add-keywords 'js-mode `(("\\(function *\\)(" (0 (progn (compose-region (match-beginning 1) (match-end 1) "") nil)))))))

;;;***

;;;### (autoloads nil nil ("starter-kit-js-pkg.el") (19970 28522
;;;;;;  819208))

;;;***

(provide 'starter-kit-js-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; starter-kit-js-autoloads.el ends here
