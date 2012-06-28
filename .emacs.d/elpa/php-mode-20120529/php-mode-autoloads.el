;;; php-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (php-mode php-file-patterns php) "php-mode" "php-mode.el"
;;;;;;  (20460 44689))
;;; Generated autoloads from php-mode.el

(let ((loads (get 'php 'custom-loads))) (if (member '"php-mode" loads) nil (put 'php 'custom-loads (cons '"php-mode" loads))))

(add-to-list 'interpreter-mode-alist (cons "php" 'php-mode))

(defvar php-file-patterns '("\\.php[s345t]?\\'" "\\.phtml\\'" "\\.inc\\'") "\
List of file patterns for which to automatically invoke `php-mode'.")

(custom-autoload 'php-file-patterns "php-mode" nil)

(autoload 'php-mode "php-mode" "\
Major mode for editing PHP code.

\\{php-mode-map}

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("php-mode-pkg.el") (20460 44689 488961))

;;;***

(provide 'php-mode-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; php-mode-autoloads.el ends here
