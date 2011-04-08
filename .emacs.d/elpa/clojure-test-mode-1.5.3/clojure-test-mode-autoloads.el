;;; clojure-test-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (clojure-test-mode) "clojure-test-mode" "clojure-test-mode.el"
;;;;;;  (19839 38750))
;;; Generated autoloads from clojure-test-mode.el

(autoload 'clojure-test-mode "clojure-test-mode" "\
A minor mode for running Clojure tests.

\(fn &optional ARG)" t nil)

(defun clojure-test-maybe-enable nil "\
Enable clojure-test-mode if the current buffer contains a namespace 
with a \"test.\" bit on it." (let ((ns (clojure-find-package))) (when (search "test." ns) (save-window-excursion (clojure-test-mode t)))))

(add-hook 'clojure-mode-hook 'clojure-test-maybe-enable)

;;;***

;;;### (autoloads nil nil ("clojure-test-mode-pkg.el") (19839 38750
;;;;;;  391382))

;;;***

(provide 'clojure-test-mode-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; clojure-test-mode-autoloads.el ends here
