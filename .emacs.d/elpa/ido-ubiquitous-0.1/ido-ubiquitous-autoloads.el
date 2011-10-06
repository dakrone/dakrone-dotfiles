;;; ido-ubiquitous-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "ido-ubiquitous" "ido-ubiquitous.el" (20109
;;;;;;  52803))
;;; Generated autoloads from ido-ubiquitous.el

(defvar ido-ubiquitous-enabled t "\
If non-nil, use ido-completing-read instead of completing-read if possible.
    
  Set it to nil using let in around-advice for functions where the
  original completing-read is required.  For example, if a function
  foo absolutely must use the original completing-read, define some
  advice like this:
    
  (defadvice foo (around original-completing-read-only activate)
    (let (ido-ubiquitous-enabled) ad-do-it))")

(defadvice completing-read (around use-ido-when-possible activate) (if (or (not ido-ubiquitous-enabled) (and (boundp 'ido-cur-list) ido-cur-list)) ad-do-it (let ((allcomp (all-completions "" collection predicate))) (if allcomp (setq ad-return-value (ido-completing-read prompt allcomp nil require-match initial-input hist def)) ad-do-it))))

;;;***

;;;### (autoloads nil nil ("ido-ubiquitous-pkg.el") (20109 52803
;;;;;;  855845))

;;;***

(provide 'ido-ubiquitous-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ido-ubiquitous-autoloads.el ends here
