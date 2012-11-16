;;
;;     File:   /u3/ccmproc2/emacs/safe-load.el  (939 bytes)
;;     Date:   Tue Dec  1 09:26:25 1992
;;     Author: Lawrence Buja <ccmproc2@sunny>
;;
(defvar safe-load-error-list ""
  "*List of files that reported errors when loaded via safe-load")

(defun safe-load (file &optional noerror nomessage nosuffix)
  "Load a file.  If error when loading, report back, wait for
   a key stroke then continue on"
  (interactive "f")
  (condition-case nil (load file noerror nomessage nosuffix)
    (error
     (progn
       (setq safe-load-error-list  (concat safe-load-error-list  " " file))
       (message "****** [Return to continue] Error loading %s" safe-load-error-list )
       (sleep-for 1)
       nil))))

(defun safe-load-check ()
  "Check for any previous safe-load loading errors.  (safe-load.el)"
  (interactive)
  (if (string-equal safe-load-error-list "") ()
    (message (concat "****** error loading: " safe-load-error-list))))
