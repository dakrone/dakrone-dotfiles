;; use space not use tab
(setq-default indent-tabs-mode nil)

;; delete trailling space and blank line tail of file
(defun my/cleanup-for-spaces ()
  (interactive)
  (delete-trailing-whitespace)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-max))
      (delete-blank-lines))))

(when window-system
  (add-hook 'before-save-hook 'my/cleanup-for-spaces))
