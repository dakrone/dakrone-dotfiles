;;;; setting for searching

(defun isearch-yank-symbol ()
  (interactive)
  (isearch-yank-internal (lambda () (forward-symbol 1) (point))))

(define-key isearch-mode-map (kbd "C-M-w") 'isearch-yank-symbol)
