;; html-mode
(defun html-mode-insert-br ()
  (interactive)
  (insert "<br />"))

(defvar html-mode-map nil "keymap used in html-mode")
(unless html-mode-map
  (setq html-mode-map (make-sparse-keymap))
  (define-key html-mode-map (kbd "C-c b") 'html-mode-insert-br))

;; zen-coding
(add-hook 'sgml-mode-hook 'zencoding-mode)
(add-hook 'html-mode-hook 'zencoding-mode)

;; css-mode
(autoload 'css-mode "css-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.css$" . css-mode))

;; for auto-complete
(defvar ac-source-css-property-names
  '((candidates . (loop for property in ac-css-property-alist
                        collect (car property)))))

(defun my-css-mode-hook ()
  (add-to-list 'ac-sources 'ac-source-css-property)
  (add-to-list 'ac-sources 'ac-source-css-property-names))
(add-hook 'css-mode-hook 'my-css-mode-hook)
