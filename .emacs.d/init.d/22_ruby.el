;; setting for ruby
(autoload 'ruby-mode "ruby-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))

(eval-after-load "ruby-mode"
  '(progn
     ;; style
     (setq ruby-deep-indent-paren nil)

     ;; binding
     (define-key ruby-mode-map (kbd "C-M-a") 'my/ruby-beginning-of-defun)
     (define-key ruby-mode-map (kbd "C-M-e") 'my/ruby-end-of-defun)

     ;; indentation
     (setq ruby-deep-indent-paren nil)
     (defadvice ruby-indent-line (after unindent-closing-paren activate)
       (let ((column (current-column))
             (indent nil)
             (offset 0))
         (save-excursion
           (back-to-indentation)
           (let ((state (syntax-ppss)))
             (setq offset (- column (current-column)))
             (when (and (eq (char-after) ?\))
                        (not (zerop (car state))))
               (goto-char (cadr state))
               (setq indent (current-indentation)))))
         (when indent
           (indent-line-to indent)
           (and (> offset 0) (forward-char offset)))))

     ;; auto insert `end'
     (ruby-end-mode)

     ;; auto insert pair
     (require 'ruby-electric)
     (setq ruby-electric-expand-delimiters-list nil)
     (define-key ruby-mode-map (kbd "M-|") 'my/insert-vertical-bar)

     ;; rsense
     (setq rsense-home (concat user-emacs-directory "elisps/rsense"))
     (add-to-list 'load-path (concat rsense-home "/etc"))
     (require 'rsense)

     ;; my hook
     (add-hook 'ruby-mode-hook 'my/ruby-mode-hook)

     ;; yari
     (require 'yari)
     (define-key ruby-mode-map (kbd "C-c C-d") 'yari-helm)))

(defun my/ruby-mode-hook ()
  ;; auto-complete rsense
  (add-to-list 'ac-sources ac-source-rsense-method)
  (add-to-list 'ac-sources ac-source-rsense-constant))

(defvar yari-helm-source-ri-pages
  '((name . "RI documentation")
    (candidates . (lambda () (yari-ruby-obarray)))
    (action  ("Show with Yari" . yari))
    (candidate-number-limit . 300)
    (requires-pattern . 2)
    "Source for completing RI documentation."))

(defun yari-helm (&optional rehash)
  (interactive (list current-prefix-arg))
  (when current-prefix-arg (yari-ruby-obarray rehash))
  (helm :sources 'yari-helm-source-ri-pages :buffer "*yari*"))

(defun my/insert-vertical-bar ()
  (interactive)
  (insert "||")
  (backward-char 1))

;; Ruby's move defun
(defun my/ruby-beginning-of-defun (&optional arg)
  (interactive "p")
  (and (re-search-backward (concat "^\\s-+\\(" ruby-block-beg-re "\\)\\_>")
                           nil 'move)
       (progn (back-to-indentation) t)))

(defun my/ruby-end-of-defun (&optional arg)
  (interactive "p")
  (and (re-search-forward (concat "^\\s-+\\(" ruby-block-end-re "\\)\\($\\|\\b[^_]\\)")
                          nil 'move (or arg 1))
       (progn (beginning-of-line) t))
  (forward-line 1)
  (back-to-indentation))
