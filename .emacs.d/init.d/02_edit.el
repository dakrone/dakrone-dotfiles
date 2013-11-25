;;;; editing operations

;; Use regexp version as Default
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "M-%") 'query-replace-regexp)

;; copy sexp
(defun my/copy-sexp ()
  (interactive)
  (copy-sexp)
  (message "%s" (thing-at-point 'sexp)))
(global-set-key (kbd "M-C-SPC") 'my/copy-sexp)

;; delete-speces
(defun delete-following-spaces ()
  (interactive)
  (let ((orig-point (point)))
    (save-excursion
      (if current-prefix-arg
          (skip-chars-backward " \t")
        (skip-chars-forward " \t"))
      (delete-region orig-point (point)))))
(global-set-key (kbd "M-k") 'delete-following-spaces)

(defun my/join-line ()
  (interactive)
  (join-line -1))
(global-set-key (kbd "M-K") 'my/join-line)

;; for word delete instead of kill-word and backward-kill-word
(defun delete-word (arg)
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))

(defun delete-cursor-word-or-region ()
  (interactive)
  (if (use-region-p)
      (call-interactively #'kill-region)
    (progn
      (backward-word)
      (delete-word 1))))

(defun my/backward-kill-word (args)
  (interactive "p")
  (let ((from (save-excursion
                (forward-word -1)
                (point)))
        (limit (save-excursion
                 (back-to-indentation)
                 (point))))
    (cond ((bolp) (backward-kill-word args))
          (t
           (delete-region (max from limit) (point))))))

(global-set-key (kbd "C-w") 'delete-cursor-word-or-region)
(global-set-key (kbd "M-d") 'delete-word)
(global-set-key (kbd "M-<backspace>") 'my/backward-kill-word)

;; moving with ace-jump-mode
(eval-after-load "ace-jump-mode"
  '(progn
     (set-face-foreground 'ace-jump-face-foreground "lime green")
     (set-face-bold-p 'ace-jump-face-foreground nil)
     (set-face-underline-p 'ace-jump-face-foreground nil)
     ))

(defun backward-symbol (arg)
  (interactive "p")
  (forward-symbol (- arg)))

(defun my/setup-symbol-moving ()
  (local-set-key (kbd "C-M-f") 'forward-symbol)
  (local-set-key (kbd "C-M-b") 'backward-symbol))

(defun number-rectangle (start end format-string from)
  "Delete (don't save) text in the region-rectangle, then number it."
  (interactive
   (list (region-beginning) (region-end)
         (read-string "Number rectangle: " (if (looking-back "^ *") "%d. " "%d"))
         (read-number "From: " 1)))
  (save-excursion
    (goto-char start)
    (setq start (point-marker))
    (goto-char end)
    (setq end (point-marker))
    (delete-rectangle start end)
    (goto-char start)
    (loop with column = (current-column)
          while (and (<= (point) end) (not (eobp)))
          for i from from   do
          (move-to-column column t)
          (insert (format format-string i))
          (forward-line 1)))
  (goto-char start))

(global-set-key (kbd "C-x r N") 'number-rectangle)

;; moving block
(defvar my/backward-up-list-regexp
  "[{\"(\[]")
(make-variable-buffer-local 'my/backward-up-list-regexp)

(defvar my/down-list-regexp
  "[{\"(\[]")
(make-variable-buffer-local 'my/down-list-regexp)

(defun my/backward-up-list (arg)
  (interactive "p")
  (unless (ignore-errors
            (backward-up-list arg) t)
    (re-search-backward my/backward-up-list-regexp nil t)))

(defun my/down-list (arg)
  (interactive "p")
  (unless (ignore-errors
            (down-list arg) t)
    (re-search-forward my/down-list-regexp nil t)))

(global-set-key (kbd "C-M-u") 'my/backward-up-list)
(global-set-key (kbd "C-M-d") 'my/down-list)

;; like Vim 'f'
(defun my/move-specified-char (arg)
  (interactive "p")
  (let ((regexp (char-to-string (read-char))))
    (cond ((and current-prefix-arg (listp current-prefix-arg))
           (re-search-backward regexp nil t))
          (t
           (forward-char 1)
           (re-search-forward regexp nil t arg)
           (backward-char 1)))))
(global-set-key (kbd "C-M-r") 'my/move-specified-char)

;; Insert next line and previous line('o' and 'O')
(defun edit-next-line ()
  (interactive)
  (end-of-line)
  (newline-and-indent))

(defun edit-previous-line ()
  (interactive)
  (forward-line -1)
  (if (not (= (line-number-at-pos) 1))
      (end-of-line))
  (newline-and-indent))

(global-set-key (kbd "M-o") 'edit-next-line)
(global-set-key (kbd "M-O") 'edit-previous-line)

;; Move matched paren('%')
(defun goto-match-paren (arg)
  "Go to the matching  if on (){}[], similar to vi style of % "
  (interactive "p")
  (cond ((looking-at "[\[\(\{]") (forward-sexp))
        ((looking-back "[\]\)\}]" 1) (backward-sexp))
        ((looking-at "[\]\)\}]") (forward-char) (backward-sexp))
        ((looking-back "[\[\(\{]" 1) (backward-char) (forward-sexp))
        (t nil)))
(define-key ctl-x-map (kbd "%") 'goto-match-paren)

;; grep
(setq grep-command "ag -i --nocolor --nogroup ")

;; highlight specified words
(defun my/add-watchwords ()
  (font-lock-add-keywords
   nil '(("\\<\\(FIXME\\|TODO\\|XXX\\|@@@\\)\\>"
          1 '((:foreground "pink") (:weight bold)) t))))

(add-hook 'prog-mode-hook 'my/add-watchwords)

;; Stolen from
;; http://emacsredux.com/blog/2013/05/22/smarter-navigation-to-the-beginning-of-a-line/
(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to `smarter-move-beginning-of-line'
;; (global-set-key [remap move-beginning-of-line]
;;                 'smarter-move-beginning-of-line)

(add-hook 'prog-mode-hook
          (lambda ()
            (require 'smart-tab)
            (global-smart-tab-mode 1)
            (diminish 'smart-tab-mode "st")
            (add-to-list 'smart-tab-disabled-major-modes 'mu4e-compose-mode)
            (add-to-list 'smart-tab-disabled-major-modes 'erc-mode)
            (add-to-list 'smart-tab-disabled-major-modes 'shell-mode)))
