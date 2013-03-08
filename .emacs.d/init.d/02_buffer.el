;;;; setting about `buffer'

;; auto-save
(defun my/auto-save-buffers ()
  (save-excursion
    (dolist (buffer (buffer-list))
      (set-buffer buffer)
      (when (and (buffer-file-name)
                 (buffer-modified-p)
                 (not buffer-read-only)
                 (file-writable-p (buffer-file-name)))
        (save-buffer)))))

(run-with-idle-timer 10 t 'my/auto-save-buffers)

;; winner-mode
(require 'winner)
(winner-mode t)

;; move other window
(setq windmove-wrap-around t)
(windmove-default-keybindings)

;; naming of same name file
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; use ibuffer instead of list-buffer
(defalias 'list-buffers 'ibuffer)

;; mark 'D'(delete) for matching buffer
(require 'ibuffer)
(defun ibuffer-menu-grep-delete (str)
  (interactive
   (list (read-string "Delete Mark Regexp: ")))
  (save-excursion
    (goto-char (point-min))
    (forward-line 2)
    (while (re-search-forward str nil t)
      (save-excursion
        (ibuffer-mark-for-delete nil))
      (end-of-line))))

(define-key ibuffer-mode-map "R" 'ibuffer-menu-grep-delete)

;; based on http://ergoemacs.org/emacs/elisp_examples.html
(defvar my/cycle-buffer-limit 30)

(defun my/buffer-not-switch-p ()
  (or (string-match "^*" (buffer-name)) (eq major-mode 'dired-mode)
      (eq major-mode 'direx:direx-mode)))

(defun my/next-buffer ()
  (interactive)
  (next-buffer)
  (let ((i 0))
    (while (and (my/buffer-not-switch-p) (< i my/cycle-buffer-limit))
      (incf i)
      (next-buffer))))

(defun my/previous-buffer ()
  (interactive)
  (previous-buffer)
  (let ((i 0))
    (while (and (my/buffer-not-switch-p) (< i my/cycle-buffer-limit))
      (incf i)
      (previous-buffer))))

(global-set-key (kbd "M-9") 'my/next-buffer)
(global-set-key (kbd "M-0") 'my/previous-buffer)

(defun my/push-mark ()
  (interactive)
  (push-mark))
(global-set-key (kbd "C-x C-SPC") 'my/push-mark)
(global-set-key (kbd "C-x C-x") 'pop-global-mark)
