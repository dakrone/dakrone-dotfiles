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

(global-set-key (kbd "M-9") 'bs-cycle-next)
(global-set-key (kbd "M-0") 'bs-cycle-previous)
