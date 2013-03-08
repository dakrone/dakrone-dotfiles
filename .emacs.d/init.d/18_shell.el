;; eshell
(eval-after-load "em-prompt"
  '(progn
     (set-face-attribute 'eshell-prompt nil
                         :foreground "yellow")))

(defun my/eshell-mode-hook ()
  (define-key eshell-mode-map (kbd "M-r") 'helm-eshell-history))
(add-hook 'eshell-mode-hook 'my/eshell-mode-hook)

(defvar eshell-pop-buffer "*eshell-pop*")
(defvar eshell-prev-buffer nil)

(defun eshell-pop ()
  (interactive)
  (setq eshell-prev-buffer (current-buffer))
  (unless (get-buffer eshell-pop-buffer)
    (save-window-excursion
      (pop-to-buffer (get-buffer-create eshell-pop-buffer))
      (eshell-mode)))
  (popwin:popup-buffer (get-buffer eshell-pop-buffer) :height 20 :stick t))
(global-set-key (kbd "M-g M-s") 'eshell-pop)

(defun eshell/cde ()
  (let* ((file-name (buffer-file-name eshell-prev-buffer))
         (dir (or (and file-name (file-name-directory file-name))
                  (and (eq major-mode 'dired-mode) dired-directory)
                  (with-current-buffer eshell-prev-buffer
                    default-directory))))
    (eshell/cd dir)))

(defun eshell/cdp ()
  (let* ((cmd "git rev-parse --show-toplevel")
         (dir (with-temp-buffer
                (unless (call-process-shell-command cmd nil t)
                  (error "Here is not Git Repository"))
                (goto-char (point-min))
                (buffer-substring-no-properties
                 (point) (line-end-position)))))
    (eshell/cd dir)))

(defun eshell/e (file)
  (let ((curwin (get-buffer-window))
        (filepath (concat default-directory file)))
    (other-window 1)
    (find-file filepath)
    (delete-window curwin)))
