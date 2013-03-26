;; setting of auto-complete
(require 'popup)
(require 'fuzzy)
(require 'pos-tip)
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories
             (concat user-emacs-directory "ac-dict"))
(global-auto-complete-mode t)
(ac-config-default)

;; Enable auto-complete mode other than default enable modes
(dolist (mode '(magit-log-edit-mode
                markdown-mode))
  (add-to-list 'ac-modes mode))

(setq ac-auto-start t
      ac-use-menu-map t
      ac-quick-help-delay 1.0)

(define-key ac-complete-mode-map (kbd "C-n") 'ac-next)
(define-key ac-complete-mode-map (kbd "C-p") 'ac-previous)
(define-key ac-complete-mode-map (kbd "C-s") 'ac-isearch)
(define-key ac-completing-map (kbd "<tab>") 'ac-complete)

;; for global minor mode
(defun my/auto-complete ()
  (interactive)
  (case major-mode
    (ruby-mode (ac-complete-rsense))
    (python-mode (jedi:complete))
    (otherwise (auto-complete))))

;; look command with auto-complete
(defun ac-look-candidates ()
  (if (not (executable-find "look"))
      (message "error: not found `look'")
    (let ((cmd (format "look -f %s" ac-prefix)))
      (ignore-errors
        (split-string
         (shell-command-to-string cmd) "\n")))))

(defun ac-look ()
  (interactive)
  (auto-complete '(ac-source-look)))

(ac-define-source look
  '((candidates . ac-look-candidates)
    (requires . 2)))

(global-set-key (kbd "M-l") 'ac-look)
