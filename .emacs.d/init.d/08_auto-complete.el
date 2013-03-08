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

(setq ac-auto-start nil)

(setq ac-use-menu-map t)
(define-key ac-complete-mode-map (kbd "C-n") 'ac-next)
(define-key ac-complete-mode-map (kbd "C-p") 'ac-previous)
(define-key ac-complete-mode-map (kbd "C-s") 'ac-isearch)
(define-key ac-completing-map (kbd "<tab>") 'ac-complete)

(setq ac-quick-help-delay 1.0)

;; for global minor mode
(defun my/auto-complete ()
  (interactive)
  (case major-mode
    (ruby-mode (ac-complete-rsense))
    (python-mode (jedi:complete))
    (otherwise (auto-complete))))

;; look command with auto-complete
(defun ac-look-candidates ()
  (unless (executable-find "look")
    (error "Please install `look' command"))
  (let ((cmd (format "look -f %s" ac-prefix)))
    (ignore-errors
      (split-string
       (shell-command-to-string cmd) "\n"))))

(defun ac-look ()
  (interactive)
  (auto-complete '(ac-source-look)))

(ac-define-source look
  '((candidates . ac-look-candidates)
    (requires . 2)))

(global-set-key (kbd "C-M-l") 'ac-look)

;; nrepl auto-complete
(add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
(add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'nrepl-mode))

(defun set-auto-complete-as-completion-at-point-function ()
  (setq completion-at-point-functions '(auto-complete)))
(add-hook 'auto-complete-mode-hook
          'set-auto-complete-as-completion-at-point-function)

(add-hook 'nrepl-mode-hook 'set-auto-complete-as-completion-at-point-function)
(add-hook 'nrepl-interaction-mode-hook
          'set-auto-complete-as-completion-at-point-function)

