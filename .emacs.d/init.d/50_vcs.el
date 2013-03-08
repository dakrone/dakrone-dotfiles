;;;; Common VCS setting
(global-auto-revert-mode 1)
(setq vc-follow-symlinks t)
(setq auto-revert-check-vc-info t)

;; disable vc-mode
(setq vc-handled-backends '())

;;; Setting for Git
;; sgit
;; (require 'sgit)
;; (global-set-key (kbd "C-x v l") 'sgit:log)
;; (global-set-key (kbd "C-x v d") 'sgit:diff)
;; (global-set-key (kbd "C-x v s") 'sgit:status)

;; git-gutter
(global-git-gutter-mode t)
(global-set-key (kbd "C-x C-a") 'git-gutter:toggle)
(global-set-key (kbd "C-x =") 'git-gutter:popup-diff)

;; (eval-after-load "git-gutter"
;;   '(progn
;;      (setq git-gutter:modified-sign " ")
;;      (setq git-gutter:deleted-sign-sign " ")
;;      (set-face-background 'git-gutter:deleted  "red")
;;      (set-face-background 'git-gutter:modified "purple")))

;; Jump to next/previous hunk
(global-set-key (kbd "C-c P") 'git-gutter:previous-hunk)
(global-set-key (kbd "C-c N") 'git-gutter:next-hunk)


;; magit
(global-set-key (kbd "M-g M-g") 'magit-status)
(eval-after-load "magit"
  '(progn
     (define-key magit-mode-map (kbd "C-c C-b") 'magit-browse)
     (define-key magit-status-mode-map (kbd "W") 'magit-toggle-whitespace)

     ;; faces
     (set-face-attribute 'magit-branch nil
                         :foreground "yellow" :weight 'bold :underline t)
     (set-face-attribute 'magit-item-highlight nil
                         :background "gray3")
     (custom-set-variables
      '(magit-set-upstream-on-push (quote dontask)))))

(defun magit-toggle-whitespace ()
  (interactive)
  (if (member "-w" magit-diff-options)
      (magit-dont-ignore-whitespace)
    (magit-ignore-whitespace)))

(defun magit-ignore-whitespace ()
  (interactive)
  (add-to-list 'magit-diff-options "-w")
  (magit-refresh))

(defun magit-dont-ignore-whitespace ()
  (interactive)
  (setq magit-diff-options (remove "-w" magit-diff-options))
  (magit-refresh))

(defun my/magit-log-edit-mode-hook ()
  (flyspell-mode t)
  (push 'ac-source-look ac-sources))
(add-hook 'magit-log-edit-mode-hook 'my/magit-log-edit-mode-hook)

(defun magit-browse ()
  (interactive)
  (let ((url (with-temp-buffer
               (unless (zerop (call-process-shell-command "git remote -v" nil t))
                 (error "Failed: 'git remote -v'"))
               (goto-char (point-min))
               (when (re-search-forward "github\\.com[:/]\\(.+?\\)\\.git" nil t)
                 (format "https://github.com/%s" (match-string 1))))))
    (unless url
      (error "Can't find repository URL"))
    (browse-url url)))
