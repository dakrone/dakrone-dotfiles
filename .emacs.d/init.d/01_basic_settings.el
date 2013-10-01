;; encoding
(prefer-coding-system 'utf-8-unix)

;; Coloring
(global-font-lock-mode t)

;; temp directory
(when (file-exists-p "/mnt/ramdisk")
  (setq temporary-file-directory "/mnt/ramdisk/"))

;; for GC
(setq gc-cons-threshold (* gc-cons-threshold 10))

;; echo stroke
(setq echo-keystrokes 0.1)

;; large file
(setq large-file-warning-threshold (* 25 1024 1024))

;; saveplace
(savehist-mode 1)
(load "saveplace")
(setq-default save-place t)

(setq dabbrev-case-fold-search nil)

;; highlight mark region
(transient-mark-mode t)

;; indicate last line
(setq-default indicate-empty-lines t)
(setq-default indicate-buffer-boundaries 'right)

;; Disable default scroll bar and tool bar
(when window-system
  (set-scroll-bar-mode 'nil)
  (tool-bar-mode 0))

;; enable yascrollbar
;; (global-yascroll-bar-mode)
;; (add-to-list 'yascroll:disabled-modes 'org-mode)

;; not create backup file
(setq backup-inhibited t
      delete-auto-save-files t)

;; Disable menu bar
(menu-bar-mode -1)

;; don't beep
(setq ring-bell-function (lambda()))

;; don't display start message
(setq inhibit-startup-message t)

;; display line infomation
(line-number-mode 1)
(column-number-mode 1)

;; to send clip board
(setq x-select-enable-clipboard t)

;; ignore upper or lower
(setq read-file-name-completion-ignore-case t)

;; yes-or-no-p
(defalias 'yes-or-no-p 'y-or-n-p)

;; move by visual line
(setq line-move-visual t)

;; which-func
;; (require 'which-func)
;; (set-face-foreground 'which-func "chocolate4")
;; (set-face-bold-p 'which-func t)
;; (which-function-mode t)

;; invisible mouse cursor when editing text
(setq make-pointer-invisible t)

;; Diminish things
;; "I will diminish, and go into the West..."
(require 'diminish)

;; undo-tree
(require 'undo-tree)
(global-undo-tree-mode)
(diminish 'undo-tree-mode)

(define-key undo-tree-map (kbd "C-x u") 'undo-tree-visualize)
(define-key undo-tree-map (kbd "C-/") 'undo-tree-undo)

;; fill-mode
(setq fill-column 80)

;; move to mark position
(setq set-mark-command-repeat-pop t)

;; fixed line position after scrollup, scrolldown
(defadvice scroll-up (around scroll-up-relative activate)
  "Scroll up relatively without move of cursor."
  (let ((orig-line (count-lines (window-start) (point))))
    ad-do-it
    (move-to-window-line orig-line)))

(defadvice scroll-down (around scroll-down-relative activate)
  "Scroll down relatively without move of cursor."
  (let ((orig-line (count-lines (window-start) (point))))
    ad-do-it
    (move-to-window-line orig-line)))

;; expand symbolic link
(setq-default find-file-visit-truename t)

;; dim parens
(add-hook 'prog-mode-hook (lambda () (require 'parenface)))

;; Maximize
(when window-system
  (require 'maxframe)
  (add-hook 'window-setup-hook 'maximize-frame t))

