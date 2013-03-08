;;;; dired
(require 'dired)
;; Not create new buffer, if you chenge directory in dired
(put 'dired-find-alternate-file 'disabled nil)
(define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
;; display directories by first
(load-library "ls-lisp")
(setq ls-lisp-dirs-first t)
;; recursive copy, remove
(setq dired-recursive-copies 'always)
(setq dired-recursive-deletes 'always)

;; dired-x
(load "dired-x")

;; binding
(define-key dired-mode-map (kbd "C-M-u") 'dired-up-directory)

;; wdired
(eval-after-load "wdired"
  '(progn
     (define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)))

;; direx
;; (require 'direx)
;; (require 'direx-project)
;; (defun my/dired-jump ()
;;   (interactive)
;;   (cond (current-prefix-arg
;;          (dired-jump))
;;         ((not (one-window-p))
;;          (or (ignore-errors
;;                (direx-project:jump-to-project-root) t)
;;              (direx:jump-to-directory)))
;;         (t
;;          (or (ignore-errors
;;                (direx-project:jump-to-project-root-other-window) t)
;;              (direx:jump-to-directory-other-window)))))

;; (global-set-key (kbd "C-x C-j") 'my/dired-jump)

(add-hook 'dired-mode-hook (lambda () (hl-line-mode)))
