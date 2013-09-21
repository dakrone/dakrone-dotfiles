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

;; binding
(define-key dired-mode-map (kbd "C-M-u") 'dired-up-directory)

(add-hook 'dired-mode-hook (lambda () (hl-line-mode)))
