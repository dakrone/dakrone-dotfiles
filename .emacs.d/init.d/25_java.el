;; Java things

(add-hook
 'java-mode-hook
 (lambda ()
   (setq tab-width 4)))

;; Malabar things
;; (add-to-list 'load-path "~/src/elisp/malabar-1.5.0-SNAPSHOT/lisp")

;; (setq semantic-default-submodes '(global-semantic-idle-scheduler-mode
;;                                   global-semanticdb-minor-mode
;;                                   global-semantic-idle-summary-mode
;;                                   global-semantic-mru-bookmark-mode))
;; (semantic-mode 1)
;; (require 'malabar-mode)
;; (setq malabar-groovy-lib-dir "~/src/elisp/malabar-1.5.0-SNAPSHOT/lib")
;; (add-to-list 'auto-mode-alist '("\\.java\\'" . malabar-mode))

;; (add-hook 'malabar-mode-hook
;;           (lambda ()
;;             (add-hook 'after-save-hook 'malabar-compile-file-silently
;;                       nil t)))
