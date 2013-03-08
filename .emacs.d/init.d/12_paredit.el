;;;; Paredit
(require 'paredit)

(dolist (hook '(emacs-lisp-mode-hook
                lisp-interaction-mode-hook
                lisp-mode-hook
                ielm-mode-hook
                scheme-mode-hook
                inferior-scheme-mode-hook
                clojure-mode-hook
                slime-repl-mode-hook))
  (add-hook hook 'enable-paredit-mode))

(define-key paredit-mode-map (kbd "C-(") 'paredit-forward-barf-sexp)
(define-key paredit-mode-map (kbd "C-)") 'paredit-forward-slurp-sexp)

(eval-after-load 'paredit
  '(define-key paredit-mode-map (kbd ")") 'paredit-close-parenthesis))

