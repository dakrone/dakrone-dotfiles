;;;; Paredit
(defun my/enable-paredit-mode ()
  (interactive)
  (require 'paredit)
  (enable-paredit-mode)
  (define-key paredit-mode-map (kbd "C-(") 'paredit-forward-barf-sexp)
  (define-key paredit-mode-map (kbd "C-)") 'paredit-forward-slurp-sexp)
  (diminish 'paredit-mode "()"))

(dolist (hook '(emacs-lisp-mode-hook
                lisp-interaction-mode-hook
                lisp-mode-hook
                ielm-mode-hook
                scheme-mode-hook
                inferior-scheme-mode-hook
                clojure-mode-hook
                slime-repl-mode-hook))
  (add-hook hook 'my/enable-paredit-mode))


(eval-after-load 'paredit
  '(define-key paredit-mode-map (kbd ")") 'paredit-close-parenthesis))

(global-set-key (kbd "C-x M-p") 'my/enable-paredit-mode)
