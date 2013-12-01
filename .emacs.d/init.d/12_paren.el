;;; 12_paren.el -- my edits for parenthetical things
;;; Commentary:
;;; Code:

;; Paredit
(defun my/enable-paredit-mode ()
  (interactive)
  (require 'paredit)
  (enable-paredit-mode)
  (define-key paredit-mode-map (kbd "C-(") 'paredit-forward-barf-sexp)
  (define-key paredit-mode-map (kbd "C-)") 'paredit-forward-slurp-sexp)
  (define-key paredit-mode-map (kbd ")") 'paredit-close-parenthesis)
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

;; Smartparens - not smart enough!
;; (require 'smartparens-config)
(smartparens-global-mode t)
;; (smartparens-global-strict-mode t)
(show-smartparens-global-mode t)

(add-hook 'smartparens-mode-hook
          (lambda ()
            (show-smartparens-mode t)))

(add-hook 'sh-mode-hook
          (lambda ()
            ;; Remove when https://github.com/Fuco1/smartparens/issues/257
            ;; is fixed
            (setq sp-autoescape-string-quote nil)))

(sp-use-paredit-bindings)

(define-key sp-keymap (kbd "C-(") 'sp-forward-barf-sexp)
(define-key sp-keymap (kbd "C-)") 'sp-forward-slurp-sexp)
(define-key sp-keymap (kbd "C-M-f") 'sp-forward-sexp)
(define-key sp-keymap (kbd "C-M-b") 'sp-backward-sexp)

(sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)

(sp-with-modes '(html-mode sgml-mode)
  (sp-local-pair "<" ">"))

(sp-with-modes sp--lisp-modes
  (sp-local-pair "(" nil :bind "C-("))

;;; 12_paren.el ends here
