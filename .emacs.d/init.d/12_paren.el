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
(require 'smartparens-config)
(smartparens-global-strict-mode t)
;; (smartparens-global-strict-mode t)
(show-smartparens-global-mode t)

(add-hook 'sh-mode-hook
          (lambda ()
            ;; Remove when https://github.com/Fuco1/smartparens/issues/257
            ;; is fixed
            (setq sp-autoescape-string-quote nil)))

;;(sp-use-paredit-bindings)

(define-key sp-keymap (kbd "C-(") 'sp-forward-barf-sexp)
(define-key sp-keymap (kbd "C-)") 'sp-forward-slurp-sexp)

(define-key sp-keymap (kbd "C-M-f") 'sp-forward-sexp)
(define-key sp-keymap (kbd "C-M-b") 'sp-backward-sexp)
(define-key sp-keymap (kbd "C-M-f") 'sp-forward-sexp)
(define-key sp-keymap (kbd "C-M-b") 'sp-backward-sexp)

(define-key sp-keymap (kbd "C-M-d") 'sp-down-sexp)
(define-key sp-keymap (kbd "C-M-a") 'sp-backward-down-sexp)
(define-key sp-keymap (kbd "C-S-a") 'sp-beginning-of-sexp)
(define-key sp-keymap (kbd "C-S-d") 'sp-end-of-sexp)

(define-key sp-keymap (kbd "C-M-e") 'sp-up-sexp)
(define-key emacs-lisp-mode-map (kbd ")") 'sp-up-sexp)
(define-key sp-keymap (kbd "C-M-u") 'sp-backward-up-sexp)
(define-key sp-keymap (kbd "C-M-t") 'sp-transpose-sexp)

(define-key sp-keymap (kbd "C-M-n") 'sp-next-sexp)
(define-key sp-keymap (kbd "C-M-p") 'sp-previous-sexp)

(define-key sp-keymap (kbd "C-M-k") 'sp-kill-sexp)
(define-key sp-keymap (kbd "C-M-w") 'sp-copy-sexp)

(define-key sp-keymap (kbd "C-<right>") 'sp-forward-slurp-sexp)
(define-key sp-keymap (kbd "C-<left>") 'sp-forward-barf-sexp)
(define-key sp-keymap (kbd "C-M-<left>") 'sp-backward-slurp-sexp)
(define-key sp-keymap (kbd "C-M-<right>") 'sp-backward-barf-sexp)

(define-key sp-keymap (kbd "M-D") 'sp-splice-sexp)

(define-key sp-keymap (kbd "C-]") 'sp-select-next-thing-exchange)
(define-key sp-keymap (kbd "C-<left_bracket>") 'sp-select-previous-thing)
(define-key sp-keymap (kbd "C-M-]") 'sp-select-next-thing)

(define-key sp-keymap (kbd "M-F") 'sp-forward-symbol)
(define-key sp-keymap (kbd "M-B") 'sp-backward-symbol)

(define-key sp-keymap (kbd "H-t") 'sp-prefix-tag-object)
(define-key sp-keymap (kbd "H-p") 'sp-prefix-pair-object)
(define-key sp-keymap (kbd "H-s c") 'sp-convolute-sexp)
(define-key sp-keymap (kbd "H-s a") 'sp-absorb-sexp)
(define-key sp-keymap (kbd "H-s e") 'sp-emit-sexp)
(define-key sp-keymap (kbd "H-s p") 'sp-add-to-previous-sexp)
(define-key sp-keymap (kbd "H-s n") 'sp-add-to-next-sexp)
(define-key sp-keymap (kbd "H-s j") 'sp-join-sexp)
(define-key sp-keymap (kbd "H-s s") 'sp-split-sexp)

;; (setq sp-cancel-autoskip-on-backward-movement nil)
;; (setq sp-autoskip-closing-pair t)
;; (custom-set-variables
;;  '(sp-cancel-autoskip-on-backward-movement nil))

(sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)
;; Remove '' pairing in elisp because quoting is used a ton
(sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)

(sp-with-modes '(html-mode sgml-mode)
  (sp-local-pair "<" ">"))

(sp-with-modes sp--lisp-modes
  (sp-local-pair "(" nil :bind "C-("))

;;; 12_paren.el ends here
