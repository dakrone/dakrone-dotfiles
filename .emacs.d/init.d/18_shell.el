;; shell scripts and eshell stuff

(defun my/eshell-mode-hook ()
  (define-key eshell-mode-map (kbd "M-r") 'helm-eshell-history))
(add-hook 'eshell-mode-hook 'my/eshell-mode-hook)

;; Stuff for editing shell scripts
(add-hook 'sh-mode-hook
          (lambda ()
            ;; Turn off show-paren-mode (it freezes up)
            (show-paren-mode -1)
            (flycheck-mode -1)
            (setq blink-matching-paren nil)
            (define-key sh-mode-map (kbd "C-c C-n") 'flycheck-tip-cycle)))
