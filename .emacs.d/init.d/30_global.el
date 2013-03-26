;;;; GNU gtags
(add-hook 'helm-gtags-mode-hook
          '(lambda ()
             (local-set-key (kbd "M-t") 'helm-gtags-find-tag)
             (local-set-key (kbd "M-r") 'helm-gtags-find-rtag)
             (local-set-key (kbd "M-s") 'helm-gtags-find-symbol)
             (local-set-key (kbd "C-t") 'helm-gtags-pop-stack)))

;;; Enable helm-gtags-mode
(dolist (hook '(c-mode-common-hook
                c++-mode-hook
                java-mode-hook
                asm-mode-hook))
  (add-hook hook 'helm-gtags-mode))
