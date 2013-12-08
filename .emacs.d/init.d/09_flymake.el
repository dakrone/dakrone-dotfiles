;; setting for flycheck

;; ;; flycheck faces
;; (eval-after-load "flycheck"
;;   '(progn
;;      (set-face-attribute 'flycheck-error nil
;;                          :foreground "yellow"
;;                          ;;:weight 'bold
;;                          :underline t
;;                          :background "red"
;;                          )
;;      (set-face-attribute 'flycheck-warning nil
;;                          ;;:foreground "white"
;;                          ;;:weight 'bold
;;                          :underline t
;;                          ;;:background "dark orange"
;;                          )
;;      ))

(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)

(add-hook 'flycheck-mode-hook
          (lambda ()
            (require 'flycheck-tip)
            (global-set-key (kbd "C-c C-n") 'flycheck-tip-cycle)
            (diminish 'flycheck-mode "fc")
            ;; Require google-this
            (require 'google-this)
            ;; Overwrite google URL to not use google
            (defun google-url ()
              (concat "https://duckduckgo.com/?q=%s"))
            ))
