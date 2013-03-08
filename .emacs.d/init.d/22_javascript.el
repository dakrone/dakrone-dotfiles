;; setting for javascript
(add-to-list 'auto-mode-alist '("\\.js$" . js-mode))
(setq-default js-auto-indent-flag nil)
(add-hook 'js-mode-hook 'flymake-jslint-load)

;; coffeescript
(defun my/coffee-mode-hook ()
  (setq coffee-tab-width 2)
  (local-unset-key (kbd "C-m")))

(add-hook 'coffee-mode-hook 'my/coffee-mode-hook)
