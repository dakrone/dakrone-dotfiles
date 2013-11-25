;; setting for javascript
(defalias 'javascript-generic-mode 'js-mode)
(add-to-list 'auto-mode-alist '("\\.js$" . js-mode))
(setq-default js-auto-indent-flag nil)
