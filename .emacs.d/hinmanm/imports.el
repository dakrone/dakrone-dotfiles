;; all the requires that I normally require

;; ==== Imports ====
;; Undo tree support
(require 'undo-tree)
(global-undo-tree-mode)

;; magic dired
(require 'dired-x)

;; smex
(smex-initialize)

;; dim parens
(require 'parenface)

;; org-mode
(add-to-list 'load-path (concat "~/.emacs.d/" (user-login-name) "/org-mode"))
(require 'org)

;; auto-complete
(require 'popup)
(require 'auto-complete-config)
(ac-config-default)
(define-key ac-mode-map (kbd "M-TAB") 'auto-complete)
(ac-set-trigger-key "TAB")

;; expand-region
(require 'expand-region)
(global-set-key (kbd "M-'") 'er/expand-region)
(global-set-key (kbd "M-,") 'er/contract-region)
(setq pending-delete-mode t)

;; nrepl
(add-to-list 'load-path "~/src/elisp/nrepl.el")
(require 'nrepl)
(add-hook 'nrepl-interaction-mode-hook
          'nrepl-turn-on-eldoc-mode)

;; clojure-mode, work around a weird clojure-test-mode error
(add-to-list 'load-path "~/src/elisp/clojure-mode")
(safe-load "~/src/elisp/clojure-mode/clojure-mode.el" nil t)
(safe-load "~/src/elisp/clojure-mode/clojure-test-mode.el" nil t)
(safe-load "~/src/elisp/clojure-mode/clojurescript-mode.el" nil t)

;; yaml-mode
;;(require 'yaml-mode)
(autoload 'yaml-mode "yaml-mode" "YAML Mode." t)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))

;; javap mode
;;(require 'javap-mode)
(autoload 'javap-mode "javap-mode" "Javap Mode." t)
(add-to-list 'auto-mode-alist '("\\.class$" . javap-mode))

;; ssh-config-mode
(add-to-list 'auto-mode-alist '(".ssh/config$" . ssh-config-mode))

;; Display the battery level in the bottom bar
(display-battery-mode t)

;; helm
(require 'helm-config)
;;(helm-mode 1)
(global-set-key (kbd "C-c h") 'helm-mini)
;;(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)
(global-set-key (kbd "C-x f") 'helm-recentf)

;; gpg stuff
(require 'epa-file)
(epa-file-enable)
