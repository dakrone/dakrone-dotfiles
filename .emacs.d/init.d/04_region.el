;;;; region setting

;; expand region
(global-set-key (kbd "C-@") 'er/expand-region)
(global-set-key (kbd "C-M-@") 'er/contract-region)

;; multimark
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-x r e") 'mc/edit-lines)

(eval-after-load "multiple-cursors-core"
  '(progn
     (dolist (command '(delete-cursor-word-or-region
                        cperl-electric-semi))
       (add-to-list 'mc/cmds-to-run-for-all 'command))))

;; wrap-region
(require 'wrap-region)
(wrap-region-global-mode t)

(wrap-region-add-wrapper "*" "*" nil 'org-mode)
(wrap-region-add-wrapper "_" "_" nil 'org-mode)
(wrap-region-add-wrapper "/" "/" nil 'org-mode)
(wrap-region-add-wrapper "+" "+" nil 'org-mode)

;; disable paredit enable mode
(add-to-list 'wrap-region-except-modes 'emacs-lisp-mode)
(add-to-list 'wrap-region-except-modes 'scheme-mode)
(add-to-list 'wrap-region-except-modes 'lisp-mode)
(add-to-list 'wrap-region-except-modes 'clojure-mode)

;; my own autoinsert implementation with wrap-region
(defun my/wrap-region-trigger (input)
  `(lambda ()
     (interactive)
     (unless (use-region-p)
       (set-mark (point)))
     (let ((last-input-event ,(string-to-char input)))
       (wrap-region-trigger 1 ,input))))

(defun my/wrap-region-as-autopair ()
  (local-set-key (kbd "M-\"") (my/wrap-region-trigger "\""))
  (local-set-key (kbd "M-'")  (my/wrap-region-trigger "'"))
  (local-set-key (kbd "M-(")  (my/wrap-region-trigger "("))
  (local-set-key (kbd "M-[")  (my/wrap-region-trigger "["))
  (local-set-key (kbd "M-{")  (my/wrap-region-trigger "{")))

;; set majar mode derived `prog-mode'
(dolist (hook '(c-mode-hook
                c++-mode-hook
                python-mode-hook
                haskell-mode-hook
                ruby-mode-hook
                coffee-mode-hook
                cperl-mode-hook))
  (add-hook hook 'my/wrap-region-as-autopair))
