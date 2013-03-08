;;; slime
(autoload 'slime "slime" nil t)
(eval-after-load "slime"
  '(progn
     ;; SLIME REPL
     (slime-setup '(slime-repl slime-fancy slime-banner slime-presentations))

     ;; encoding
     (setq slime-net-coding-system 'utf-8-unix)

     ;; for clojure
     (setq slime-protocol-version 'ignore)

     (defun my/slime-mode-hook ()
       (define-key slime-mode-map (kbd "C-M-i") 'auto-complete)
       (define-key slime-mode-map (kbd "C-c C-d C-l") 'helm-hyperspec))
     (add-hook 'slime-mode-hook 'my/slime-mode-hook)

     ;; face
     (set-face-foreground 'slime-repl-inputed-output-face "pink1")

      ;;;; ac-slime
     (require 'ac-slime)
     (add-hook 'slime-mode-hook 'set-up-slime-ac)
     (add-hook 'slime-repl-mode-hook 'set-up-slime-ac)))

(eval-after-load "slime-repl"
  '(progn
     (define-key slime-repl-mode-map (kbd "TAB") nil)
     (define-key slime-repl-mode-map (kbd "C-M-i") 'auto-complete)))

(dolist (hook '(lisp-mode-hook))
  (add-hook hook 'slime-mode))
