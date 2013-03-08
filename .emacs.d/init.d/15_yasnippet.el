;;;; yasnippet
(require 'yasnippet)
(custom-set-variables
 '(yas/snippet-dirs `(,(concat user-emacs-directory "my_snippets"))))

;; enable yasnippet mode
(dolist (hook '(c-mode-hook
                c++-mode-hook
                cperl-mode-hook
                emacs-lisp-mode-hook
                js-mode-hook
                org-mode-hook
                python-mode-hook
                ruby-mode-hook
                sh-mode-hook
                wl-draft-mode-hook))
  (add-hook hook 'yas/minor-mode-on))

;; helm interface
(eval-after-load "helm-config"
  '(progn
     (defun my-yas/prompt (prompt choices &optional display-fn)
       (let* ((names (loop for choice in choices
                           collect (or (and display-fn (funcall display-fn choice))
                                       coice)))
              (selected (helm-other-buffer
                         `(((name . ,(format "%s" prompt))
                            (candidates . names)
                            (action . (("Insert snippet" . (lambda (arg) arg))))))
                         "*helm yas/prompt*")))
         (if selected
             (let ((n (position selected names :test 'equal)))
               (nth n choices))
           (signal 'quit "user quit!"))))
     (custom-set-variables '(yas/prompt-functions '(my-yas/prompt)))
     (global-set-key (kbd "M-=") 'yas/insert-snippet)))

;; snippet-mode for *.yasnippet files
(add-to-list 'auto-mode-alist '("\\.yasnippet$" . snippet-mode))

;; utility functions
(defun yas/perl-package-name ()
  (let ((file-path (file-name-sans-extension (buffer-file-name))))
    (if (string-match "lib/\\(.+\\)$" file-path)
        (replace-regexp-in-string "/" "::" (match-string 1 file-path))
      (file-name-nondirectory file-path))))
