;; python-setting
(defadvice run-python (around run-python-no-sit activate)
  "Suppress absurd sit-for in run-python of python.el"
  (let ((process-launched (or (ad-get-arg 2) ; corresponds to `new`
                              (not (comint-check-proc python-buffer)))))
    (flet ((sit-for (seconds &optional nodisp)
                    (when process-launched
                      (accept-process-output (get-buffer-process python-buffer)))))
      ad-do-it)))

(autoload 'helm-pydoc "helm-pydoc")

(eval-after-load "python"
  '(progn
     ;; auto-complete mode for python
     (require 'jedi)
     (define-key python-mode-map (kbd "C-c C-d") 'jedi:show-doc)

     ;; show-doc
     (setq jedi:tooltip-method nil)
     (set-face-attribute 'jedi:highlight-function-argument nil
                         :foreground "green")

     ;; binding
     (define-key python-mode-map (kbd "C-c C-l") 'jedi:get-in-function-call)
     (define-key python-mode-map (kbd "C-c C-a") 'helm-pydoc)
     (define-key python-mode-map (kbd "C-M-d") 'my/python-next-block)
     (define-key python-mode-map (kbd "C-M-u") 'my/python-up-block)
     (define-key python-mode-map (kbd "C-c C-z") 'run-python)
     (define-key python-mode-map (kbd "<backtab>") 'python-back-indent)))

(defun my/python-mode-hook ()
  (jedi:ac-setup)

  ;; autopair
  (setq autopair-handle-action-fns
        '(autopair-default-handle-action autopair-python-triple-quote-action)))

(add-hook 'python-mode-hook 'my/python-mode-hook)

(defvar my/python-block-regexp
  "\\<\\(for\\|if\\|while\\|try\\|class\\|def\\)\\s-")

(defun my/python-next-block ()
  (interactive)
  (let ((orig (point))
        (new nil))
    (save-excursion
      (forward-char)
      (if (re-search-forward my/python-block-regexp nil t)
          (setq new (point))))
    (when new
      (goto-char new)
      (backward-word))))

(defun my/python-up-block ()
  (interactive)
  (let ((orig (point))
        (new nil))
    (save-excursion
      (backward-char)
      (if (re-search-backward my/python-block-regexp nil t)
          (setq new (point))))
    (when new
      (goto-char new))))

;; back indent
(defun python-back-indent ()
  (interactive)
  (let ((current-pos (point))
        (regexp-str (format " +\\{%d\\}" python-indent)))
   (save-excursion
     (beginning-of-line)
     (when (re-search-forward regexp-str current-pos t)
       (beginning-of-line)
       (delete-char python-indent)))))
