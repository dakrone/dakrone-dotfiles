;; python-setting
(autoload 'python-mode "python" nil t)
(defadvice run-python (around run-python-no-sit activate)
  "Suppress absurd sit-for in run-python of python.el"
  (let ((process-launched (or (ad-get-arg 2) ; corresponds to `new`
                              (not (comint-check-proc python-buffer)))))
    (flet ((sit-for (seconds &optional nodisp)
                    (when process-launched
                      (accept-process-output (get-buffer-process python-buffer)))))
      ad-do-it)))

(eval-after-load "python"
  '(progn
     ;; auto-complete mode for python
     (require 'jedi)
     (define-key python-mode-map (kbd "C-c C-d") 'jedi:show-doc)

     ;; show-doc
     (setq jedi:tooltip-method 'nil)
     (set-face-attribute 'jedi:highlight-function-argument nil
                         :foreground "green")

     ;; to speedup, just load it on demand
     (autoload 'pylookup-lookup "pylookup"
       "Lookup SEARCH-TERM in the Python HTML indexes." t)
     (autoload 'pylookup-update "pylookup"
       "Run pylookup-update and create the database at `pylookup-db-file'." t)

     (define-key python-mode-map (kbd "C-c C-l") 'pylookup-lookup)

     ;; binding
     (define-key python-mode-map (kbd "C-c C-a") 'my/python-help)
     (define-key python-mode-map (kbd "C-c C-i") 'my/python-insert-import-statement)
     (define-key python-mode-map (kbd "C-M-d") 'my/python-next-block)
     (define-key python-mode-map (kbd "C-M-u") 'my/python-up-block)
     (define-key python-mode-map (kbd "C-c C-z") 'run-python)
     (define-key python-mode-map (kbd "<backtab>") 'python-back-indent)))

(add-hook 'python-mode-hook 'jedi:ac-setup)

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

;; Insert import statement
(defun my/python-search-duplicate-import (module)
  (save-excursion
    (let ((regexp (format "^\\(from\\|import\\)\\s-+%s" module)))
      (re-search-backward regexp nil t))))

(defun my/python-insert-import-statement ()
  (interactive)
  (save-excursion
    (skip-chars-backward "^ \n")
    (let ((exp (thing-at-point 'symbol))
          (module nil))
      (when (and exp (string-match "^\\([^.]+\\)" exp))
        (setq module (match-string 1 exp)))
      (if (re-search-backward "^\\(import\\|from\\)\\s-+" nil t)
          (forward-line 1)
        (progn
          (goto-char (point-min))
          (loop while (string-match "^#" (thing-at-point 'line))
                do
                (forward-line 1))))
      (let* ((prompt (if current-prefix-arg
                         "Import from module: "
                       "import module: "))
             (imported (read-string prompt module nil module)))
        (when (my/python-search-duplicate-import imported)
          (error (format "'%s' is already imported" imported)))
        (insert (format "%s %s"
                        (if current-prefix-arg "from" "import")
                        imported))))))

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

;; help utilities
(defun my/python-help ()
  (interactive)
  (let ((module (read-string "Pydoc module: " ))
        (buf (get-buffer-create "*Python Help*")))
    (with-current-buffer (get-buffer-create buf)
      (erase-buffer)
      (call-process-shell-command (format "pydoc %s" module) nil t t)
      (goto-char (point-min)))
    (pop-to-buffer buf)))
