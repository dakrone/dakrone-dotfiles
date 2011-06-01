;;; Slimy bits

(eval-after-load 'slime
  '(setq slime-words-of-encouragement
         (append '("I'll take you 'round the you-knee-verse..."
                   "Time to hunt the glorious Piranhamoose."
                   "\"Shut up woman, get on my horse.\"")
                 slime-words-of-encouragement)))

(defun lein-swank ()
  (interactive)
  (let ((root (locate-dominating-file default-directory "project.clj")))
    (or root (error "Not in a Leiningen project."))
    (shell-command (format "cd %s && lein swank %s &" root 4009)
                   "*lein-swank*")
    (set-process-filter (get-buffer-process "*lein-swank*")
                        ;; gAAAAAAAAAAAH! no leixcal scope? what is this, 1970?
                        (lambda (process output)
                          (when (string-match "Connection opened on" output)
                            (condition-case e
                                (slime-connect "localhost" 4009)
                              (error
                               (message "Retrying slime-connect...")
                               (slime-connect "localhost" 4009)))
                            (set-process-filter process nil))))
    (message "Starting swank server...")))

(defalias 'jack-in 'lein-swank)

(defun swank-clojure-dim-font-lock ()
  "Dim irrelevant lines in Clojure debugger buffers."
  (if (string-match "clojure" (buffer-name))
      (font-lock-add-keywords
       nil `((,(concat " [0-9]+: " (regexp-opt '("clojure.core"
                                                 "clojure.lang"
                                                 "swank." "java."))
                       ;; TODO: regexes ending in .* are ignored by
                       ;; font-lock; what gives?
                       "[a-zA-Z0-9\\._$]*")
              . font-lock-comment-face)) t)))

(add-hook 'sldb-mode-hook 'swank-clojure-dim-font-lock)

(ignore-errors
  (load-file (expand-file-name "pcomplete-ssh.el"
                               (file-name-directory
                                (or (buffer-file-name) load-file-name)))))

(setq slime-net-coding-system 'utf-8-unix) ; welcome to the 21st century, slime.

(defvar safe-tunnel-process nil)

(defvar safe-tunnel-port 4008)

(defun safe-remote-slime ()
  (interactive)
  (let ((host (ido-completing-read "Host: " (pcmpl-ssh-hosts))))
    (when (and safe-tunnel-process
               (eq 'run (process-status safe-tunnel-process)))
      (if (y-or-n-p "Tunnel already open; kill?")
          (ignore-errors (kill-process safe-tunnel-process))
        (error "Aborted.")))
    (setq safe-tunnel-process
          (start-process "safe-tunnel" "*safe-tunnel*" "ssh"
                         "-L" (format "%s:127.0.0.1:4005" safe-tunnel-port)
                         ;; TODO: launch swank if needed
                         host "echo connected && sleep 16"))
    (set-process-filter safe-tunnel-process
                        (lambda (process output)
                          (with-current-buffer "*safe-tunnel*"
                            (insert output))
                          (when (string-match "connected" output)
                            (slime-connect "localhost" safe-tunnel-port)
                            (set-process-filter process nil))))))

(ignore-errors
  (load-file (expand-file-name "cdt.el"
                               (file-name-directory
                                (or (buffer-file-name) load-file-name)))))

;;; Magit

(defun add-safe-ticket-number ()
  (when (string-match "\\(sa-\\)?safe/$" default-directory)
    (goto-char (point-max))
    (let* ((branch (magit-get-current-branch))
           (ticket-number (second (split-string branch "-"))))
      (when (and ticket-number (string-match "^[0-9]+$" ticket-number)
                 (= (point-min) (point-max)))
        (insert "[#safe-" ticket-number "] ")))))

(add-hook 'magit-log-edit-mode-hook 'add-safe-ticket-number)

(setq clojure-test-ns-segment-position 1)

;;; Hooks

(defun sonian-format-buffer ()
  "Should be used in a git-hook because calling this function
actually kills emacs. Exits with code 0 if the file is correctly
formatted, 1 if it isn't."
  (indent-region (point-min) (point-max))
  (untabify (point-min) (point-max))
  (delete-trailing-whitespace)
  (if (buffer-modified-p)
      (kill-emacs 1)
    (kill-emacs 0)))

(when (> emacs-major-version 23)
  ;; Emacs 24 needs new whitespace-style settings
  (setq whitespace-style '(face trailing lines-tail tabs)
        whitespace-line-column 80))

(eval-after-load 'clojure-mode
  '(add-hook 'clojure-mode-hook
             (lambda ()
               (font-lock-add-keywords
                'clojure-mode
                '(("(\\(with-[^[:space:]]*\\)" (1 font-lock-keyword-face))
                  ("(\\(def[^[:space:]]*\\)" (1 font-lock-keyword-face)))))))

(provide 'sonian)
