;; Miscellaneous things that don't fit elsewhere
(defadvice package-compute-transaction
    (before
     package-compute-transaction-reverse (package-list requirements)
     activate compile)
  "reverse the requirements"
  (setq requirements (reverse requirements))
  (print requirements))

(defadvice package-download-tar
    (after package-download-tar-initialize activate compile)
  "initialize the package after compilation"
  (package-initialize))

(defadvice package-download-single
    (after package-download-single-initialize activate compile)
  "initialize the package after compilation"
  (package-initialize))

;; This code makes % act like the buffer name in the minibuffer, similar to Vim
(define-key minibuffer-local-map "%"
  (function
   (lambda ()
     (interactive)
     (insert (file-name-nondirectory
              (buffer-file-name
               (window-buffer (minibuffer-selected-window))))))))

;; Treat clipboard input as UTF-8 string first; compound text next, etc.
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;; Transparency
(when (eq window-system 'ns)
  (set-frame-parameter (selected-frame) 'alpha '(100 100))
  (add-to-list 'default-frame-alist '(alpha 100 100)))

;; split the way I want
(setq split-height-threshold nil)
(setq split-width-threshold 180)

;; always turn whitespace mode on
(whitespace-mode t)
(add-hook 'prog-mode-hook (lambda ()
                            (whitespace-mode t)
                            (subword-mode t)
                            (idle-highlight-mode t)))
;; Except turn it off for shell scripts, because yea, not much you can do with
;; long data
(add-hook 'sh-mode-hook (lambda ()
                          (setq whitespace-line-column 180)))
(add-hook 'java-mode-hook (lambda ()
                            (setq whitespace-line-column 180)))


;; ==== Fix ssh-agent ====
(defun find-agent ()
  (first (split-string
          (shell-command-to-string
           (concat
            "ls -t1 "
            "$(find /tmp/ -uid $UID -path \\*ssh\\* -type s 2> /dev/null)"
            "|"
            "head -1")))))

(defun fix-agent ()
  (interactive)
  (let ((agent (find-agent)))
    (setenv "SSH_AUTH_SOCK" agent)
    (message agent)))



;; ==== JSON formatting ====
(defun beautify-json ()
  (interactive)
  (let ((b (if mark-active (min (point) (mark)) (point-min)))
        (e (if mark-active (max (point) (mark)) (point-max))))
    (shell-command-on-region b e
                             "python -mjson.tool" (current-buffer) t)))

(global-set-key (kbd "C-c C-f") 'beautify-json)

;; ==== Scpaste stuff ====
(setq scpaste-http-destination "http://p.writequit.org")
(setq scpaste-scp-destination "p.writequit.org:~/public_html/wq/paste/")

;; ==== copy-paste on Mac ====
(defun copy-from-osx ()
  (shell-command-to-string "/usr/bin/pbpaste"))

(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "/usr/bin/pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(when (macosx-p)
  (setq interprogram-cut-function 'paste-to-osx)
  (setq interprogram-paste-function 'copy-from-osx))

;; ==== path env stuff ====
(defun add-to-path (path-element)
  "Add the specified path element to the Emacs PATH"
  (interactive "DEnter directory to be added to path: ")
  (if (file-directory-p path-element)
      (setenv "PATH"
              (concat (expand-file-name path-element)
                      path-separator (getenv "PATH")))))

(add-to-path (concat "~/bin"))
(add-to-path "/usr/bin")
(add-to-path "/usr/local/bin")
(add-to-path "/usr/local/MacGPG2/bin")

(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (interactive)
  (indent-buffer)
  (untabify-buffer)
  (delete-trailing-whitespace))

;; Make rgrep actually work
;;(setq ido-ubiquitous-enabled nil)
(require 'ido-ubiquitous)
(require 'ido-vertical-mode)
(ido-vertical-mode t)
;; Require a newline at the end
(setq require-final-newline t)
