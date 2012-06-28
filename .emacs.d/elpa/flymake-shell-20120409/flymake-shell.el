;;; flymake-shell.el --- A flymake syntax-checker for shell scripts
;;
;;; Copyright (C) 2011 Steve Purcell
;;; Author: Steve Purcell <steve@sanityinc.com>
;;; URL: https://github.com/purcell/flymake-shell
;;; Version: DEV
;;;
;;; Commentary:

;; Usage:

;;   (require 'flymake-shell)
;;   (add-hook 'sh-set-shell-hook 'flymake-shell-load)

(require 'flymake)

;;; Code:

(defconst flymake-shell-supported-shells '(bash zsh sh))

(defconst flymake-shell-err-line-pattern-re
  '(("^\\(.+\\): line \\([0-9]+\\): \\(.+\\)$" 1 2 nil 3) ; bash
    ("^\\(.+\\): ?\\([0-9]+\\): \\(.+\\)$" 1 2 nil 3)) ; zsh / dash
  "Regexp matching shell error messages.")

(defun flymake-shell--create-temp-in-system-tempdir (file-name prefix)
  "Return a temporary file name into which flymake can save buffer contents.

This is tidier than `flymake-create-temp-inplace', and therefore
preferable when the checking doesn't depend on the file's exact
location."
  (make-temp-file (or prefix "flymake-shell") nil ".sh"))

(defun flymake-shell-init ()
  "Construct a command that flymake can use to check shell source."
  (list (format "%s" sh-shell)
        (list "-n" (flymake-init-create-temp-buffer-copy
                    'flymake-shell--create-temp-in-system-tempdir))))

;;;###autoload
(defun flymake-shell-load ()
  "Configure flymake mode to check the current buffer's shell-script syntax.

This function is designed to be called in `sh-set-shell-hook'; it
does not alter flymake's global configuration, so function
`flymake-mode' alone will not suffice."
  (interactive)
  (unless (eq 'sh-mode major-mode)
    (error "Cannot enable flymake-shell in this major mode"))
  (if (memq sh-shell flymake-shell-supported-shells)
      (progn
        ;; We know we're in shell-script mode, so stop flymake
        ;; second-guessing based on filename
        (set (make-local-variable 'flymake-allowed-file-name-masks) '(("." flymake-shell-init)))
        (set (make-local-variable 'flymake-err-line-patterns) flymake-shell-err-line-pattern-re)
        (if (executable-find (symbol-name sh-shell))
            (flymake-mode t)
          (message "Not enabling flymake: '%s' command not found" sh-shell)))
    (message "Shell %s is not supported by flymake-shell" sh-shell)))


(defadvice flymake-post-syntax-check (before flymake-force-check-was-interrupted)
  (setq flymake-check-was-interrupted t))
(ad-activate 'flymake-post-syntax-check)


(provide 'flymake-shell)
;;; flymake-shell.el ends here
