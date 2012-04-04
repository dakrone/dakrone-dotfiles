
;;; flymake-css.el --- Flymake support for css using csslint
;;
;;; Author: Steve Purcell <steve@sanityinc.com>
;;; Homepage: https://github.com/purcell/flymake-css
;;; Version: 0.2
;;
;;; Commentary:
;;
;; Usage: (add-hook 'css-mode-hook 'flymake-css-load)
;;
;; Beware that csslint is quite slow, so there can be a significant lag
;; between editing and the highlighting of resulting errors.
;;
;; Like the author's many other flymake-*.el extensions, this code is
;; designed to configure flymake in a buffer-local fashion, which
;; avoids the dual pitfalls of 1) inflating the global list of
;; `flymake-err-line-patterns' and 2) being required to specify the
;; matching filename extensions (e.g. "*.css") redundantly.
;;
;; Based mainly on the author's flymake-jslint.el, and using the
;; error regex from Arne Jørgensen's similar flymake-csslint.el.
(require 'flymake)

;;; Code:

(defgroup flymake-css nil
  "Flymake checking of CSS using csslint"
  :group 'programming
  :prefix "flymake-css-")

;;;###autoload
(defcustom flymake-css-lint-command "csslint"
  "Name (and optionally full path) of csslint executable."
  :type 'string :group 'flymake-css)

(defvar flymake-css-err-line-patterns
  '(("^\\(.*\\): line \\([[:digit:]]+\\), col \\([[:digit:]]+\\), \\(.+\\)$" 1 2 3 4)))

(defun flymake-css--create-temp-in-system-tempdir (file-name prefix)
  "Return a temporary file name into which flymake can save buffer contents.

This is tidier than `flymake-create-temp-inplace', and therefore
preferable when the checking doesn't depend on the file's exact
location."
  (make-temp-file (or prefix "flymake-css") nil ".css"))

(defun flymake-css-init ()
  "Construct a command that flymake can use to check css source."
  (list flymake-css-lint-command
        (list "--format=compact"
              (flymake-init-create-temp-buffer-copy
               'flymake-css--create-temp-in-system-tempdir))))


;;;###autoload
(defun flymake-css-load ()
  "Configure flymake mode to check the current buffer's css syntax.

This function is designed to be called in `css-mode-hook' or
equivalent; it does not alter flymake's global configuration, so
function `flymake-mode' alone will not suffice."
  (interactive)
  (set (make-local-variable 'flymake-allowed-file-name-masks) '(("." flymake-css-init)))
  (set (make-local-variable 'flymake-err-line-patterns) flymake-css-err-line-patterns)
  (if (executable-find flymake-css-lint-command)
      (flymake-mode t)
    (message "Not enabling flymake: csslint command not found")))


(defadvice flymake-post-syntax-check (before flymake-force-check-was-interrupted)
  (setq flymake-check-was-interrupted t))
(ad-activate 'flymake-post-syntax-check)


(provide 'flymake-css)
;;; flymake-css.el ends here
