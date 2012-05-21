;;; kibit-mode.el --- Enhance clojure-mode with Kibit analysis

;; Copyright (C) 2012 Alex Redington <http://www.holychao.com>
;; Authors: Alex Redington
;; Created: 2012
;; Version: 0.1
;; Keywords: clojure kibit
;; Package-Requires: ((clojure-mode "1.11.5")
;;                    (mode-compile "2.29"))

;;; Commentary:
;;
;; This file is NOT part of GNU Emacs.
;;
;; Copyright (C) 2012 Alex Redington

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use,
;; copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the
;; Software is furnished to do so, subject to the following
;; conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;; OTHER DEALINGS IN THE SOFTWARE.

;;; Documentation:
;;
;; This minor mode acts as a compilation mechanism for interactively replacing
;; clojure s-expressions by their more idiomatic representations. It provides
;; the following capabilities:
;;
;;  * Run a check over the currently open file (bound to `\C-c \C-n`). This will
;;    open a compilation mode buffer showing the kibit replacements.
;;
;;  * Implement a suggested replacement (bound to `r`). This will destroy the
;;    extant formatting when the replacement is inserted

;;; Dependencies:
;; This minor mode depends on `mode-compile` and `clojure-mode`.

;;; Change Log:
;;
;; 0.1 - First cut of kibit-mode

;;; Code:

(require 'clojure-mode)
(require 'compile)

(defconst kibit-mode-keymap (make-sparse-keymap) "Keymap used in kibit mode")

(define-key kibit-mode-keymap (kbd "C-c C-n") 'kibit-check)

(defgroup kibit-mode nil
  "Kibit minor mode.")

(eval-and-compile
  (defvar kibit-mode-path
    (let ((path (or (locate-library "kibit-mode") load-file-name)))
      (and path (file-name-directory path)))
    "Directory containing the kibit-mode package.
This is used to execute the supporting kibit analysis execution environment.
The default value is automatically computed from the location of the
Emacs Lisp package."))


;;;###autoload
(define-minor-mode kibit-mode
  "Minor mode for kibit compilation support"
  :lighter " kibit"
  :keymap  kibit-mode-keymap)

(defun kibit-check ()
  "Runs the current file through kibit check"
  (interactive)
  (let ((default-directory kibit-mode-path))
    (compile (concat "lein run -m kibit-mode.core "
                     (buffer-file-name)))))

(add-to-list 'compilation-error-regexp-alist-alist
             '(kibit-mode "\\([0-9A-Za-z_./\:-]+\\.clj\\):\\([0-9]+\\):" 1 2))
(add-to-list 'compilation-error-regexp-alist 'kibit-mode)

(require 'flymake)
(defun flymake-kibit-init ()
  (flymake-simple-make-init-impl
   'flymake-create-temp-with-folder-structure nil nil
   buffer-file-name
   'flymake-get-kibit-cmdline))

(defun flymake-get-kibit-cmdline (source base-dir)
  (list (concat kibit-mode-path "bin/kibit-flymake.sh") (list source) kibit-mode-path))

(push '(".+\\.clj$" flymake-kibit-init) flymake-allowed-file-name-masks)
(push '(".+\\.cljs$" flymake-kibit-init) flymake-allowed-file-name-masks)

(push '("\\(.*\\):\\([0-9]+\\): \\(ERROR: .* CORRECTION: .*\\)"
        1 2 nil 3)
      flymake-err-line-patterns)

(provide 'kibit-mode)
;;; kibit-mode.el ends here
