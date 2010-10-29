;;; es-log-mode.el --- For viewing ElasticSearch logs

;; Copyright (C) 2010 Phil Hagelberg
;;
;; Author: Phil Hagelberg
;; URL:
;; Version: 0.1
;; Keywords: log utility

;; This file is not part of GNU Emacs.

;;; Commentary:

;; cool story bro

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(defvar es-log-font-lock-regex
  (concat "^\\[\\([0-9:,]+\\)\\]"
          "\\[\\([A-Z ]+\\)\\]"
          "\\[\\([a-z\\. ]+\\)\\] "
          "\\[\\(.*?\\)\\]"))

(defvar es-log-mode-hook nil)

(defun es-log-mode ()
  (interactive)
  (setq major-mode 'es-log-mode)
  (setq mode-name "ES-Log")

  (run-mode-hooks 'es-log-mode-hook)

  (font-lock-add-keywords
   ;; TODO: merge these?
   nil `((,es-log-font-lock-regex 1 font-lock-comment-face)
         (,es-log-font-lock-regex 2 font-lock-comment-face)
         (,es-log-font-lock-regex 3 font-lock-builtin-face)
         (,es-log-font-lock-regex 4 font-lock-builtin-face)
         ("\\[\\(........-....-....-....-............\\)\\]"
          1 font-lock-string-face)
         (,(concat "\\[\\([0-2]?[0-9]?[0-9]\\.[0-2]?[0-9]?[0-9]\\."
                   "[0-2]?[0-9]?[0-9]\\.[0-2]?[0-9]?[0-9]\\\\)\\]")
          1 font-lock-preprocessor-face)))

  (font-lock-mode 1)
  (auto-revert-tail-mode 1))

(provide 'es-log-mode) ;;; es-log.el ends here
