;;; color-theme-dpaste.el --- Dpaste color theme for GNU Emacs.

;; Copyright (C) 2010 Yesudeep Mangalapilly <yesudeep@gmail.com>

;; Author: Yesudeep Mangalapilly
;; Keywords: dpaste color theme emacs
;; URL: http://github.com/gorakhargosh/color-themes-collection
;; Version: 0.0.1
;; Package-Requires: ((color-theme "6.6.1"))

;; This file is NOT a part of GNU Emacs.

;;; License:

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; Code:

(eval-when-compile
  (require 'color-theme))

(defun color-theme-dpaste ()
  "Dpaste color theme for GNU Emacs."
  (interactive)
  (color-theme-install
   '(color-theme-dpaste
      ((background-color . "#ffffff")
      (background-mode . light)
      (border-color . "#969696")
      (cursor-color . "#000000")
      (foreground-color . "#000000")
      (mouse-color . "black"))
     (fringe ((t (:background "#969696"))))
     (mode-line ((t (:foreground "#dedede" :background "#a1a1a1"))))
     (region ((t (:background "#fcf9d9"))))
     (font-lock-builtin-face ((t (:foreground "#858585"))))
     (font-lock-comment-face ((t (:foreground "#a1a1a1"))))
     (font-lock-function-name-face ((t (:foreground "#a72035"))))
     (font-lock-keyword-face ((t (:foreground "#000000"))))
     (font-lock-string-face ((t (:foreground "#c78129"))))
     (font-lock-type-face ((t (:foreground"#154b99"))))
     (font-lock-variable-name-face ((t (:foreground "#000000"))))
     (minibuffer-prompt ((t (:foreground "#2a7442" :bold t))))
     (font-lock-warning-face ((t (:foreground "Red" :bold t))))
     )))


(color-theme-dpaste)

(provide 'color-theme-dpaste)

;;; color-theme-dpaste.el ends here
