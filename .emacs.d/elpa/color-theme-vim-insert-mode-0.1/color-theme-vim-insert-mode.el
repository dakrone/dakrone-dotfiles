;;; color-theme-vim-insert-mode.el --- Color theme VIM insert mode
;; 
;; Filename: color-theme-vim-insert-mode.el
;; Description: Color theme VIM insert mode
;; Author: Matthew L. Fidler
;; Maintainer: Matthew L. Fidler
;; Created: Tue Feb  8 09:43:14 2011 (-0600)
;; Version: 0.1
;; Last-Updated: Tue Feb  8 10:36:36 2011 (-0600)
;;           By: Matthew L. Fidler
;;     Update #: 3 
;; URL:
;; Keywords: 
;; Compatibility: 
;; 
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;;  This is the VIM insert mode used in key-choices.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change Log:
;; 08-Feb-2011    Matthew L. Fidler  
;;    Added commentary for ELPA
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:


(defun color-theme-vim-insert-mode ()
  "Color theme by Michael Soulier, created 2003-03-26."
  (interactive)
  (color-theme-install
   '(color-theme-vim-colors
     (;(background-color . "#ffffff")
      (background-color . "#e2e2e2")
      (background-mode . light)
      (border-color . "black")
      (cursor-color . "#000000")
      (foreground-color . "#000000")
      (mouse-color . "#000000"))
     ((Man-overstrike-face . bold)
      (Man-underline-face . underline)
      (apropos-keybinding-face . underline)
      (apropos-label-face . italic)
      (apropos-match-face . secondary-selection)
      (apropos-property-face . bold-italic)
      (apropos-symbol-face . bold)
      (cperl-here-face . font-lock-string-face)
      (cperl-invalid-face quote underline)
      (cperl-pod-face . font-lock-comment-face)
      (cperl-pod-head-face . font-lock-variable-name-face)
      (help-highlight-face . underline)
      (ispell-highlight-face . highlight)
      (list-matching-lines-face . bold)
      (rpm-spec-dir-face . rpm-spec-dir-face)
      (rpm-spec-doc-face . rpm-spec-doc-face)
      (rpm-spec-ghost-face . rpm-spec-ghost-face)
      (rpm-spec-macro-face . rpm-spec-macro-face)
      (rpm-spec-package-face . rpm-spec-package-face)
      (rpm-spec-tag-face . rpm-spec-tag-face)
      (tags-tag-face . default)
      (view-highlight-face . highlight)
      (widget-mouse-face . highlight))
     (default ((t (:background "#ffffff" :foreground "#000000"))))
     (Info-title-1-face ((t (nil))))
     (Info-title-2-face ((t (nil))))
     (Info-title-3-face ((t (nil))))
     (Info-title-4-face ((t (:bold (bold extra-bold ultra-bold)))))
     (bold ((t (:bold (bold extra-bold ultra-bold)))))
     (bold-italic ((t (:italic (italic oblique) :bold (bold extra-bold ultra-bold)))))
     (border ((t (:background "black"))))
     (comint-highlight-input ((t (:bold (bold extra-bold ultra-bold)))))
     (comint-highlight-prompt ((t (:foreground "dark blue"))))
     (cperl-array-face ((t (:foreground "brown"))))
     (cperl-hash-face ((t (:foreground "red"))))
     (cperl-nonoverridable-face ((t (:foreground "#008b8b"))))
     (cursor ((t (:background "#000000"))))
     (fixed-pitch ((t (nil))))
     (font-lock-builtin-face ((t (:foreground "purple"))))
     (font-lock-comment-face ((t (:foreground "blue"))))
     (font-lock-constant-face ((t (:foreground "green4"))))
     (font-lock-doc-face ((t (:background "#ffffff"))))
     (font-lock-function-name-face ((t (:foreground "#008b8b"))))
     (font-lock-keyword-face ((t (:bold (bold extra-bold ultra-bold) :foreground "#a52a2a"))))
     (font-lock-string-face ((t (:background "#ffffff" :foreground "#ff00ff"))))
     (font-lock-type-face ((t (:foreground "ForestGreen"))))
     (font-lock-variable-name-face ((t (:foreground "#008b8b"))))
     (font-lock-warning-face ((t (:bold (bold extra-bold ultra-bold) :foreground "Red"))))
     (fringe ((t (:background "#e5e5e5"))))
     (header-line ((t (:background "grey90" :foreground "grey20"))))
     (highlight ((t (:background "darkseagreen2"))))
     (info-header-node ((t (nil))))
     (info-header-xref ((t (nil))))
     (info-menu-5 ((t (:foreground "red1"))))
     (info-menu-header ((t (:bold (bold extra-bold ultra-bold)))))
     (info-node ((t (:italic (italic oblique) :bold (bold extra-bold ultra-bold) :foreground "brown"))))
     (info-xref ((t (:bold (bold extra-bold ultra-bold) :foreground "magenta4"))))
     (isearch ((t (:background "magenta4" :foreground "lightskyblue1"))))
     (isearch-lazy-highlight-face ((t (:background "paleturquoise"))))
     (italic ((t (:italic (italic oblique)))))
     (menu ((t (nil))))
     (mode-line ((t (:background "grey75" :foreground "black"))))
     (mouse ((t (:background "#000000"))))
     (region ((t (:background "lightgoldenrod2"))))
     (rpm-spec-dir-face ((t (:foreground "green"))))
     (rpm-spec-doc-face ((t (:foreground "magenta"))))
     (rpm-spec-ghost-face ((t (:foreground "red"))))
     (rpm-spec-macro-face ((t (:foreground "purple"))))
     (rpm-spec-package-face ((t (:foreground "red"))))
     (rpm-spec-tag-face ((t (:foreground "blue"))))
     (scroll-bar ((t (:background "grey75" :foreground "#000000"))))
     (secondary-selection ((t (:background "yellow"))))
     (sh-heredoc-face ((t (:foreground "tan"))))
     (show-paren-match-face ((t (:background "turquoise"))))
     (show-paren-mismatch-face ((t (:background "purple" :foreground "white"))))
     (tool-bar ((t (:background "grey75" :foreground "black"))))
     (tooltip ((t (:background "lightyellow" :foreground "black"))))
     (trailing-whitespace ((t (:background "red"))))
     (underline ((t (:underline t))))
     (variable-pitch ((t (nil))))
     (widget-button-face ((t (:bold (bold extra-bold ultra-bold)))))
     (widget-button-pressed-face ((t (:foreground "red"))))
     (widget-documentation-face ((t (:foreground "dark green"))))
     (widget-field-face ((t (:background "gray85"))))
     (widget-inactive-face ((t (:foreground "dim gray"))))
     (widget-single-line-field-face ((t (:background "gray85")))))))
(provide 'color-theme-vim-insert-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; color-theme-vim-insert-mode.el ends here
