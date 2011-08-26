;;; color-theme-sanityinc-solarized.el --- A version of Ethan Schoonover's Solarized theme

;; Copyright (C) 2011 Steve Purcell

;; Author: Steve Purcell <steve@sanityinc.com>
;; Keywords: themes
;; X-URL: http://github.com/purcell/color-theme-sanityinc-solarized
;; URL: http://github.com/purcell/color-theme-sanityinc-solarized
;; Version: 1.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Here are two slightly subdued color themes that are easy on the eyes
;; and cover a reasonably complete set of faces.
;;
;; Use:
;;
;;   M-x color-theme-solarized-light
;;   M-x color-theme-solarized-dark
;;
;;; Credit:
;;
;; Genius colour selection by Ethan Schoonover:
;; http://ethanschoonover.com/solarized
;; Some faces borrowed from Greg Pfeil's emacs theme:
;; https://github.com/sellout/solarized/blob/master/emacs-color-theme-solarized/color-theme-solarized.el
;;
;;; Code:

;; requires
(require 'color-theme)


;;;###autoload
(defun color-theme-sanityinc-solarized (mode)
  (interactive "Sdark or light")
  ;; These are the Generic RGB equivalents of the "official" sRGB hex values
  (let* ((base03  "#042028")            ; (0.0159 0.1265 0.1597)
         (base02  "#0a2832")            ; (0.0394 0.1601 0.1983)
         (base01  "#465a61")            ; (0.2767 0.3567 0.3830)
         (base00  "#52676f")            ; (0.3244 0.4072 0.4385)
         (base0   "#708183")            ; (0.4406 0.5096 0.5169)
         (base1   "#81908f")            ; (0.5060 0.5649 0.5636)
         (base2   "#e9e2cb")            ; (0.9161 0.8900 0.7978)
         (base3   "#fcf4dc")            ; (0.9894 0.9579 0.8641)
         (yellow  "#a57705")            ; (0.6475 0.4675 0.0235)
         (orange  "#bd3612")            ; (0.7418 0.2133 0.0735)
         (red     "#c60007")            ; (0.7770 0.0000 0.0290)
         (magenta "#c61b6e")            ; (0.7774 0.1080 0.4352)
         (violet  "#5859b7")            ; (0.3479 0.3514 0.7179)
         (blue    "#2075c7")            ; (0.1275 0.4627 0.7823)
         (cyan    "#259185")            ; (0.1468 0.5708 0.5250)
         (green   "#728a05")            ; (0.4498 0.5412 0.0202)
         (foregrounds (list base1 base0 base00 base01))
         (backgrounds (list base03 base02))
         (contrast-backgrounds (list base3 base2)))
    (when (eq 'light mode)
      (rotatef backgrounds contrast-backgrounds)
      (setq foregrounds (reverse foregrounds)))
    (let ((background (first backgrounds))
          (alt-background (second backgrounds))
          (strong (first foregrounds))
          (normal (second foregrounds))
          (faint (third foregrounds))
          (faintest (fourth foregrounds))
          (contrast-background (second contrast-backgrounds)))
      (color-theme-install
       `(,(intern (concat "color-theme-sanityinc-solarized-" (symbol-name mode)))
         ((background-color . ,background)
          (background-mode . light)
          (border-color . ,normal)
          (cursor-color . ,magenta)
          (foreground-color . ,normal)
          (mouse-color . ,cyan))

         ;; Standard font lock faces
         (default ((t (nil))))
         (bold ((t (:weight bold))))
         (bold-italic ((t (:slant italic :weight bold))))
         (underline ((t (:underline t))))
         (italic ((t (:slant italic))))
         (font-lock-builtin-face ((t (:foreground ,violet))))
         (font-lock-comment-delimiter-face ((t (:foreground ,faintest :slant italic))))
         (font-lock-comment-face ((t (:foreground ,faint :slant italic))))
         (font-lock-constant-face ((t (:foreground ,violet))))
         (font-lock-doc-face ((t (:foreground ,magenta))))
         (font-lock-doc-string-face ((t (:foreground ,yellow))))
         (font-lock-function-name-face ((t (:foreground ,blue))))
         (font-lock-keyword-face ((t (:foreground ,green :weight bold))))
         (font-lock-negation-char-face ((t (:foreground ,green))))
         (font-lock-preprocessor-face ((t (:foreground ,magenta))))
         (font-lock-regexp-grouping-backslash ((t (:foreground ,magenta))))
         (font-lock-regexp-grouping-construct ((t (:foreground ,violet))))
         (font-lock-string-face ((t (:foreground ,cyan))))
         (font-lock-type-face ((t (:foreground ,yellow))))
         (font-lock-variable-name-face ((t (:foreground ,yellow))))
         (font-lock-warning-face ((t (:weight bold :foreground ,red))))
         (shadow ((t (:foreground ,(fourth foregrounds)))))
         (success ((t (:foreground ,green))))
         (error ((t (:foreground ,red))))

         ;; Flymake
         (flymake-warnline ((t (:underline ,orange :background ,background))))
         (flymake-errline ((t (:underline ,red :background ,background))))

         ;; Clojure errors
         (clojure-test-failure-face ((t (:background nil :inherit flymake-warnline))))
         (clojure-test-error-face ((t (:background nil :inherit flymake-errline))))
         (clojure-test-success-face ((t (:background nil :foreground nil :underline ,green))))

         ;; For Brian Carper's extended clojure syntax table
         (clojure-keyword ((t (:foreground ,yellow))))
         (clojure-parens ((t (:foreground ,strong))))
         (clojure-braces ((t (:foreground ,green))))
         (clojure-brackets ((t (:foreground ,yellow))))
         (clojure-double-quote ((t (:foreground ,cyan :background nil))))
         (clojure-special ((t (:foreground ,blue))))
         (clojure-java-call ((t (:foreground ,magenta))))

         ;; MMM-mode
         (mmm-code-submode-face ((t (:background ,alt-background))))
         (mmm-comment-submode-face ((t (:inherit font-lock-comment-face))))
         (mmm-output-submode-face ((t (:background ,alt-background))))

         ;; Search
         (match ((t (:foreground ,blue :background ,background :inverse-video t))))
         (isearch ((t (:foreground ,yellow :background ,background :inverse-video t))))
         (isearch-lazy-highlight-face ((t (:foreground ,cyan :background ,background :inverse-video t))))
         (isearch-fail ((t (:background ,background :inherit font-lock-warning-face :inverse-video t))))

         ;; IDO
         (ido-subdir ((t (:foreground ,magenta))))
         (ido-first-match ((t (:foreground ,yellow))))
         (ido-only-match ((t (:foreground ,green))))

         ;; Emacs interface
         (fringe ((t (:background ,alt-background))))
         (border ((t (:background ,alt-background))))
         (border-glyph ((t (nil))))
         (highlight ((t (:inverse-video nil :background ,alt-background))))
         (gui-element ((t (:background ,alt-background :foreground ,normal))))
         (mode-line ((t (:foreground nil :background ,alt-background :weight bold
                                     :box (:line-width 1 :color ,normal)))))
         (mode-line-buffer-id ((t (:foreground ,magenta :background nil))))
         (mode-line-inactive ((t (:inherit mode-line
                                           :foreground ,faintest
                                           :background ,alt-background :weight normal
                                           :box (:line-width 1 :color ,normal)))))
         (mode-line-emphasis ((t (:foreground ,strong))))
         (mode-line-highlight ((t (:foreground ,magenta :box nil :weight bold))))
         (minibuffer-prompt ((t (:foreground ,blue))))
         (region ((t (:background ,contrast-background))))
         (secondary-selection ((t (:background ,alt-background))))

         (header-line ((t (:background nil :foreground ,strong :inherit nil))))
         (trailing-whitespace ((t (:inherit font-lock-warning-face :foreground nil :background nil :inverse-video t))))

         ;; Parenthesis matching (built-in)
         (show-paren-match ((t (:background nil :foreground nil :inverse-video t))))
         (show-paren-mismatch ((t (:background ,magenta :foreground ,background))))

         ;; Parenthesis matching (mic-paren)
         (paren-face-match ((t (:foreground nil :background nil :inherit show-paren-match))))
         (paren-face-mismatch ((t (:foreground nil :background nil :inherit show-paren-mismatch))))
         (paren-face-no-match ((t (:foreground nil :background nil :inherit show-paren-mismatch))))

         (sh-heredoc ((t (:foreground nil :inherit font-lock-string-face))))
         (slime-highlight-edits-face ((t (:foreground ,strong))))
         (slime-repl-input-face ((t (:weight bold))))
         (slime-repl-prompt-face ((t (:underline t :weight bold))))
         (slime-repl-output-face ((t (:background ,background))))

         (diff-added ((t (:foreground ,green))))
         (diff-changed ((t (:foreground ,violet))))
         (diff-removed ((t (:foreground ,orange))))
         (diff-header ((t (:foreground ,cyan :background nil))))
         (diff-file-header ((t (:foreground ,blue :background nil))))
         (diff-hunk-header ((t (:foreground ,magenta))))

         ;; dired+
         (diredp-dir-heading ((t (:foreground nil :background nil :inherit heading))))
         (diredp-dir-priv ((t (:foreground ,cyan :background nil))))
         (diredp-exec-priv ((t (:foreground ,blue :background nil))))
         (diredp-file-name ((t (:foreground ,yellow))))
         (diredp-file-suffix ((t (:foreground ,green))))
         (diredp-flag-mark-line ((t (:background nil :inherit highlight))))
         (diredp-ignored-file-name ((t (:foreground ,faintest))))
         (diredp-link-priv ((t (:background nil :foreground ,violet))))
         (diredp-no-priv ((t (:background nil))))
         (diredp-number ((t (:foreground ,yellow))))
         (diredp-other-priv ((t (:background nil :foreground ,magenta))))
         (diredp-rare-priv ((t (:foreground ,red :background nil))))
         (diredp-read-priv ((t (:foreground ,green :background nil))))
         (diredp-symlink ((t (:foreground ,violet))))
         (diredp-write-priv ((t (:foreground ,yellow :background nil))))

         ;; Magit (a patch is pending in magit to make these standard upstream)
         (magit-item-highlight ((t (:inherit highlight :background nil))))
         (magit-diff-add ((t (:inherit diff-added :foreground nil))))
         (magit-diff-changed ((t (:inherit diff-changed :foreground nil))))
         (magit-diff-del ((t (:inherit diff-removed :foreground nil))))
         (magit-log-graph ((t (:foreground ,faintest))))

         (link ((t (:foreground nil :underline t))))
         (widget-button ((t (:underline t))))
         (widget-field ((t (:background ,alt-background :box (:line-width 1 :color ,normal)))))

         ;; Stop outline-3 from inheriting font-lock-keyword-face, which we've made bold
         (outline-3 ((t (:inherit nil :foreground ,green))))

         (org-link ((t (:foreground ,blue :underline t))))
         (org-date ((t (:foreground ,blue :underline t))))
         (org-agenda-structure ((t (:foreground ,magenta))))
         (org-agenda-date ((t (:foreground ,blue :underline nil))))
         (org-done ((t (:foreground ,green))))
         (org-todo ((t (:foreground ,red))))
         (org-special-keyword ((t (:foreground ,orange))))

         (org-document-title ((t (:foreground ,cyan))))
         (org-column ((t (:background ,alt-background))))
         (org-warning ((t (:weight bold :foreground ,red))))

         (org-scheduled-previously ((t (:foreground ,orange))))

         (markdown-header-face ((t (:inherit header-line))))
         (markdown-url-face ((t (:inherit link))))
         (markdown-link-face ((t (:foreground ,blue :underline t))))

         (hl-sexp-face ((t (:background ,alt-background))))
         (highlight-80+ ((t (:background ,alt-background))))

         ;; Python-specific overrides
         (py-builtins-face ((t (:foreground ,orange :weight normal))))

         ;; Message-mode
         (message-header-other ((t (:inherit header-line :foreground nil :background nil :weight normal))))
         (message-header-subject ((t (:inherit message-header-other :weight bold :foreground ,yellow))))
         (message-header-to ((t (:inherit message-header-other :weight bold :foreground ,orange))))
         (message-header-cc ((t (:inherit message-header-to :foreground nil))))
         (message-header-name ((t (:inherit header-line :foreground ,green :background nil))))
         (message-header-newsgroups ((t (:foreground ,cyan :background nil :slant normal))))
         (message-separator ((t (:foreground ,magenta))))


         ;; Gnus
         (gnus-cite-1 ((t (:inherit outline-1 :foreground nil))))
         (gnus-cite-2 ((t (:inherit outline-2 :foreground nil))))
         (gnus-cite-3 ((t (:inherit outline-3 :foreground nil))))
         (gnus-cite-4 ((t (:inherit outline-4 :foreground nil))))
         (gnus-cite-5 ((t (:inherit outline-5 :foreground nil))))
         (gnus-cite-6 ((t (:inherit outline-6 :foreground nil))))
         (gnus-cite-7 ((t (:inherit outline-7 :foreground nil))))
         (gnus-cite-8 ((t (:inherit outline-8 :foreground nil))))
         ;; there are several more -cite- faces...
         (gnus-header-content ((t (:inherit header-line :foreground nil :background nil :weight normal))))
         (gnus-header-subject ((t (:inherit gnus-header-content :weight bold :foreground ,yellow))))
         (gnus-header-from ((t (:inherit gnus-header-content :weight bold :foreground ,orange))))
         (gnus-header-name ((t (:inherit header-line :foreground ,green :background nil))))
         (gnus-button ((t (:inherit link :foreground nil))))
         (gnus-signature ((t (:inherit font-lock-comment-face))))

         (gnus-summary-normal-unread ((t (:foreground ,strong :weight normal))))
         (gnus-summary-normal-read ((t (:foreground ,normal :weight normal))))
         (gnus-summary-normal-ancient ((t (:foreground ,cyan :weight normal))))
         (gnus-summary-normal-ticked ((t (:foreground ,orange :weight normal))))
         (gnus-summary-low-unread ((t (:foreground ,faint :weight normal))))
         (gnus-summary-low-read ((t (:foreground ,faintest :weight normal))))
         (gnus-summary-low-ancient ((t (:foreground ,faintest :weight normal))))
         (gnus-summary-high-unread ((t (:foreground ,yellow :weight normal))))
         (gnus-summary-high-read ((t (:foreground ,green :weight normal))))
         (gnus-summary-high-ancient ((t (:foreground ,green :weight normal))))
         (gnus-summary-high-ticked ((t (:foreground ,orange :weight normal))))
         (gnus-summary-cancelled ((t (:foreground ,red :background nil :weight normal))))

         (gnus-group-mail-low ((t (:foreground ,faintest))))
         (gnus-group-mail-low-empty ((t (:foreground ,faintest))))
         (gnus-group-mail-1 ((t (:foreground nil :weight normal :inherit outline-1))))
         (gnus-group-mail-2 ((t (:foreground nil :weight normal :inherit outline-2))))
         (gnus-group-mail-3 ((t (:foreground nil :weight normal :inherit outline-3))))
         (gnus-group-mail-4 ((t (:foreground nil :weight normal :inherit outline-4))))
         (gnus-group-mail-5 ((t (:foreground nil :weight normal :inherit outline-5))))
         (gnus-group-mail-6 ((t (:foreground nil :weight normal :inherit outline-6))))
         (gnus-group-mail-1-empty ((t (:inherit gnus-group-mail-1 :foreground ,faint))))
         (gnus-group-mail-2-empty ((t (:inherit gnus-group-mail-2 :foreground ,faint))))
         (gnus-group-mail-3-empty ((t (:inherit gnus-group-mail-3 :foreground ,faint))))
         (gnus-group-mail-4-empty ((t (:inherit gnus-group-mail-4 :foreground ,faint))))
         (gnus-group-mail-5-empty ((t (:inherit gnus-group-mail-5 :foreground ,faint))))
         (gnus-group-mail-6-empty ((t (:inherit gnus-group-mail-6 :foreground ,faint))))

         (custom-variable-tag ((t (:foreground ,blue))))
         (custom-group-tag ((t (:foreground ,blue))))
         )))))

;;;###autoload
(defun color-theme-sanityinc-solarized-dark ()
  (interactive)
  (color-theme-sanityinc-solarized 'dark))

;;;###autoload
(defun color-theme-sanityinc-solarized-light ()
  (interactive)
  (color-theme-sanityinc-solarized 'light))


(provide 'color-theme-sanityinc-solarized)
;;; color-theme-sanityinc-solarized.el ends here
