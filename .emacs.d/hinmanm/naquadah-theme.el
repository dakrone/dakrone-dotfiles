;;; naquadah-theme.el --- A color theme

;; Copyright (C) 2011 Julien Danjou

;; Authors: Julien Danjou <julien@danjou.info>

;; This file is NOT part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(deftheme naquadah
  "Naquadah theme.")


;; We want the face to be created even if they do not exist.
(put 'naquadah 'theme-immediate t)

;; This colors are stolen from Tango.
(defun naquadah
  (let* ((aluminium-1 "#eeeeec")
         (aluminium-2 "#d3d7cf")
         (aluminium-3 "#babdb6")
         (aluminium-4 "#888a85")
         (aluminium-5 "#555753")
         (aluminium-6 "#2e3436")

         (butter-1 "#fce94f")
         (butter-2 "#edd400")
         (butter-3 "#c4a000")

         (orange-1 "#fcaf3e")
         (orange-2 "#f57900")
         (orange-3 "#ce5c00")

         (chocolate-1 "#e9b96e")
         (chocolate-2 "#c17d11")
         (chocolate-3 "#9f5902")

         (chameleon-1 "#8ae234")
         (chameleon-2 "#73d216")
         (chameleon-3 "#4e9a06")

         (sky-blue-1 "#729fcf")
         (sky-blue-2 "#3465a4")
         (sky-blue-3 "#204a87")

         (plum-1 "#ad7fa8")
         (plum-2 "#75507b")
         (plum-3 "#5c3566")

         (scarlet-red-1 "#ef2929")
         (scarlet-red-2 "#cc0000")
         (scarlet-red-3 "#a40000")

         (background "#252A2B")

         (black "#0c191C")

         (gradient-1 sky-blue-1)
         (gradient-2 chameleon-1)
         (gradient-3 butter-1)
         (gradient-4 plum-1)
         (gradient-5 chocolate-1)
         (gradient-6 orange-1)
         (gradient-7 sky-blue-2)
         (gradient-8 chameleon-2)
         (gradient-9 orange-2)
         (gradient-10 plum-2)
         (gradient-11 chocolate-2))
    (custom-theme-set-faces
     'naquadah
     `(default ((((min-colors 4096)) (:background ,background :foreground ,aluminium-1))))
     `(shadow ((t (:foreground ,aluminium-3))))
     `(secondary-selection ((t (:background ,sky-blue-3))))
     `(cursor ((t (:background ,scarlet-red-3))))
     `(hl-line ((t (:background ,aluminium-6))))
     `(highlight ((t (:background ,scarlet-red-2))))
     `(fringe ((t (:background ,black))))
     `(mode-line ((t (:foreground ,aluminium-1 :background ,black
                                  :box (:line-width 1 :color ,aluminium-6)))))
     `(mode-line-inactive ((t (:foreground ,aluminium-5 :background ,aluminium-6
                                           :box (:line-width 1 :color ,black)))))
     `(mode-line-buffer-id ((t (:bold t :foreground ,orange-2))))
     `(header-line ((t (:foreground ,aluminium-1 :background ,black
                                    :box (:line-width 1 :color ,aluminium-6)))))
     `(region ((t (:background ,black))))
     `(link ((t (:foreground ,sky-blue-1))))
     `(link-visited ((t (:inherit 'link :foreground ,plum-1))))
     `(match ((t (:bold t :background ,chocolate-1 :foreground ,black))))
     `(tooltip ((t (:inherit 'variable-pitch :foreground ,aluminium-1 :background ,black))))
     '(bold ((t (:bold t))))
     '(italic ((t (:italic t))))

     `(font-lock-builtin-face ((t (:foreground ,sky-blue-1))))
     '(font-lock-keyword-face ((t (:inherit 'font-lock-builtin-face :bold t))))
     '(font-lock-comment-face ((t (:inherit 'shadow :italic t))))
     '(font-lock-comment-delimiter-face ((t (:inherit 'font-lock-comment-face))))
     `(font-lock-constant-face ((t (:foreground ,chameleon-2))))
     '(font-lock-type-face ((t (:inherit 'font-lock-constant-face :bold t))))
     '(font-lock-doc-face ((t (:inherit 'shadow))))
     `(font-lock-string-face ((t (:foreground ,plum-1))))
     `(font-lock-variable-name-face ((t (:foreground ,scarlet-red-1))))
     `(font-lock-warning-face ((t (:bold t :foreground ,orange-1))))
     `(font-lock-function-name-face ((t (:foreground ,butter-2 :bold t))))

     '(comint-highlight-prompt ((t ())))

     `(isearch ((t (:background ,orange-3 :foreground ,background))))
     `(isearch-fail ((t (:background ,scarlet-red-2))))
     `(lazy-highlight ((t (:background ,chocolate-1 :foreground ,background))))

     `(show-paren-match-face ((t (:background ,chameleon-3))))
     `(show-paren-mismatch-face ((t (:background ,plum-3))))

     `(minibuffer-prompt ((t (:foreground ,sky-blue-1 :bold t))))

     ;; `(widget-mouse-face ((t (:bold t :foreground ,aluminium-1 :background ,scarlet-red-2))))
     ;; `(widget-field ((t (:foreground ,orange-1 :background "gray30"))))
     ;; `(widget-single-line-field ((t (:foreground ,orange-1 :background "gray30"))))

     `(custom-group-tag ((t (:bold t :foreground ,orange-2 :height 1.3))))
     `(custom-variable-tag ((t (:bold t :foreground ,butter-2 :height 1.1))))
     `(custom-face-tag ((t (:bold t :foreground ,butter-2 :height 1.1))))
     `(custom-state ((t (:foreground ,sky-blue-1))))
     ;; `(custom-button  ((t :background "gray50" :foreground ,black
     ;; :box (:line-width 1 :style released-button))))
     ;; '(custom-variable-button ((t (:inherit 'custom-button))))
     ;; '(custom-button-mouse  ((t (:inherit 'custom-button :background "gray60"))))
     ;; '(custom-button-unraised  ((t (:background "gray50" :foreground "black"))))
     ;; '(custom-button-mouse-unraised  ((t (:inherit 'custom-button-unraised :background "gray60"))))
     ;; '(custom-button-pressed  ((t (:inherit 'custom-button :box (:style pressed-button)))))
     ;; '(custom-button-mouse-pressed-unraised  ((t (:inherit 'custom-button-unraised :background "gray60"))))
     '(custom-documentation ((t (:inherit 'font-lock-comment-face))))

     `(gnus-cite-1 ((t (:foreground ,gradient-1))))
     `(gnus-cite-2 ((t (:foreground ,gradient-2))))
     `(gnus-cite-3 ((t (:foreground ,gradient-3))))
     `(gnus-cite-4 ((t (:foreground ,gradient-4))))
     `(gnus-cite-5 ((t (:foreground ,gradient-5))))
     `(gnus-cite-6 ((t (:foreground ,gradient-6))))
     `(gnus-cite-7 ((t (:foreground ,gradient-7))))
     `(gnus-cite-8 ((t (:foreground ,gradient-8))))
     `(gnus-cite-9 ((t (:foreground ,gradient-9))))
     `(gnus-cite-10 ((t (:foreground ,gradient-10))))
     `(gnus-cite-11 ((t (:foreground ,gradient-11))))
     `(gnus-header-name ((t (:bold t :foreground ,sky-blue-1))))
     `(gnus-header-from ((t (:bold t))))
     `(gnus-header-to ((t (:bold t :foreground ,aluminium-2))))
     `(gnus-header-subject ((t ())))
     `(gnus-header-content ((t (:italic t :foreground ,aluminium-3))))
     '(gnus-header-newsgroups ((t (:inherit 'gnus-header-to))))
     `(gnus-signature ((t (:italic t :foreground ,aluminium-3))))
     `(gnus-summary-cancelled ((t (:background ,black :foreground ,butter-1))))
     `(gnus-summary-normal-ancient ((t (:foreground ,chameleon-3))))
     `(gnus-summary-normal-read ((t (:foreground ,chameleon-1))))
     `(gnus-summary-normal-ticked ((t (:foreground ,scarlet-red-1))))
     `(gnus-summary-normal-unread ((t (:foreground ,aluminium-1))))
     '(gnus-summary-high-ancient ((t (:inherit 'gnus-summary-normal-ancient))))
     '(gnus-summary-high-read ((t (:inherit 'gnus-summary-normal-read))))
     '(gnus-summary-high-ticked ((t (:inherit 'gnus-summary-normal-ticked))))
     '(gnus-summary-high-unread ((t (:inherit 'gnus-summary-normal-unread))))
     '(gnus-summary-low-ancient ((t (:inherit 'gnus-summary-normal-ancient :italic t))))
     '(gnus-summary-low-read ((t (:inherit 'gnus-summary-normal-read :italic t))))
     '(gnus-summary-low-ticked ((t (:inherit 'gnus-summary-normal-ticked :italic t))))
     '(gnus-summary-low-unread ((t (:inherit 'gnus-summary-normal-unread :italic t))))
     `(gnus-summary-selected ((t (:background ,sky-blue-3 :foreground ,aluminium-1))))
     `(spam ((t (:background ,black :foreground ,orange-2))))

     '(message-header-name ((t (:inherit 'gnus-header-name))))
     '(message-header-to ((t ())))
     '(message-header-other ((t ())))
     '(message-header-subject ((t (:inherit 'gnus-header-subject))))
     `(message-header-cc ((t (:foreground ,aluminium-2))))
     `(message-header-xheader ((t (:foreground ,aluminium-4))))
     `(message-separator ((t (:foreground ,sky-blue-3))))
     `(message-mml ((t (:foreground ,chameleon-1))))

     ;; org-mode
     `(org-level-1 ((t (:bold t :foreground ,gradient-1 :height 1.3))))
     `(org-level-2 ((t (:bold t :foreground ,gradient-2 :height 1.2))))
     `(org-level-3 ((t (:bold t :foreground ,gradient-3 :height 1.1))))
     `(org-level-4 ((t (:bold t :foreground ,gradient-4))))
     `(org-level-5 ((t (:bold t :foreground ,gradient-5))))
     `(org-level-6 ((t (:bold t :foreground ,gradient-6))))
     `(org-level-7 ((t (:bold t :foreground ,gradient-7))))
     `(org-level-8 ((t (:bold t :foreground ,gradient-8))))

     '(org-mode-line-clock ((t ())))
     `(org-mode-line-clock-overrun ((t (:foreground ,scarlet-red-1))))
     `(org-document-title ((t (:bold t :foreground ,sky-blue-1 :height 1.4))))
     `(org-document-info ((t (:foreground ,sky-blue-1 :italic t))))
     `(org-todo ((t (:bold t :foreground ,scarlet-red-2))))
     `(org-done ((t (:bold t :foreground ,chameleon-3))))
     `(org-hide ((t (:foreground ,background))))
     `(org-scheduled ((t (:foreground ,chameleon-2))))
     `(org-scheduled-previously ((t (:foreground ,orange-2))))
     `(org-scheduled-today ((t (:foreground ,chameleon-1))))
     `(org-date ((t (:foreground ,chocolate-1))))
     `(org-special-keyword ((t (:foreground ,scarlet-red-1 :bold t))))
     `(org-agenda-done ((t ())))
     `(org-time-grid ((t (:inherit 'shadow))))
     `(org-agenda-date ((t (:foreground ,butter-1 :height 1.2))))
     `(org-agenda-date-today ((t (:inherit 'org-agenda-date :foreground ,butter-2 :weight bold :height 1.3))))
     `(org-agenda-date-tc ((t (:inherit 'org-agenda-date :foreground ,butter-3))))
     `(org-agenda-date-weekend ((t (:inherit 'org-agenda-date :foreground ,scarlet-red-1 :weight bold))))

     `(org-habit-clear-future-face ((t (:background ,sky-blue-3))))
     `(org-habit-clear-face ((t (:background ,sky-blue-2))))
     `(org-habit-ready-future-face ((t (:background ,chameleon-3))))
     `(org-habit-ready-face ((t (:background ,chameleon-2 :foreground ,black))))
     `(org-habit-alert-ready-future-face ((t (:background ,orange-3))))
     `(org-habit-overdue-face ((t (:background ,scarlet-red-3))))
     `(org-habit-overdue-future-face ((t (:background ,scarlet-red-3))))

     ;; egocentric-mode
     `(egocentric-face ((t (:foreground ,scarlet-red-1 :weight bold))))

     ;; erc
     '(erc-direct-msg-face ((t (:inherit 'egocentric-face))))
     '(erc-header-line ((t (:inherit 'header-line))))
     '(erc-input-face ((t (:inherit 'shadow))))
     '(erc-my-nick-face ((t (:inherit 'egocentric-face))))
     `(erc-notice-face ((t (:foreground ,sky-blue-1))))
     `(erc-prompt-face ((t (:background ,black :foreground ,aluminium-1 :weight bold))))
     `(erc-timestamp-face ((t (:foreground ,aluminium-2 :weight bold))))
     `(erc-pal-face ((t (:foreground ,chameleon-1 :weight bold))))
     `(erc-keyword-face ((t (:foreground ,orange-1))))
     '(erc-fool-face ((t (:inherit 'shadow))))
     '(erc-current-nick-face ((t (:inherit 'egocentric-face))))

     `(which-func ((t (:foreground ,sky-blue-1))))

     `(mm-uu-extract ((t (:background ,aluminium-6))))

     ;; diff-mode
     `(diff-added ((t (:foreground ,chameleon-2))))
     `(diff-changed ((t (:foreground ,orange-1))))
     `(diff-removed ((t (:foreground ,scarlet-red-1))))
     '(diff-hunk-header ((t (:bold t))))
     `(diff-function ((t (:foreground ,orange-1))))
     `(diff-header ((t (:background ,aluminium-6))))
     `(diff-file-header ((t (:foreground ,aluminium-1))))

     ;; magit
     '(magit-diff-add ((t (:inherit diff-added))))
     '(magit-diff-del ((t (:inherit diff-removed))))
     '(magit-diff-none ((t (:inherit diff-context))))
     '(magit-diff-hunk-header ((t (:inherit (magit-header diff-hunk-header)))))
     '(magit-diff-file-header  ((t (:inherit (magit-header diff-file-header)))))
     `(magit-log-sha1 ((t (:foreground ,scarlet-red-1))))
     `(magit-log-graph ((t (:foreground ,aluminium-2))))
     `(magit-item-highlight ((t (:background ,aluminium-6))))
     `(magit-item-mark ((t (:foreground ,orange-1))))
     `(magit-log-tag-label ((t (:background ,chameleon-3 :box t))))
     `(magit-log-head-label-bisect-good ((t (:background ,chameleon-2 :box t))))
     `(magit-log-head-label-bisect-bad ((t (:background ,scarlet-red-3 :box t))))
     `(magit-log-head-label-remote ((t (:background ,butter-2 :box t))))
     '(magit-log-head-label-tags ((t (:inherit (magit-log-tag-label)))))
     `(magit-log-head-label-local ((t (:foreground ,sky-blue-1 :background ,aluminium-5
                                                   :box t))))


     ;; git-commit-mode
     '(git-commit-summary-face ((t (:bold t))))
     `(git-commit-branch-face ((t (:foreground ,orange-2 :bold t))))
     `(git-commit-nonempty-second-line-face ((t (:foreground ,scarlet-red-2))))
     '(git-commit-comment-face ((t (:inherit font-lock-comment-face))))
     '(git-commit-known-pseudo-header-face ((t (:inherit gnus-header-name-face))))
     '(git-commit-pseudo-header-face ((t (:inherit gnus-header-content))))

     ;; makefile-mode
     `(makefile-space ((t (:background ,plum-3))))

     ;; rst-mode
     `(rst-level-1-face ((t (:foreground ,gradient-1 :height 1.3))))
     `(rst-level-2-face ((t (:foreground ,gradient-2 :height 1.2))))
     `(rst-level-3-face ((t (:foreground ,gradient-3 :height 1.1))))
     `(rst-level-4-face ((t (:foreground ,gradient-4))))
     `(rst-level-5-face ((t (:foreground ,gradient-5))))
     `(rst-level-6-face ((t (:foreground ,gradient-6)))))))

(provide-theme 'naquadah)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; naquadah-theme.el ends here
