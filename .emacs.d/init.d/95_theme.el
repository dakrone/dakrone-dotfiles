;;; color-theme-dakrone.el --- My dark theme
;;; Commentary:

;; Version: 0.0.1
;; Keywords: themes
;; Author: Lee Hinman
;; Maintainer: Lee Hinman <lee@writequit.org>
;; This file is not part of GNU Emacs.

;;; Code:

(defun color-theme-dakrone ()
  "My custom color theme.  A little bit of everything."
  (interactive)
  (color-theme-install
   '(color-theme-dakrone
     ((foreground-color . "cornsilk")
      (background-color . "gray10")
      (mouse-color . "black")
      (cursor-color . "white")
      (border-color . "black")
      (background-mode . dark))
     (default ((t (:background "gray10"))))

     (mode-line
      ((t (:foreground "#bfebbf" :background "#2b2b2b"))))
     ;; (mode-line-buffer-id ((t (:inherit zenburn-strong-1-face))))
     (mode-line-inactive
      ((t (:foreground "#5f7f5f"  :background "#2b2b2b"))))

     ;; back to mine
     (highlight ((t (:foreground "wheat" :background "darkslategray"))))
     (linum ((t (:foreground "gray20"))))
     (minibuffer-prompt ((t (:foreground "cyan"))))
     (bold ((t (:bold t))))
     (italic ((t (:italic t))))
     (bold-italic ((t (:bold t :italic t))))
     (region ((t (:background "dimgray"))))
     (secondary-selection ((t (:background "deepskyblue4"))))
     (magit-item-highlight ((t (:background "gray20"))))
     (underline ((t (:underline t))))
     (info-node ((t (:foreground "yellow" :bold t :italic t))))
     (info-menu-5 ((t (:underline t))))
     (info-xref ((t (:foreground "yellow" :bold t))))
     (diary-face ((t (:foreground "orange"))))
     (calendar-today-face ((t (:underline t))))
     (holiday-face ((t (:background "red"))))
     (show-paren-match-face ((t (:background "deepskyblue4"))))
     (show-paren-mismatch-face ((t (:foreground "white" :background "red"))))
     (paren-face ((t (:foreground "#303030"))))
     (font-lock-comment-face ((t (:foreground "#656763"))))
     (font-lock-string-face ((t (:foreground "#8AE234"))))
     ;;(font-lock-keyword-face ((t (:foreground "cyan1"))))
     (font-lock-keyword-face ((t (:foreground "#729FCF"))))
     (font-lock-builtin-face ((t (:foreground "LightSteelBlue"))))
     (font-lock-function-name-face ((t (:foreground "cyan1"))))
     (font-lock-variable-name-face ((t (:foreground "cyan1"))))
     ;;(font-lock-type-face ((t (:foreground "yellow1"))))
     (font-lock-type-face ((t (:foreground "#edd400"))))
     (idle-highlight ((t (:background "#444444"))))
     (font-lock-constant-face ((t (:foreground "salmon"))))
     (font-lock-warning-face ((t (:foreground "gold" :bold t))))
     (blank-space-face ((t (:background "#1e2426"))))
     (blank-tab-face ((t (:foreground "black" :background "cornsilk"))))
     (highline-face ((t (:background "gray35"))))
     ;; can't seem to get colorize-nicks and erc-pals to work together
     ;;     (erc-pal-face ((t (nil))))
     (erc-pal-face ((t (:foreground "indianred"))))
     (eshell-ls-directory-face ((t (:foreground "green" :bold t))))
     (eshell-ls-symlink-face ((t (:foreground "Cyan" :bold t))))
     (eshell-ls-executable-face ((t (:foreground "orange" :bold t))))
     (eshell-ls-readonly-face ((t (:foreground "gray"))))
     (eshell-ls-unreadable-face ((t (:foreground "DarkGrey"))))
     (eshell-ls-special-face ((t (:foreground "Magenta" :bold t))))
     (eshell-ls-missing-face ((t (:foreground "Red" :bold t))))
     (eshell-ls-archive-face ((t (:foreground "Orchid" :bold t))))
     (eshell-ls-backup-face ((t (:foreground "LightSalmon"))))
     (eshell-ls-product-face ((t (:foreground "LightSalmon"))))
     (eshell-ls-clutter-face ((t (:foreground "blue" :bold t))))

     ;; Org
     ;; (org-hide ((t (:foreground "#2e3436"))))
     (org-level-1 ((t (:bold t :foreground "#edd400" :height 1.2))))
     (org-level-2 ((t (:bold t :foreground "#729FCF" :height 1.1))))
     (org-level-3 ((t (:bold t :foreground "cyan1" :height 1.0))))
     ;; (org-level-2 ((t (:bold nil :foreground "#edd400" :height 1.2))))
     ;; (org-level-3 ((t (:bold t :foreground "#6ac214" :height 1.0))))
     ;; (org-level-4 ((t (:bold nil :foreground "tomato" :height 1.0))))
     ;; (org-date ((t (:underline t :foreground "magenta3"))))
     ;; (org-footnote  ((t (:underline t :foreground "magenta3"))))
     ;; (org-link ((t (:foreground "skyblue2" :background "#2e3436"))))
     ;; (org-special-keyword ((t (:foreground "brown"))))
     ;; (org-verbatim ((t (:foreground "#eeeeec" :underline t :slant italic))))
     ;; (org-block ((t (:foreground "#bbbbbc"))))
     ;; (org-quote ((t (:inherit org-block :slant italic))))
     ;; (org-verse ((t (:inherit org-block :slant italic))))
     ;; (org-todo ((t (:bold t :foreground "Red"))))
     ;; (org-done ((t (:bold t :foreground "ForestGreen"))))
     ;; (org-agenda-structure ((t (:weight bold :foreground "tomato"))))
     ;; (org-agenda-date ((t (:foreground "#6ac214"))))
     ;; (org-agenda-date-weekend ((t (:weight normal :foreground "dodger blue"))))
     ;; (org-agenda-date-today ((t (:weight bold :foreground "#edd400"))))


     (sgml-start-tag-face ((t (:foreground "mediumspringgreen"))))
     (custom-button-face ((t (:foreground "white"))))
     (sgml-ignored-face ((t (:foreground "gray20" :background "gray60"))))
     (sgml-doctype-face ((t (:foreground "orange"))))
     (sgml-sgml-face ((t (:foreground "yellow"))))
     (vc-annotate-face-0046FF ((t (:foreground "wheat" :background "black"))))
     (custom-documentation-face ((t (:foreground "white"))))
     (sgml-end-tag-face ((t (:foreground "greenyellow"))))
     (linemenu-face ((t (:background "gray30"))))
     (sgml-entity-face ((t (:foreground "gold"))))
     (message-header-to-face ((t (:foreground "floral white" :bold t))))
     (message-header-cc-face ((t (:foreground "ivory"))))
     (message-header-subject-face ((t (:foreground "papaya whip" :bold t))))
     (message-header-newsgroups-face ((t (:foreground "lavender blush" :bold t :italic t))))
     (message-header-other-face ((t (:foreground "pale turquoise"))))
     (message-header-name-face ((t (:foreground "light sky blue"))))
     (message-header-xheader-face ((t (:foreground "blue"))))
     (message-separator-face ((t (:foreground "sandy brown"))))
     (message-cited-text-face ((t (:foreground "plum1"))))
     (message-mml-face ((t (:foreground "ForestGreen"))))
     (font-latex-bold-face ((t (nil))))
     (font-latex-italic-face ((t (nil))))
     (font-latex-math-face ((t (nil))))
     (font-latex-sedate-face ((t (:foreground "Gray85"))))
     (font-latex-string-face ((t (:foreground "orange"))))
     (font-latex-warning-face ((t (:foreground "gold"))))
     (widget-documentation-face ((t (:foreground "lime green"))))
     (widget-button-face ((t (:bold t))))
     (widget-field-face ((t (:background "gray20"))))
     (widget-single-line-field-face ((t (:background "gray20"))))
     (widget-inactive-face ((t (:foreground "wheat"))))
     (widget-button-pressed-face ((t (:foreground "red"))))
     (custom-invalid-face ((t (:foreground "yellow" :background "red"))))
     (custom-rogue-face ((t (:foreground "pink" :background "black"))))
     (custom-modified-face ((t (:foreground "white" :background "blue"))))
     (custom-set-face ((t (:foreground "blue"))))
     (custom-changed-face ((t (:foreground "wheat" :background "skyblue"))))
     (custom-saved-face ((t (:underline t))))
     (custom-state-face ((t (:foreground "light green"))))
     (custom-variable-tag-face ((t (:foreground "skyblue" :underline t))))
     (custom-variable-button-face ((t (:bold t :underline t))))
     (custom-face-tag-face ((t (:foreground "white" :underline t))))
     (custom-group-tag-face-1 ((t (:foreground "pink" :underline t))))
     (custom-group-tag-face ((t (:foreground "skyblue" :underline t))))
     (swbuff-current-buffer-face ((t (:foreground "red" :bold t))))
     (ediff-current-diff-face-A ((t (:foreground "firebrick" :background "pale green"))))
     (ediff-current-diff-face-B ((t (:foreground "DarkOrchid" :background "Yellow"))))
     (ediff-current-diff-face-C ((t (:foreground "white" :background "indianred"))))
     (ediff-current-diff-face-Ancestor ((t (:foreground "Black" :background "VioletRed"))))
     (ediff-fine-diff-face-A ((t (:foreground "Navy" :background "sky blue"))))
     (ediff-fine-diff-face-B ((t (:foreground "Black" :background "cyan"))))
     (ediff-fine-diff-face-C ((t (:foreground "Black" :background "Turquoise"))))
     (ediff-fine-diff-face-Ancestor ((t (:foreground "Black" :background "Green"))))
     (ediff-even-diff-face-A ((t (:foreground "Black" :background "light grey"))))
     (ediff-even-diff-face-B ((t (:foreground "White" :background "Grey"))))
     (ediff-even-diff-face-C ((t (:foreground "Black" :background "light grey"))))
     (ediff-even-diff-face-Ancestor ((t (:foreground "White" :background "Grey"))))
     (ediff-odd-diff-face-A ((t (:foreground "White" :background "Grey"))))
     (ediff-odd-diff-face-B ((t (:foreground "Black" :background "light grey"))))
     (ediff-odd-diff-face-C ((t (:foreground "White" :background "Grey"))))
     (ediff-odd-diff-face-Ancestor ((t (:foreground "Black" :background "light grey"))))))
  (eval-after-load 'diff-mode
    '(progn
       (set-face-foreground 'diff-added "green4")
       (set-face-background 'diff-added "gray10")
       (set-face-foreground 'diff-removed "red3")
       (set-face-background 'diff-removed "gray10")))

  (eval-after-load 'magit
    '(progn
       (set-face-foreground 'magit-diff-add "green3")
       (set-face-background 'magit-diff-add "gray10")
       (set-face-foreground 'magit-diff-del "red3")
       (set-face-background 'magit-diff-del "gray10")
       (set-face-background 'magit-diff-file-header "gray10")
       (set-face-background 'magit-diff-hunk-header "gray10"))))

;; Show Paren Mode
(defun enable-show-paren-mode () "Enable show-paren mode."
  (interactive)
  (show-paren-mode t)
  (setq show-paren-delay 0
        show-paren-style 'expression))

(defun set-show-paren-face-background-dark ()
  (set-face-background 'show-paren-match-face "#232323"))
(defun set-show-paren-face-background-light ()
  (set-face-background 'show-paren-match-face "#dddddd"))

;; clojure-specific colors
(defmacro defclojureface (name color desc &optional others)
  `(defface
     ,name '((((class color)) (:foreground ,color ,@others)))
     ,desc :group 'faces))

;; My custom theme
(defun dakrone-dark ()
  (interactive)
  (if (eq window-system 'ns)
      (set-face-background 'default "gray10"))
  (if (eq window-system 'mac)
      (set-face-background 'default "gray10"))
  (color-theme-dakrone)
  (set-face-foreground 'paren-face "DimGrey")
  (add-hook 'show-paren-mode-hook 'set-show-paren-face-background-dark)
  (defclojureface clojure-parens       "DimGrey"   "Clojure parens")
  (defclojureface clojure-braces       "DimGrey"   "Clojure braces")
  (defclojureface clojure-brackets     "SteelBlue" "Clojure brackets")
  (defclojureface clojure-keyword      "#729FCF"   "Clojure keywords")
  (defclojureface clojure-namespace    "#c476f1"   "Clojure namespace")
  (defclojureface clojure-java-call    "DarkCyan"   "Clojure Java calls")
  (defclojureface clojure-special      "#1BF21B"   "Clojure special")
  (defclojureface clojure-double-quote "#1BF21B"   "Clojure special")
  (defclojureface clojure-collapsed-fn "Cyan"      "Clojure special"))

(defun dakrone-light ()
  (interactive)
  ;;(load-theme 'tsdh-light t)
  (load-theme 'leuven t)
  (setq frame-background-mode 'light)
  (set-background-color "#fcf4dc")
  (set-face-background 'default "#fcf4dc")
  (set-background-color "#000000")
  (set-face-background 'default "#ffffff")
  (set-cursor-color "#52676f")
  (set-foreground-color "#52676f")
  ;;(set-face-foreground 'paren-face "DimGrey")
  (add-hook 'show-paren-mode-hook 'set-show-paren-face-background-light)
  (defclojureface clojure-parens       "#696969"   "Clojure parens")
  (defclojureface clojure-braces       "#696969"   "Clojure braces")
  (defclojureface clojure-brackets     "#4682b4"   "Clojure brackets")
  (defclojureface clojure-keyword      "DarkCyan"  "Clojure keywords")
  (defclojureface clojure-namespace    "#c476f1"   "Clojure namespace")
  (defclojureface clojure-java-call    "#008b8b"   "Clojure Java calls")
  (defclojureface clojure-special      "#006400"   "Clojure special")
  (defclojureface clojure-double-quote "#006400"   "Clojure special")
  (if window-system
      (set-face-foreground 'region nil)))

(defun tweak-clojure-syntax ()
  (mapcar (lambda (x) (font-lock-add-keywords nil x))
          '((("#?['`]*(\\|)"       . 'clojure-parens))
            (("#?\\^?{\\|}"        . 'clojure-brackets))
            (("\\[\\|\\]"          . 'clojure-braces))
            ((":\\w+"              . 'clojure-keyword))
            (("nil\\|true\\|false\\|%[1-9]?" . 'clojure-special))
            (("(\\(\\.[^ \n)]*\\|[^ \n)]+\\.\\|new\\)\\([ )\n]\\|$\\)" 1
              'clojure-java-call)))))

(add-hook 'clojure-mode-hook 'tweak-clojure-syntax)

(if (eq background 'dark)
    (dakrone-dark)
  (dakrone-light))

;; (enable-show-paren-mode)

;;; 95_theme.el ends here
