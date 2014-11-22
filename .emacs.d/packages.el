;; Emacs package system
(require 'package)
(setq package-user-dir (concat user-emacs-directory "elpa"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)

(package-refresh-contents)

(defvar my/install-packages
  '(
    ;; package management
    use-package

    ;; themeing
    rainbow-mode leuven-theme dakrone-theme smart-mode-line
    color-identifiers-mode subatomic-theme subatomic256-theme
    moe-theme powerline nyan-mode spacegray-theme zenburn-theme

    ;; misc
    diminish gist scpaste

    ;; es-mode
    es-mode

    ;; IRC/ERC and social stuff
    erc-hl-nicks ercn alert twittering-mode

    ;; for auto-complete
    fuzzy popup company

    ;; editing utilities
    expand-region smex windresize ag undo-tree iedit ido-ubiquitous
    ido-vertical-mode yasnippet smart-tab anzu smartparens flx-ido projectile
    smooth-scrolling keyfreq prodigy column-marker ace-jump-mode
    multiple-cursors fancy-narrow easy-kill indent-guide toggle-quotes
    visible-mark simple-call-tree simple-call-tree+ editorconfig ggtags
    bookmark+ volatile-highlights fill-column-indicator golden-ratio wc-mode
    comment-dwim2

    ;; highlighting
    idle-highlight-mode highlight-symbol highlight-numbers highlight-quoted

    ;; LaTeX
    auctex

    ;; org-mode
    htmlize gnuplot-mode gnuplot org-toc ox-reveal

    ;; buffer utils
    popwin dired+ dired-imenu

    ;; haskell
    haskell-mode ghc ghci-completion

    ;; config
    ssh-config-mode

    ;; flycheck
    flycheck flycheck-tip

    ;; clojure
    clojure-mode clojure-test-mode cider paredit paren-face kibit-mode
    cider-decompile ac-cider

    ;; perl
    cperl-mode

    ;; python
    hy-mode jedi

    ;; ruby
    ruby-mode ruby-test-mode inf-ruby puppet-mode rbenv chruby

    ;; rust
    rust-mode

    ;; go
    go-mode

    ;; java
    emacs-eclim malabar-mode groovy-mode javap-mode

    ;; javascript
    tern json-mode js2-mode

    ;; emacs-lisp
    elisp-slime-nav paredit

    ;; markup language
    markdown-mode markdown-mode+ yaml-mode zencoding-mode

    ;; helm
    helm helm-descbinds helm-ag helm-projectile helm-swoop
    helm-gtags

    ;; git
    magit git-gutter git-timemachine magit-gh-pulls

    ;; eshell
    eshell-prompt-extras

    ;; gnus
    gnus-x-gm-raw

    ;; eww
    eww-lnum
    ))

;; org-mode is forced manually
(package-install 'org)

(dolist (pack my/install-packages)
  (unless (package-installed-p pack)
    (package-install pack)))
