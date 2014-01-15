;; Emacs package system
(require 'package)
(setq package-user-dir (concat user-emacs-directory "elpa"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)

(package-refresh-contents)

(defvar my/install-packages
  '(
    ;; use-package
    use-package

    ;; themeing
    rainbow-mode leuven-theme dakrone-theme smart-mode-line

    ;; misc
    diminish yagist

    ;; IRC/ERC and social stuff
    erc-hl-nicks ercn todochiku twittering-mode

    ;; for auto-complete
    auto-complete fuzzy popup ac-nrepl

    ;; editing utilities
    expand-region smex windresize ag undo-tree iedit ido-ubiquitous
    ido-vertical-mode yasnippet idle-highlight-mode smart-tab anzu
    smartparens flx-ido projectile smooth-scrolling yascroll keyfreq
    lusty-explorer discover prodigy

    ;; org-mode
    org htmlize gnuplot-mode gnuplot

    ;; buffer utils
    popwin

    ;; haskell
    haskell-mode ghc ghci-completion

    ;; config
    ssh-config-mode

    ;; flycheck
    flycheck flycheck-tip

    ;; clojure
    clojure-mode cider paredit parenface kibit-mode clojure-test-mode
    cider-tracing cider-decompile javap-mode

    ;; perl
    cperl-mode

    ;; python
    jedi

    ;; ruby
    ruby-mode ruby-test-mode inf-ruby

    ;; rust
    rust-mode

    ;; go
    go-mode

    ;; java
    emacs-eclim

    ;; emacs-lisp
    elisp-slime-nav paredit

    ;; markup language
    markdown-mode markdown-mode+ yaml-mode zencoding-mode

    ;; helm
    helm helm-descbinds helm-ag helm-projectile

    ;; git
    magit git-gutter
    ))

(dolist (pack my/install-packages)
  (unless (package-installed-p pack)
    (package-install pack)))
