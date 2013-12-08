;; Emacs package system
(require 'package)
(setq package-user-dir (concat user-emacs-directory "elpa"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)

(package-refresh-contents)

(defvar my/install-packages
  '(
    ;; init-loader
    init-loader

    ;; themeing
    color-theme rainbow-mode rainbow-delimiters maxframe leuven-theme powerline

    ;; misc
    diminish erc-hl-nicks ercn erc-tweet todochiku twittering-mode yagist

    ;; for auto-complete
    auto-complete fuzzy popup ac-slime ac-nrepl

    ;; highlight
    ace-jump-mode vline col-highlight

    ;; editing utilities
    expand-region wrap-region smex windresize autopair ido-hacks ag
    undo-tree mark-multiple iedit ido-ubiquitous ido-vertical-mode
    yasnippet goto-chg idle-highlight-mode org org-magit smart-tab anzu
    smartparens flx-ido projectile

    ;; buffer utils
    popwin

    ;; haskell
    haskell-mode ghc ghci-completion

    ;; config
    ssh-config-mode

    ;; flycheck
    flycheck flycheck-tip google-this

    ;; clojure
    clojure-mode cider paredit parenface kibit-mode clojure-test-mode
    clojure-snippets cider-tracing cider-decompile javap-mode

    ;; perl
    cperl-mode

    ;; python
    jedi

    ;; ruby
    ruby-mode ruby-test-mode inf-ruby yari rsense

    ;; rust
    rust-mode

    ;; go
    go-mode

    ;; emacs-lisp
    elisp-slime-nav paredit

    ;; common utility
    quickrun

    ;; markup language
    haml-mode htmlize adoc-mode markdown-mode markdown-mode+ yaml-mode
    zencoding-mode

    ;; helm
    helm helm-gtags helm-descbinds helm-themes helm-ag helm-projectile

    ;; git
    magit git-gutter
    ))

(dolist (pack my/install-packages)
  (unless (package-installed-p pack)
    (package-install pack)))
