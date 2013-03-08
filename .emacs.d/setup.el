;; Emacs package system
(require 'package)
(setq package-user-dir (concat user-emacs-directory "elpa"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)

(package-refresh-contents)

(defvar my/install-packages
  '(
    ;;;; themeing
    color-theme rainbow-mode

    ;;;; misc
    diminish dired+ erc-hl-nicks ercn

    ;;;; for auto-complete
    auto-complete fuzzy popup ac-slime pos-tip ac-nrepl

    ;;;; highlight
    ace-jump-mode vline col-highlight

    ;;;; editing utilities
    expand-region wrap-region smex windresize
    undo-tree mark-multiple redo+ smartrep
    yasnippet goto-chg find-file-in-project idle-highlight-mode

    ;;;; buffer utils
    popwin elscreen yascroll

    ;;;; programming
    ;; haskell
    haskell-mode

    ;; config
    ssh-config-mode

    ;; flymake
    flycheck

    ;; clojure
    clojure-mode nrepl paredit parenface kibit-mode

    ;; coffee-script
    coffee-mode

    ;; perl
    cperl-mode

    ;; python
    jedi

    ;; ruby
    ruby-block ruby-compilation ruby-end ruby-interpolation
    ruby-mode ruby-test-mode ruby-tools inf-ruby
    yari ruby-electric rsense

    ;; emacs-lisp
    elisp-slime-nav thingopt

    ;; Common Lisp
    slime paredit

    ;; common utility
    quickrun

    ;;;; markup language
    haml-mode sass-mode htmlize
    markdown-mode markdown-mode+
    scss-mode yaml-mode zencoding-mode

    ;; helm
    helm helm-gtags helm-descbinds helm-themes helm-ag

    ;; git
    magit git-gutter

    init-loader
))

(dolist (pack my/install-packages)
  (unless (package-installed-p pack)
    (package-install pack)))
