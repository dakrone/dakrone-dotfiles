;; Theming stuff

(require 'powerline)

(powerline-default-theme)

;; clojure-specific colors
(defmacro defclojureface (name color desc &optional others)
  `(defface
     ,name '((((class color)) (:foreground ,color ,@others)))
     ,desc :group 'faces))

(defun dakrone-dark ()
  ;; https://github.com/dakrone/dakrone-theme
  (load-theme 'dakrone t)
  (if window-system
    (set-background-color "#262626")))

(defun dakrone-light ()
  ;; https://github.com/fniessen/emacs-leuven-theme
  (load-theme 'leuven t)
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

;; Define faces in clojure code
(defun tweak-clojure-syntax ()
  "Tweaks syntax for Clojure-specific faces."
  (mapcar (lambda (x) (font-lock-add-keywords nil x))
          '((("#?['`]*(\\|)"       . 'clojure-parens))
            (("#?\\^?{\\|}"        . 'clojure-brackets))
            (("\\[\\|\\]"          . 'clojure-braces))
            ((":\\w+"              . 'clojure-keyword))
            (("nil\\|true\\|false\\|%[1-9]?" . 'clojure-special))
            (("(\\(\\.[^ \n)]*\\|[^ \n)]+\\.\\|new\\)\\([ )\n]\\|$\\)" 1
              'clojure-java-call)))))

(add-hook 'clojure-mode-hook 'tweak-clojure-syntax)

(if (eq my/background 'dark)
    (dakrone-dark)
  (dakrone-light))
