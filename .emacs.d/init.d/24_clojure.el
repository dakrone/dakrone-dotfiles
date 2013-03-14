;; Clojure
(add-hook 'nrepl-mode-hook 'subword-mode)
(add-hook 'nrepl-interaction-mode-hook 'nrepl-turn-on-eldoc-mode)

;; ==== Clojure stuff ====
(eval-after-load 'slime '(setq slime-protocol-version 'ignore))
(add-to-list 'auto-mode-alist '("\\.cljs$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

(defun lisp-enable-paredit-hook () (paredit-mode 1))
(add-hook 'clojure-mode-hook 'lisp-enable-paredit-hook)
(add-hook 'lisp-mode-hook 'lisp-enable-paredit-hook)

(defmacro defclojureface (name color desc &optional others)
  `(defface
     ,name '((((class color)) (:foreground ,color ,@others)))
     ,desc :group 'faces))

(defclojureface clojure-parens       "DimGrey"   "Clojure parens")
(defclojureface clojure-braces       "DimGrey"   "Clojure braces")
(defclojureface clojure-brackets     "SteelBlue" "Clojure brackets")
(defclojureface clojure-keyword      "#729FCF"   "Clojure keywords")
(defclojureface clojure-namespace    "#c476f1"   "Clojure namespace")
(defclojureface clojure-java-call    "DarkCyan"   "Clojure Java calls")
(defclojureface clojure-special      "#1BF21B"   "Clojure special")
(defclojureface clojure-double-quote "#1BF21B"   "Clojure special")
(defclojureface clojure-collapsed-fn "Cyan"      "Clojure special")

(defun tweak-clojure-syntax ()
  (mapcar (lambda (x) (font-lock-add-keywords nil x))
          '((("#?['`]*(\\|)"       . 'clojure-parens))
            (("#?\\^?{\\|}"        . 'clojure-brackets))
            (("\\[\\|\\]"          . 'clojure-braces))
            ((":\\w+"              . 'clojure-keyword))
            (("#?\""               0 'clojure-double-quote prepend))
            (("nil\\|true\\|false\\|%[1-9]?" . 'clojure-special))
            (("(\\(\\.[^ \n)]*\\|[^ \n)]+\\.\\|new\\)\\([ )\n]\\|$\\)" 1
              'clojure-java-call)))))

(add-hook 'clojure-mode-hook 'tweak-clojure-syntax)

;; Better indention (from Kevin)
(add-hook 'clojure-mode-hook
          (lambda ()
            (setq clojure-mode-use-backtracking-indent t)
            (eldoc-mode t)))

;; Lazytest indention in clojure
(eval-after-load 'clojure-mode
  '(define-clojure-indent
     (describe 'defun)
     (testing 'defun)
     (given 'defun)
     (scenario 'defun)
     (expect 'defun)
     (it 'defun)
     (do-it 'defun)))

;; compile faster
(setq font-lock-verbose nil)

(defun nrepl-popup-tip-symbol-at-point ()
  "show docs for the symbol at point -- AWESOMELY"
  (interactive)
  (popup-tip (ac-nrepl-documentation (symbol-at-point))
             :point (ac-nrepl-symbol-start-pos)
             :around t
             :scroll-bar t
             :margin t))

(add-hook 'nrepl-mode-hook
          (lambda ()
            (define-key nrepl-interaction-mode-map
              (kbd "C-c C-d")
              'nrepl-popup-tip-symbol-at-point)))
(setq nrepl-popup-stacktraces nil)

;; nrepl auto-complete
(add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
(add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'nrepl-mode))

(defun set-auto-complete-as-completion-at-point-function ()
  (setq completion-at-point-functions '(auto-complete)))
(add-hook 'auto-complete-mode-hook
          'set-auto-complete-as-completion-at-point-function)

(add-hook 'nrepl-mode-hook 'set-auto-complete-as-completion-at-point-function)
(add-hook 'nrepl-interaction-mode-hook
          'set-auto-complete-as-completion-at-point-function)