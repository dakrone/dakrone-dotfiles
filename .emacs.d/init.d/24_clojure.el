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

;; Better indention (from Kevin)
(add-hook 'clojure-mode-hook
          (lambda ()
            (setq clojure-mode-use-backtracking-indent t)
            (eldoc-mode t)))

;; custom test locations instead of foo_test.clj, use test/foo.clj
(defun my-clojure-test-for (namespace)
  (let* ((namespace (clojure-underscores-for-hyphens namespace))
         (segments (split-string namespace "\\."))
         (before (subseq segments 0 1))
         (after (subseq segments 1))
         (test-segments (append before (list "test") after)))
    (format "%stest/%s.clj"
            (locate-dominating-file buffer-file-name "src/")
            (mapconcat 'identity test-segments "/"))))

(defun my-clojure-test-implementation-for (namespace)
  (let* ((namespace (clojure-underscores-for-hyphens namespace))
         (segments (split-string namespace "\\."))
         (before (subseq segments 0 1))
         (after (subseq segments 2))
         (impl-segments (append before after)))
    (format "%s/src/%s.clj"
            (locate-dominating-file buffer-file-name "src/")
            (mapconcat 'identity impl-segments "/"))))

(eval-after-load 'clojure-mode
  '(progn
     (setq clojure-test-for-fn 'my-clojure-test-for)
     (setq clojure-test-implementation-for-fn
           'my-clojure-test-implementation-for)
     (global-set-key (kbd "C-c t") 'clojure-jump-between-tests-and-code)))

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

(defun my/nrepl-mode-setup ()
  (require 'nrepl-ritz))

(add-hook 'nrepl-interaction-mode-hook 'my/nrepl-mode-setup)
