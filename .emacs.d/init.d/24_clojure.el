;; ==== Clojure stuff ====
(eval-after-load 'slime '(setq slime-protocol-version 'ignore))
(add-to-list 'auto-mode-alist '("\\.cljs$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.asciidoc$" . adoc-mode))
(add-to-list 'auto-mode-alist '("\\.adoc$" . adoc-mode))

(defun lisp-enable-paredit-hook () (paredit-mode 1))
(add-hook 'lisp-mode-hook 'lisp-enable-paredit-hook)

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

(defun nrepl-popup-tip-symbol-at-point ()
  "show docs for the symbol at point -- AWESOMELY"
  (interactive)
  (popup-tip (ac-nrepl-documentation (symbol-at-point))
             :point (ac-nrepl-symbol-start-pos)
             :around t
             :scroll-bar t
             :margin t))

;; Auto completion for NREPL

;; nrepl auto-complete
(defun set-auto-complete-as-completion-at-point-function ()
  (setq completion-at-point-functions '(auto-complete)))

(add-hook 'auto-complete-mode-hook
          'set-auto-complete-as-completion-at-point-function)

(eval-after-load "auto-complete" '(add-to-list 'ac-modes 'nrepl-mode))

;; Clojure-mode hooks
(add-hook
 'clojure-mode-hook
 (lambda ()
   (require 'ac-nrepl)
   ;; Better indention (from Kevin)
   (setq clojure-mode-use-backtracking-indent t)
   ;; enable eldoc
   (eldoc-mode t)
   ;; use my test layout fns
   (setq clojure-test-for-fn 'my-clojure-test-for)
   (setq clojure-test-implementation-for-fn 'my-clojure-test-implementation-for)
   ;; compile faster
   (setq font-lock-verbose nil)
   (global-set-key (kbd "C-c t") 'clojure-jump-between-tests-and-code)
   (lisp-enable-paredit-hook)
   ;; folding ala jcrossley3
   (require 'fold-dwim-org)
   (hs-minor-mode t)
   (fold-dwim-org/minor-mode t)
   (local-set-key (kbd "C-c TAB") 'fold-dwim-org/minor-mode)
   (local-set-key (kbd "C-c C-u") 'fold-dwim-hide-all)
   (local-set-key (kbd "C-c C-o") 'fold-dwim-show-all)))

;; Nrepl-mode hooks
(add-hook 'nrepl-mode-hook
          (lambda ()
            (define-key nrepl-interaction-mode-map
              (kbd "C-c C-d")
              'nrepl-popup-tip-symbol-at-point)
            (paredit-mode t)
            (subword-mode t)
            (eldoc-mode t)
            (setq nrepl-history-file "~/.nrepl-history")
            (setq nrepl-hide-special-buffers t)
            (setq nrepl-popup-stacktraces-in-repl t)
            (ac-nrepl-setup)
            (set-auto-complete-as-completion-at-point-function)))

;; Nrepl-interaction-mode hooks
(add-hook 'nrepl-interaction-mode-hook
          (lambda ()
            (require 'nrepl-ritz)
            (set-auto-complete-as-completion-at-point-function)
            (nrepl-turn-on-eldoc-mode)))
