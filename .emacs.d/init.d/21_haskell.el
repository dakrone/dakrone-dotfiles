;; setting haskell language
(load "haskell-site-file") ;; load haskell-mode/haskell-site-file.el

(add-to-list 'auto-mode-alist
         '("\\.\\(hs\\|hi\\|gs\\)\\'" . haskell-mode))

(defun haskell-individual-setup ()
  (turn-on-haskell-doc-mode)
  (turn-on-haskell-indent)
  (ghc-init)
  (flymake-mode)

  ;; bindings
  (define-key haskell-mode-map (kbd "C-c C-d") 'helm-ghc-browse-document)
  (local-unset-key (kbd "C-M-d"))

  ;; for auto-complete
  (add-to-list 'ac-sources 'ac-source-ghc-mod))

(add-hook 'haskell-mode-hook 'haskell-individual-setup)

;; Wrap region with block comment
(defun haskell-block-commend-region (start end)
  (interactive "r")
  (save-excursion
    (let (end-marker)
      (goto-char end)
      (setq end-marker (point-marker))
      (goto-char start)
      (insert "{-\n")
      (goto-char (marker-position end-marker))
      (insert "-}"))))

;; find document
(defvar helm-ghc-mod-source
  '((name . "GHC Browse Documennt")
    (init . helm-ghc-mod-init)
    (candidates-in-buffer)
    (candidate-number-limit . 9999)
    (action . helm-ghc-mod-action-display-document)))

(defun helm-ghc-mod-init ()
  (with-current-buffer (helm-candidate-buffer 'global)
    (unless (call-process-shell-command "ghc-mod list" nil t t)
      (error "Failed 'ghc-mod list'"))))

(defun helm-ghc-mod-action-display-document (candidate)
  (let ((pkg (ghc-resolve-package-name candidate)))
    (if (and pkg candidate)
        (ghc-display-document pkg candidate nil)
      (error (format "Not found %s(Package %s)" candidate pkg)))))

(defun helm-ghc-browse-document ()
  (interactive)
  (helm :sources '(helm-ghc-mod-source) :buffer "*helm-ghc-document*"))
