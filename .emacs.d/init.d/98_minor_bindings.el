;;;; minor key setting
(autoload 'hs-minor-mode "hideshow" nil t)
(eval-after-load "hideshow"
  '(progn
     (define-key hs-minor-mode-map (kbd "C-#") 'hs-toggle-hiding)))

;; (makunbound 'overriding-minor-mode-map)
(define-minor-mode overriding-minor-mode
  "Most superior minir mode"
  t  ;; default is enable
  "" ;; Not display mode-line
  `((,(kbd "M-a") . backward-paragraph)
    (,(kbd "M-e") . forward-paragraph)
    (,(kbd "C-M-j") . dabbrev-expand)
    (,(kbd "C-M-i") . my/auto-complete)
    (,(kbd "M-C-o") . other-window)))

(defvar my/alt-q-map (make-sparse-keymap)
  "My original keymap binded to M-q.")
(defalias 'my/alt-q-prefix my/alt-q-map)
(define-key overriding-minor-mode-map (kbd "M-q") 'my/alt-q-prefix)

(defun add-hyper-char-to-ace-jump-word-mode (c)
  (define-key my/alt-q-map
    (read-kbd-macro (string c))
    `(lambda ()
       (interactive)
       (setq ace-jump-query-char ,c)
       (setq ace-jump-current-mode 'ace-jump-word-mode)
       (ace-jump-do (concat "\\b"
                            (regexp-quote (make-string 1 ,c)))))))

(loop for c from ?0 to ?9 do (add-hyper-char-to-ace-jump-word-mode c))
(loop for c from ?A to ?Z do (add-hyper-char-to-ace-jump-word-mode c))
(loop for c from ?a to ?z do (add-hyper-char-to-ace-jump-word-mode c))
(loop for c in '(?_ ?+ ?-) do (add-hyper-char-to-ace-jump-word-mode c))
