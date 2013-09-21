;;;; minor key setting
(autoload 'hs-minor-mode "hideshow" nil t)
(eval-after-load "hideshow"
  '(progn
     (define-key hs-minor-mode-map (kbd "C-c TAB") 'hs-toggle-hiding)
     (defvar hs-special-modes-alist
       (mapcar 'purecopy
               '((c-mode "{" "}" "/[*/]" nil nil)
                 (c++-mode "{" "}" "/[*/]" nil nil)
                 (bibtex-mode ("@\\S(*\\(\\s(\\)" 1))
                 (java-mode "{" "}" "/[*/]" nil nil)
                 (js-mode "{" "}" "/[*/]" nil)
                 (javascript-mode  "{" "}" "/[*/]" nil))))))

(add-hook 'js-mode
          '(lambda ()
             (require 'hideshow)
             (hs-minor-mode t)
             (define-key hs-minor-mode-map (kbd "C-c TAB") 'hs-toggle-hiding)))

(add-hook 'javascript-mode
          '(lambda ()
             (require 'hideshow)
             (hs-minor-mode t)
             (define-key hs-minor-mode-map (kbd "C-c TAB") 'hs-toggle-hiding)))

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
