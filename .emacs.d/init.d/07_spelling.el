;; configuration of spell check
;; ispell is already enabled on linux
;; (require 'ispell)
(setq-default ispell-program-name "aspell")
(add-to-list 'ispell-skip-region-alist '("[^\000-\377]+"))

;; flyspell
(autoload 'flyspell-mode "flyspell" "spell checking at runtime")
(eval-after-load "flyspell"
  '(progn
     (define-key flyspell-mode-map (kbd "M-n") 'flyspell-goto-next-error)
     (define-key flyspell-mode-map (kbd "M-.") 'ispell-word)))

(setq ispell-extra-args '("--sug-mode=ultra" "--ignore=3"))
(setq flyspell-issue-message-flag nil)
(setq ispell-personal-dictionary "~/.flydict")
