;;;; Man and Info document setting

;; woman setting
(eval-after-load "woman"
  '(progn
     (setq woman-use-own-frame nil)
     (setq woman-cache-filename (concat user-emacs-directory "woman_cache.el"))
     (setq woman-manpath '("/usr/share/man" "/usr/local/man" "/usr/man"))))

; spliting window when opening woman buffer
(defadvice woman-really-find-file (around woman-split-window activate)
  (switch-to-buffer-other-window (get-buffer-create "*woman-dummy*"))
  ad-do-it)
