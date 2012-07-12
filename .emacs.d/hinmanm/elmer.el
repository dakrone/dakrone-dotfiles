(defun elmer-chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'" str)
    (setq str (replace-match "" t t str)))
  str)

(defun elmer (name)
  (interactive "MName (defaults to random): ")
  (let* ((buf (get-buffer-create "*elmer*"))
         (cmd "zsh <(curl -s p.draines.com/sh)")
         (cmd (if (equal "" name)
                  cmd
                (concat cmd " " name)))
         (resp (elmer-chomp
                (save-excursion
                  (shell-command-on-region (mark) (point) cmd buf)
                  (set-buffer buf)
                  (buffer-string))))
         (url (if (string-match " http" resp)
                  (kill-new (car (last (split-string resp)))))))
    resp))

(provide 'elmer)
