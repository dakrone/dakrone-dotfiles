(defun browse-latest-url ()
  (interactive)
  (save-excursion
    (search-backward-regexp "https?://")
    (browse-url-at-point)))

;;(define-key erc-mode-map (kbd "<f2>") 'browse-latest-url)

(defun browse-last-url-in-brower ()
  (interactive)
  (save-excursion
    (let ((ffap-url-regexp
           (concat
            "\\("
            "news\\(post\\)?:\\|mailto:\\|file:"
            "\\|"
            "\\(ftp\\|https?\\|telnet\\|gopher\\|www\\|wais\\)://"
            "\\).")))
      (ffap-next t t))))

(global-set-key (kbd "C-c u") 'browse-last-url-in-brower)
