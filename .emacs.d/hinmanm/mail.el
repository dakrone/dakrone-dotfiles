;; mail-related things

;; ==== Mail stuff ====
;; mu4e stuff
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e/")
(require 'mu4e)

(setq mu4e-mu-binary "/usr/local/bin/mu")

(setq mu4e-maildir "~/.mail"
      ;; rest of the paths are relative to mu4e-maildir
      mu4e-sent-folder "/sent"
      mu4e-drafts-folder "/drafts"
      mu4e-trash-folder "/trash"
      mu4e-refile-folder "/archive")

(setq mu4e-get-mail-command "offlineimap"
      mu4e-update-interval 300)

(setq message-send-mail-functio 'smtpmail-send-it)
(setq smtpmail-smtp-server "smtp.example.org")
(setq mu4e-sent-messages-behavior 'delete)

(add-to-list 'mu4e-bookmarks
             '("size:5M..500M" "Big messages" ?b))

;; save attachments to the desktop
(setq mu4e-attachment-dir "~/Desktop")
;; attempt to show images
(setq mu4e-view-show-images t
      mu4e-view-image-max-width 800)

