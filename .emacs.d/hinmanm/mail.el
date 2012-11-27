;; mail-related things

;; ==== Mail stuff ====
;; mu4e stuff
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e/")
(require 'mu4e)

(setq mu4e-mu-binary "/usr/local/bin/mu")

(setq smtpmail-smtp-server "smtp.example.org")
;;(setq mu4e-sent-messages-behavior 'delete)

;; save attachments to the desktop
(setq mu4e-attachment-dir "~/Desktop")
;; attempt to show images
(setq mu4e-view-show-images t
      mu4e-view-image-max-width 800)

(setq smtpmail-queue-mail nil ;; start in non-queuing mode
      smtpmail-queue-dir        "~/Mail/queue/")

;; Always use pgg to sign messages
(setq mml2015-use 'epg)
;; Always use pgg to sign messages
(setq pgg-default-user-id "3acecae0")
(setq epg-gpg-program "/usr/local/bin/gpg")

(require 'gnus-dired)
;; make the `gnus-dired-mail-buffers' function also work on
;; message-mode derived modes, such as mu4e-compose-mode
(defun gnus-dired-mail-buffers ()
  "Return a list of active message buffers."
  (let (buffers)
    (save-current-buffer
      (dolist (buffer (buffer-list t))
        (set-buffer buffer)
        (when (and (derived-mode-p 'message-mode)
                   (null message-sent-message-via))
          (push (buffer-name buffer) buffers))))
    (nreverse buffers)))

(setq gnus-dired-mail-mode 'mu4e-user-agent)
(add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)

;; David's (and now my) config

 ;; Vars used below
(defvar kdl-mu4e-new-mail nil
  "Boolean to represent if there is new mail.")

(defvar kdl-mu4e-url-location-list '()
  "Stores the location of each link in a mu4e view buffer")

;; This is also defined in init.el, but b/c ESK runs all files in the
;; user-dir before init.el it must also be defined here
(defvar message-filter-regexp-list '()
  "regexps to filter matched msgs from the echo area when message is called")

(setq message-kill-buffer-on-exit t ;; kill sent msg buffers
      ;; use msmtp
      message-send-mail-function 'message-send-mail-with-sendmail
      sendmail-program   "/usr/local/bin/msmtp"
      ;; Look at the from header to determine the account from which
      ;; to send. Might not be needed b/c of kdl-msmtp
      mail-specify-envelope-from t
      mail-envelope-from 'header
      message-sendmail-envelope-from 'header
      ;; emacs email defaults
      user-mail-address  "lee@writequit.org"
      user-full-name     "Lee Hinman"
      mail-host-address  "writequit.org"
      ;; mu4e defaults
      mu4e-maildir       "~/.mail"
      ;; misc mu settings
      ;; Unicode FTW
      ;;      mu4e-use-fancy-chars t
      mu4e-use-fancy-chars nil
      ;; use the python html2text shell command to strip html
      ;; brew install html2text
      mu4e-html2text-command "/usr/local/bin/html2text -nobs"
      ;; check for new messages ever 5 minutes
      mu4e-update-interval 300)

 ;; Multi-account support
(defun kdl-mu4e-current-account (&optional msg ignore-message-at-point)
  "Figure out what the current account is based on the message being
  composed, the message under the point, or (optionally)the message
  passed in. Also supports ignoring the msg at the point."
  (let ((cur-msg (or msg
                     mu4e-compose-parent-message
                     (and (not ignore-message-at-point)
                          (mu4e-message-at-point)))))
    (when cur-msg
      (let ((maildir (mu4e-msg-field cur-msg :maildir)))
        (string-match "/\\(.*?\\)/" maildir)
        (match-string 1 maildir)))))

(defun is-gmail-account? (acct)
  (if (or (equal "sonian" acct) (equal "gmail" acct))
      t nil))

;; my elisp is bad and I should feel bad
(defun mlh-folder-for (acct g-folder-name other-folder-name)
  (if (or (equal "sonian" acct) (equal "gmail" acct))
      (format "/%s/[Gmail].%s" acct g-folder-name)
    (format "/%s/INBOX.%s" acct other-folder-name)))

;; Support for multiple accounts
(setq mu4e-sent-folder   (lambda (msg)
                           (mlh-folder-for (kdl-mu4e-current-account msg)
                                           "Sent Mail" "Sent"))
      mu4e-drafts-folder (lambda (msg)
                           (mlh-folder-for (kdl-mu4e-current-account msg)
                                           "Drafts" "Drafts"))
      mu4e-trash-folder  (lambda (msg)
                           (mlh-folder-for (kdl-mu4e-current-account msg)
                                           "Trash" "Trash"))
      mu4e-refile-folder (lambda (msg)
                           (mlh-folder-for (kdl-mu4e-current-account msg)
                                           "All Mail" "Archive"))
      ;; The following list represents the account followed by key /
      ;; value pairs of vars to set when the account is chosen
      kdl-mu4e-account-alist
      '(("gmail"
         (user-mail-address   "matthew.hinman@gmail.com")
         (msmtp-account       "gmail")
         (mu4e-sent-messages-behavior delete))
        ("sonian"
         (user-mail-address   "lee.hinman@sonian.net")
         (msmtp-account       "sonian")
         (mu4e-sent-messages-behavior delete))
        ("writequit"
         (user-mail-address   "lee@writequit.org")
         (msmtp-account       "writequit")
         (mu4e-sent-messages-behavior sent)))
      ;; These are used when mu4e checks for new messages
      mu4e-my-email-addresses
      (mapcar (lambda (acct) (cadr (assoc 'user-mail-address (cdr acct))))
              kdl-mu4e-account-alist))

(defun kdl-mu4e-choose-account ()
  "Prompt the user for an account to use"
  (completing-read (format "Compose with account: (%s) "
                           (mapconcat #'(lambda (var) (car var))
                                      kdl-mu4e-account-alist "/"))
                   (mapcar #'(lambda (var) (car var))
                           kdl-mu4e-account-alist)
                   nil t nil nil (caar kdl-mu4e-account-alist)))

(defun kdl-mu4e-set-compose-account ()
  "Set various vars when composing a message. The vars to set are
  defined in kdl-mu4e-account-alist."
  (let* ((account (or (kdl-mu4e-current-account nil t)
                      (kdl-mu4e-choose-account)))
         (account-vars (cdr (assoc account kdl-mu4e-account-alist))))
    (when account-vars
      (mapc #'(lambda (var)
                (set (car var) (cadr var)))
            account-vars))))
(add-hook 'mu4e-compose-pre-hook 'kdl-mu4e-set-compose-account)

 ;; Send mail through msmtp (setq stuff is below)
(defun kdl-msmtp ()
  "Add some arguments to the msmtp call in order to route the message
  through the right account."
  (if (message-mail-p)
      (save-excursion
        (let* ((from (save-restriction (message-narrow-to-headers)
                                       (message-fetch-field "from"))))
          (setq message-sendmail-extra-arguments (list "-a" msmtp-account))))))
(add-hook 'message-send-mail-hook 'kdl-msmtp)

 ;; Notification stuff
(setq global-mode-string
      (if (string-match-p "kdl-mu4e-new-mail"
                          (prin1-to-string global-mode-string))
          global-mode-string
        (cons
         ;;         '(kdl-mu4e-new-mail "✉" "")
         '(kdl-mu4e-new-mail "MAIL" "")
         global-mode-string)))

(defun kdl-mu4e-unread-mail-query ()
  "The query to look for unread messages in all account INBOXes.
  More generally, change this code to affect not only when the
  envelope icon appears in the modeline, but also what shows up in
  mu4e under the Unread bookmark"
  (mapconcat
   (lambda (acct)
     (let ((name (car acct)))
       (format "%s"
               (mapconcat (lambda (fmt)
                            (format fmt name))
                          '("flag:unread AND maildir:/%s/INBOX")
                          " "))))
   kdl-mu4e-account-alist
   " OR "))

(defun kdl-mu4e-new-mail-p ()
  "Predicate for if there is new mail or not"
  (not (eq 0 (string-to-number
              (replace-regexp-in-string
               "[ \t\n\r]" "" (shell-command-to-string
                               (concat "mu find "
                                       (kdl-mu4e-unread-mail-query)
                                       " | wc -l")))))))

(defun kdl-mu4e-notify ()
  "Function called to update the new-mail flag used in the mode-line"
  ;; This delay is to give emacs and mu a chance to have changed the
  ;; status of the mail in the index
  (run-with-idle-timer
   1 nil (lambda () (setq kdl-mu4e-new-mail (kdl-mu4e-new-mail-p)))))

;; I put a lot of effort (probably too much) into getting the
;; 'new mail' icon to go away by showing or hiding it:
;; - periodically (this runs even when mu4e isn't running)
(setq kdl-mu4e-notify-timer (run-with-timer 0 300 'kdl-notmuch-notify))
;; - when the index is updated (this runs when mu4e is running)
(add-hook 'mu4e-index-updated-hook 'kdl-mu4e-notify)
;; - after mail is processed (try to make the icon go away)
(defadvice mu4e-mark-execute-all
  (after mu4e-mark-execute-all-notify activate) 'kdl-mu4e-notify)
;; - when a message is opened (try to make the icon go away)
(add-hook 'mu4e-view-mode-hook 'kdl-mu4e-notify)
;; wrap lines
(add-hook 'mu4e-view-mode-hook 'visual-line-mode)

(defun kdl-mu4e-quit-and-notify ()
  "Bury the buffer and check for new messages. Mainly this is intended
  to clear out the envelope icon when done reading mail."
  (interactive)
  (bury-buffer)
  (kdl-mu4e-notify))

;; Make 'quit' just bury the buffer
(define-key mu4e-headers-mode-map "q" 'kdl-mu4e-quit-and-notify)
(define-key mu4e-main-mode-map "q" 'kdl-mu4e-quit-and-notify)

 ;; View mode stuff
;; Make it possible to tab between links
(defun kdl-mu4e-populate-url-locations (&optional force)
  "Scans the view buffer for the links that mu4e has identified and
  notes their locations"
  (when (or (null kdl-mu4e-url-location-list) force)
    (make-local-variable 'kdl-mu4e-url-location-list)
    (let ((pt (next-single-property-change (point-min) 'face)))
      (while pt
        (when (equal (get-text-property pt 'face) 'mu4e-view-link-face)
          (add-to-list 'kdl-mu4e-url-location-list pt t))
        (setq pt (next-single-property-change pt 'face)))))
  kdl-mu4e-url-location-list)

(defun kdl-mu4e-move-to-link (pt)
  (if pt
      (goto-char pt)
    (error "No link found.")))

(defun kdl-mu4e-forward-url ()
  "Move the point to the beginning of the next link in the buffer"
  (interactive)
  (let* ((pt-list (kdl-mu4e-populate-url-locations)))
    (kdl-mu4e-move-to-link
     (or (some (lambda (pt) (when (> pt (point)) pt)) pt-list)
         (some (lambda (pt) (when (> pt (point-min)) pt)) pt-list)))))

(defun kdl-mu4e-backward-url ()
  "Move the point to the beginning of the previous link in the buffer"
  (interactive)
  (let* ((pt-list (reverse (kdl-mu4e-populate-url-locations))))
    (kdl-mu4e-move-to-link
     (or (some (lambda (pt) (when (< pt (point)) pt)) pt-list)
         (some (lambda (pt) (when (< pt (point-max)) pt)) pt-list)))))

(define-key mu4e-view-mode-map (kbd "TAB") 'kdl-mu4e-forward-url)
(define-key mu4e-view-mode-map (kbd "<backtab>") 'kdl-mu4e-backward-url)

 ;; Misc
;; The bookmarks for the main screen
(setq mu4e-bookmarks
      `((,(kdl-mu4e-unread-mail-query) "New messages"         ?b)
        ("date:today..now"             "Today's messages"     ?t)
        ("date:7d..now"                "Last 7 days"          ?W)
        ("maildir:/writequit/INBOX"    "Writequit"            ?w)
        ("maildir:/sonian/INBOX"       "Sonian"               ?s)
        ("maildir:/gmail/INBOX"        "Gmail"                ?g)
        ("maildir:/writequit/INBOX OR maildir:/sonian/INBOX OR maildir:/gmail/INBOX"
         "All Mail" ?a)
        ("mime:image/*"                "Messages with images" ?p)))

;; Skip the main mu4e screen and go right to unread
(defun kdl-mu4e-view-unread ()
  "Open the Unread bookmark directly"
  (interactive)
  (mu4e~start)
  (mu4e-headers-search-bookmark (mu4e-get-bookmark-query ?b)))

(global-set-key (kbd "C-c 2") 'kdl-mu4e-view-unread)

;; Don't echo some mu4e messages
(add-to-list 'message-filter-regexp-list "mu4e.*Indexing.*processed")
(add-to-list 'message-filter-regexp-list "mu4e.*Retrieving mail")
(add-to-list 'message-filter-regexp-list "mu4e.*Started")

 ;; Start it up
(when (eq window-system 'ns)
  ;; start mu4e
  (mu4e~start)
  ;; check for unread messages
  (kdl-mu4e-notify))
