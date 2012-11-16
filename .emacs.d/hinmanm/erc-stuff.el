;; ERC-related things


;; ==== Growl support on OSX ====
;; (defun growl-notification (title message &optional sticky)
;;   "Send a Growl notification"
;;   (do-applescript
;;    (format "tell application \"GrowlHelperApp\"
;;               notify with name \"Emacs Notification\" title \"%s\" description \"%s\" application name \"Emacs.app\" sticky %s
;;            end tell"
;;            title
;;            (replace-regexp-in-string "\"" "'" message)
;;            (if sticky "yes" "no"))))

;; ==== ERC stuff ====
;; Only track my nick(s)
(defadvice erc-track-find-face
  (around erc-track-find-face-promote-query activate)
  (if (erc-query-buffer-p)
      (setq ad-return-value (intern "erc-current-nick-face"))
    ad-do-it))

(setq erc-keywords '("dakrone" "dakrone_" "dakrone__"
                     "clj-http"
                     "cheshire" "Cheshire"
                     "clojure-opennlp"))
;;(setq erc-hide-list '("JOIN" "PART" "QUIT"))
(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                "324" "329" "332" "333" "353" "477"))

;; (defun call-growl (matched-type nick msg)
;;   (let* ((nick (first (split-string nick "!"))))
;;     (growl-notification nick msg)))

;; only add the hook for Mac
;; (when (eq window-system 'ns)
;;   (add-hook 'erc-text-matched-hook 'call-growl))

(setq erc-button-url-regexp
      "\\([-a-zA-Z0-9_=!?#$@~`%&*+\\/:;,]+\\.\\)+[-a-zA-Z0-9_=!?#$@~`%&*+\\/:;,]*[-a-zA-Z0-9\\/]")

;; (and
;;  (require 'erc-highlight-nicknames)
;;  (add-to-list 'erc-modules 'highlight-nicknames)
;;  (erc-update-modules))

;; update ERC prompt with room name
(setq erc-prompt (lambda ()
                   (if (and (boundp 'erc-default-recipients)
                            (erc-default-target))
                       (erc-propertize (concat (erc-default-target) ">")
                                       'read-only t 'rear-nonsticky t
                                       'front-nonsticky t)
                     (erc-propertize (concat "ERC>") 'read-only t
                                     'rear-nonsticky t
                                     'front-nonsticky t))))

;; Don't highlight pals, because I like highlight-nicknames for that
(setq erc-pal-highlight-type 'nil)

(eval-after-load 'erc
  '(progn
     (setq erc-fill-column 75
           erc-server-coding-system '(utf-8 . utf-8)
           erc-hide-list '("JOIN" "PART" "QUIT" "NICK")
           erc-track-exclude-types (append '("324" "329" "332" "333"
                                             "353" "477" "MODE")
                                           erc-hide-list)
           erc-nick '("dakrone" "dakrone_" "dakrone__")
           erc-autojoin-timing :ident
           erc-flood-protect nil
           erc-pals '("hiredman" "danlarkin" "drewr" "pjstadig" "scgilardi"
                      "joegallo" "jimduey" "leathekd" "rhickey" "zkim" "steve"
                      "imotov" "joekinsella" "craig" "technomancy" "ddillinger"
                      "yazirian")
           erc-keywords '("clojuredocs" "clj-http" "cheshire" "itsy" "opennlp")
           erc-ignore-list '("sonian-chef")
           erc-log-matches-types-alist
           '((keyword . "ERC Keywords")
             (current-nick . "ERC Messages Addressed To You"))
           erc-log-matches-flag t
           erc-prompt-for-nickserv-password nil)
     (require 'erc-services)
;;     (require 'erc-spelling)
     (and
      (require 'erc-highlight-nicknames)
      (add-to-list 'erc-modules 'highlight-nicknames)
      (erc-update-modules))
     (erc-services-mode 1)
     (add-to-list 'erc-modules 'highlight-nicknames 'spelling)
     (add-hook 'erc-connect-pre-hook (lambda (x) (erc-update-modules)))))

(setq erc-server-reconnect-timeout 5)
(setq erc-server-reconnect-attempts 4)


