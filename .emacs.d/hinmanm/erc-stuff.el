;; ERC-related things


(when (eq window-system 'ns)
  (add-to-list 'load-path "~/src/elisp/ercn")
  (require 'ercn)

  (add-to-list 'load-path "~/src/elisp/grr.el")
  (require 'grr)

  (setq grr-command "/usr/local/bin/growlnotify")

  (setq ercn-notify-rules
        '((message . ("#84115" "#search" "#devs" "#safe"))
          (current-nick . all)
          (keyword . all)
          ;;(pal . all)
          (query-buffer . all)))

  (defun do-notify (nickname message)
    (grr-notify (buffer-name) (concat nickname ": " message) nil))

  (add-hook 'ercn-notify 'do-notify)
  (add-to-list 'erc-modules 'ercn))

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

(setq erc-button-url-regexp
      "\\([-a-zA-Z0-9_=!?#$@~`%&*+\\/:;,]+\\.\\)+[-a-zA-Z0-9_=!?#$@~`%&*+\\/:;,]*[-a-zA-Z0-9\\/]")

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
                      "yazirian" "danielglauser")
           erc-keywords '("clojuredocs" "clj-http" "cheshire" "itsy" "opennlp")
           erc-ignore-list '("sonian-chef")
           erc-log-matches-types-alist
           '((keyword . "ERC Keywords")
             (current-nick . "ERC Messages Addressed To You"))
           erc-log-matches-flag t
           erc-prompt-for-nickserv-password nil)
     (require 'erc-services)
     (and
      (require 'erc-highlight-nicknames)
      (add-to-list 'erc-modules 'highlight-nicknames)
      (erc-update-modules))
     (erc-services-mode 1)
     (add-to-list 'erc-modules 'highlight-nicknames 'spelling)
     (add-hook 'erc-connect-pre-hook (lambda (x) (erc-update-modules)))))

(setq erc-server-reconnect-timeout 5)
(setq erc-server-reconnect-attempts 4)

(setq erc-fill-column 100)
