;; ERC-related things

(setq tls-program
      '("openssl s_client -connect %h:%p -no_ssl2 -ign_eof -cert ~/host.pem"
        "gnutls-cli --priority secure256 --x509certfile ~/host.pem -p %p %h"
        "gnutls-cli --priority secure256 -p %p %h"))

;; Load passwords from file, if it doesn't exist no worries
(when (file-exists-p "~/.ercpass")
  (load-file "~/.ercpass"))

(defun start-irc ()
  "Connect to IRC."
  (interactive)
  (pause-ercn 6)
  (erc-tls :server "freenode" :port 31425
           :nick "dakrone" :password freenode-pass))

(when window-system
  (require 'ercn)
  (require 'todochiku)

  ;;----------------------------------------------------------------------------
  ;; Use terminal-notifier in OS X Mountain Lion
  ;; https://github.com/alloy/terminal-notifier (Install in /Applications)
  ;;----------------------------------------------------------------------------
  (setq terminal-notifier-path
        "/Users/hinmanm/.rvm/gems/ruby-1.9.3-p448/bin/terminal-notifier")

  (defadvice todochiku-get-arguments (around todochiku-nc)
    (setq ad-return-value
          (list "-title" title "-message" message "-activate" "org.gnu.Emacs")))

  (when (file-executable-p terminal-notifier-path)
    (setq todochiku-command terminal-notifier-path)
    (ad-activate 'todochiku-get-arguments))


  ;; load private ercn notify rules if the file exists
  (if (file-exists-p "~/.ercrules")
      (load-file "~/.ercrules")
    (setq ercn-notify-rules
          '((message . ("#denofclojure" "#elasticsearch"))
            (current-nick . all)
            (keyword . all)
            ;;(pal . all)
            (query-buffer . all))))

  (defun do-notify (nickname message)
    (todochiku-message (buffer-name)
                       (concat nickname ": " message)
                       (todochiku-icon 'irc)))

  (add-hook 'ercn-notify 'do-notify)
  (add-to-list 'erc-modules 'ercn)

  (defvar saved-ercn-rules nil)
  (defun pause-ercn (seconds)
    (setq saved-ercn-rules ercn-notify-rules)
    (setq ercn-notify-rules
          '((current-nick . nil)
            (keyword . nil)
            (pal . nil)
            (query-buffer . nil)))
    (run-with-idle-timer
     seconds nil
     (lambda ()
       (setq ercn-notify-rules saved-ercn-rules)))))

;; ==== ERC stuff ====
;; Only track my nick(s)
(defadvice erc-track-find-face
  (around erc-track-find-face-promote-query activate)
  (if (erc-query-buffer-p)
      (setq ad-return-value (intern "erc-current-nick-face"))
    ad-do-it))

(eval-after-load 'erc
  '(progn
     (setq erc-fill-column 90
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
           erc-pal-highlight-type 'nil
           erc-keywords '("dakrone" "dakrone_" "dakrone__" "clj-http"
                          "cheshire" "Cheshire" "clojure-opennlp" "itsy"
                          "opennlp" "elasticsearch-zookeeper"
                          "elasticsearch-jetty" "elasticsearch-equilibrium")
           erc-ignore-list '()
           erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                     "324" "329" "332" "333" "353" "477")
           erc-button-url-regexp
           "\\([-a-zA-Z0-9_=!?#$@~`%&*+\\/:;,]+\\.\\)+[-a-zA-Z0-9_=!?#$@~`%&*+\\/:;,]*[-a-zA-Z0-9\\/]"
           erc-log-matches-types-alist
           '((keyword . "ERC Keywords")
             (current-nick . "ERC Messages Addressed To You"))
           erc-log-matches-flag t
           erc-prompt-for-nickserv-password nil
           erc-server-reconnect-timeout 5
           erc-server-reconnect-attempts 4
           ;; update ERC prompt with room name
           erc-prompt (lambda ()
                        (if (and (boundp 'erc-default-recipients)
                                 (erc-default-target))
                            (erc-propertize (concat (erc-default-target) ">")
                                            'read-only t 'rear-nonsticky t
                                            'front-nonsticky t)
                          (erc-propertize (concat "ERC>") 'read-only t
                                          'rear-nonsticky t
                                          'front-nonsticky t))))
     (require 'erc-services)
     (require 'erc-hl-nicks)
     (add-to-list 'erc-modules 'hl-nicks)
     (add-to-list 'erc-modules 'spelling)
     (erc-services-mode 1)
     (erc-spelling-mode 1)
     (add-hook 'erc-connect-pre-hook (lambda (x) (erc-update-modules)))))
