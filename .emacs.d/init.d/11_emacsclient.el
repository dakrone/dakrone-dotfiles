;; server start for emacs-client
(require 'server)

(when (and window-system (not (macosx-p))
           (executable-find "emacs_serverstart.pl"))
  (defadvice server-start
    (after server-start-after-write-window-id ())
    (call-process "emacs_serverstart.pl"
                  nil nil nil
                  (number-to-string (emacs-pid))
                  (if window-system
                      "x"
                    "nox")))
  (ad-activate 'server-start))

(unless (server-running-p)
  (server-start))
