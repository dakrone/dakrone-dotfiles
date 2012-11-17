;; Where elisp comes to die

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


;; (defun call-growl (matched-type nick msg)
;;   (let* ((nick (first (split-string nick "!"))))
;;     (growl-notification nick msg)))

;; ;;only add the hook for Mac
;; (when (eq window-system 'ns)
;;   (add-hook 'erc-text-matched-hook 'call-growl))

;; (and
;;  (require 'erc-highlight-nicknames)
;;  (add-to-list 'erc-modules 'highlight-nicknames)
;;  (erc-update-modules))

