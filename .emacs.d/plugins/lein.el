;;; Commentary
;;  lein.el is a package that allows simple integration with leiningen through emacs.
;;
;;
;;; Usage
;; 
;; start-lein-swank
;;   Runs 'lein swank' in a buffer, then automatically connects to the running repl when
;;   available
;;
;; start-lein-deps-swank
;;   Runs 'lein deps && lein swank'
;;
;; start-lein-native-deps-swank
;;   Runs 'lein deps && lein native-deps && lein-swank'



(defvar lein-cmd-name "lein")
(defvar lein-proc nil)
(defvar lein-buffer-name "*lein*")
(defvar lein-timer nil)

(defun lein-connect ()
  (interactive)
  (stop-lein-timer)
  (slime-connect "localhost" 4005))

(defun lein-check-last-line () 
  (set-buffer "*lein*")
  (let ((last-line (first (reverse (remove-if (lambda (x) (string= "" x)) (split-string (buffer-string) "\n"))))))
    (cond ((string= last-line "Process lein exited abnormally with code 1") ((lambda () (message "Couldn't start lein process.") (stop-lein-timer))))
	  ((string= last-line "     [null] java.net.BindException: Address already in use (NO_SOURCE_FILE:1)") (message "Address already in use."))
	  ((string= last-line "     [null] #<ServerSocket ServerSocket[addr=localhost/127.0.0.1,port=0,localport=4005]>") (lein-connect)))))

(defun stop-lein-timer ()
  (cancel-timer lein-timer))

(defun start-lein-timer ()
  (when lein-timer
    (stop-lein-timer))
  (setq lein-timer (run-at-time 1 1 'lein-check-last-line)))

(defun lein-buf-changed (beg end len)
  (message (number-to-string beg))
  (message (number-to-string end))
  (message (number-to-string len))
  (message "--"))

(defun start-lein-swank ()
  (interactive)
  (when (get-buffer lein-buffer-name)
    (kill-buffer lein-buffer-name))
  (setq lein-proc (start-process-shell-command "lein" lein-buffer-name (concat  "source ~/.profile && cd " (eproject-root) " && " lein-cmd-name " swank")))
  (start-lein-timer)
  (display-buffer lein-buffer-name)
;  (set-buffer lein-buffer-name)
;  (insert "Starting lein...")
  lein-proc)

(defun start-lein-deps-swank ()
  (interactive)
  (when (get-buffer lein-buffer-name)
    (kill-buffer lein-buffer-name))
  (setq lein-proc 
	(start-process-shell-command 
	 "lein" 
	 lein-buffer-name 
	 (concat  "source ~/.profile && cd " (eproject-root) " && " lein-cmd-name " clean && " lein-cmd-name " deps && " lein-cmd-name " swank")))
  (display-buffer lein-buffer-name)
  (start-lein-timer)
  lein-proc)

(defun start-lein-native-deps-swank ()
  (interactive)
  (when (get-buffer lein-buffer-name)
    (kill-buffer lein-buffer-name))
  (setq lein-proc 
	(start-process-shell-command 
	 "lein" 
	 lein-buffer-name 
	 (concat  "source ~/.profile && cd " (eproject-root) " && " lein-cmd-name " clean && " lein-cmd-name " deps && " lein-cmd-name " native-deps && " lein-cmd-name " swank")))
  (display-buffer lein-buffer-name)
  (start-lein-timer)
  lein-proc)
