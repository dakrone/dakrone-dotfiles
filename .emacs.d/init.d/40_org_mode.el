;; org-mode
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))

(eval-after-load "org"
  '(progn
     (when (require 'org-install nil t)
       (add-to-list 'org-modules 'org-velocity)
       (define-key global-map (kbd "C-c l") 'org-store-link)
       (setq org-startup-truncated nil
             org-return-follows-link t
             org-directory user-emacs-directory
             org-use-fast-todo-selection t
             org-src-fontify-natively t
             org-todo-keywords
             '((sequence "TODO(t)" "STARTED(s)" "WAITING(w)"
                         "|" "DONE(x)" "CANCEL(c)")
               (sequence "WAITING(f)" "|" "DONE(x)" "CANCEL(c)"))
             org-todo-keyword-faces
             '(;;("TODO"      . org-warning)
               ("STARTED"   . (:foreground "deep sky blue" :weight bold))
               ("DONE"      . (:foreground "SpringGreen1" :weight bold))
               ("WAITING"   . (:foreground "orange" :weight bold)))))
     (define-key org-mode-map (kbd "C-c t") 'org-mark-ring-goto)
     (define-key org-mode-map (kbd "C-M-<return>") 'org-insert-todo-heading)
     (define-key org-mode-map (kbd "C-M-<tab>") 'show-all)
     (local-unset-key (kbd "M-S-<return>"))

     (setq org-clock-persist 'history)
     (org-clock-persistence-insinuate)
     (define-prefix-command 'org-todo-state-map)

     (define-key org-mode-map "\C-cx" 'org-todo-state-map)

     (define-key org-todo-state-map "x"
       #'(lambda nil (interactive) (org-todo "CANCELLED")))
     (define-key org-todo-state-map "d"
       #'(lambda nil (interactive) (org-todo "DONE")))
     (define-key org-todo-state-map "t"
       #'(lambda nil (interactive) (org-todo "TODO")))
     (define-key org-todo-state-map "s"
       #'(lambda nil (interactive) (org-todo "STARTED")))
     (define-key org-todo-state-map "w"
       #'(lambda nil (interactive) (org-todo "WAITING")))

     ;; Babel stuff
     (require 'ob-clojure)
     (org-babel-do-load-languages
      'org-babel-load-languages
      '((emacs-lisp . t)
        (clojure . t)
        (sh . t)
        (ruby . t)
        (python . t)))

     ;; don't run stuff automatically on export
     (setq org-export-babel-evaluate nil)

     ;; always enable noweb, results as code and exporting both
     (setq org-babel-default-header-args
           (cons '(:noweb . "yes")
                 (assq-delete-all :noweb org-babel-default-header-args)))
     (setq org-babel-default-header-args
           (cons '(:results . "code")
                 (assq-delete-all :noweb org-babel-default-header-args)))
     (setq org-babel-default-header-args
           (cons '(:exports . "both")
                 (assq-delete-all :noweb org-babel-default-header-args)))

     ;; ensure this variable is defined defined
     (unless (boundp 'org-babel-default-header-args:sh)
       (setq org-babel-default-header-args:sh '()))

     ;; add a default shebang header argument shell scripts
     (add-to-list 'org-babel-default-header-args:sh
                  '(:shebang . "#!/usr/bin/env zsh"))

     ;; add a default shebang header argument for python
     (add-to-list 'org-babel-default-header-args:python
                  '(:shebang . "#!/usr/bin/env python"))

     ;; I don't want to be prompted on every code block evaluation
     (setq org-confirm-babel-evaluate nil)

     (define-key org-mode-map (kbd "C-c M-c") 'org-babel-execute-buffer)
     (define-key org-mode-map (kbd "C-c M-t") 'org-babel-tangle)

     (defvar org-babel-default-header-args:clojure
       '((:results . "silent")))))

;; (defun org-babel-execute:clojure (body params)
;;   "Execute a block of Clojure code with Babel."
;;   (nrepl-interactive-eval body))

(defun org-babel-execute:clojure (body params)
  "Execute a block of Clojure code with Babel."
  (let ((result-plist
         (nrepl-send-string-sync
          (org-babel-expand-body:clojure body params) nrepl-buffer-ns))
        (result-type  (cdr (assoc :result-type params))))
    (org-babel-script-escape
     (cond ((eq result-type 'value)  (plist-get result-plist :value))
           ((eq result-type 'output) (plist-get result-plist :value))
           (t                        (message "Unknown :results type!"))))))

(add-hook 'org-mode-hook '(lambda ()
                            (define-key org-mode-map [C-tab] 'other-window)
                            (flyspell-mode -1)
                            (define-key org-mode-map [C-S-tab]
                              (lambda ()
                                (interactive)
                                (other-window -1)))))

;; Standard key bindings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(define-key global-map "\C-cc" 'org-capture)
(setq org-directory (concat "~" (user-login-name) "/org"))
;; (setq org-agenda-files (list org-directory))
(setq org-agenda-files (directory-files "~/org" t ".*\.org"))
(setq org-completion-use-ido 't)
(setq org-refile-targets `((,(concat "~" (user-login-name)
                                     "/org/todo.org") . (:level . 1))
                           (,(concat "~" (user-login-name)
                                     "/org/notes.org") . (:level . 1))))
(setq org-capture-templates
      '(("f" "Follow up" entry (file+headline "~/org/todo.org" "Follow up")
         "* WAITING %i%? %^G\nAdded: %U")
        ("t" "Todo" entry (file+headline "~/org/todo.org" "Unsorted")
         "* TODO %i%? %^G\nAdded: %U")
        ("n" "Notes" entry (file+headline "~/org/notes.org" "Notes")
         "* %?\nAdded: %U")
        ("j" "Journal" entry (file+datetree "~/org/journal.org")
         "* %?\nEntered on %U\n  %i")))

;;(add-hook 'remember-mode-hook 'org-remember-apply-template)
;;(define-key global-map [(control meta ?r)] 'remember)
;; (custom-set-variables
;;  '(org-agenda-files (quote ("~/agenda.org" "~/work.org")))
;;  '(org-default-notes-file "~/notes.org")
;;  '(org-agenda-ndays 7)
;;  '(org-deadline-warning-days 14)
;;  '(org-agenda-show-all-dates t)
;;  '(org-agenda-skip-deadline-if-done t)
;;  '(org-agenda-skip-scheduled-if-done t)
;;  '(org-agenda-start-on-weekday nil)
;;  '(org-reverse-note-order t)
;;  '(org-fast-tag-selection-single-key (quote expert))
;;  '(org-agenda-custom-commands
;;    (quote (("d" todo "DELEGATED" nil)
;;            ("c" todo "DONE|DEFERRED|CANCELLED" nil)
;;            ("w" todo "WAITING" nil)
;;            ("W" agenda "" ((org-agenda-ndays 21)))
;;            ("A" agenda ""
;;             ((org-agenda-skip-function
;;               (lambda nil
;;                 (org-agenda-skip-entry-if (quote notregexp) "\\=.*\\[#A\\]")))
;;              (org-agenda-ndays 1)
;;              (org-agenda-overriding-header "Today's Priority #A tasks: ")))
;;            ("u" alltodo ""
;;             ((org-agenda-skip-function
;;               (lambda nil
;;                 (org-agenda-skip-entry-if (quote scheduled) (quote deadline)
;;                                           (quote regexp) "<[^>\n]+>")))
;;              (org-agenda-overriding-header "Unscheduled TODO entries: "))))))
;;  '(org-remember-store-without-prompt t)
;;  '(org-remember-templates
;;    (quote ((116 "* TODO %?\n  %u" "~/agenda.org" "Tasks")
;;            (110 "* %u %?" "~/notes.org" "Notes"))))
;;  '(remember-annotation-functions (quote (org-remember-annotation)))
;;  '(remember-handler-functions (quote (org-remember-handler))))
