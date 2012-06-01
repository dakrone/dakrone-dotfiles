;; initalize all ELPA packages
(package-initialize)



;; ==== Imports ====
;; Gist support
(require 'gist)

;; Undo tree support
(require 'undo-tree)
(global-undo-tree-mode)

;; magic dired
;;(require 'dired-x)

;; smex
(smex-initialize)

;; dim parens
(require 'parenface)

;; org-mode
(add-to-list 'load-path (concat "~/.emacs.d/" (user-login-name) "/org-mode"))
(require 'org)

;; auto-complete
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)

;; go-to-char ('f' in vim)
;;(require 'iy-go-to-char)

;; minimap
;;(require 'minimap)

;; ace-jump-mode
;;(require 'ace-jump-mode)
;;(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

;; yaml-mode
;;(require 'yaml-mode)
(autoload 'yaml-mode "yaml-mode" "YAML Mode." t)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))

;; javap mode
;;(require 'javap-mode)
(autoload 'javap-mode "javap-mode" "Javap Mode." t)
(add-to-list 'auto-mode-alist '("\\.class$" . javap-mode))

;; kibit-mode
(add-to-list 'load-path (concat "~/.emacs.d/" (user-login-name) "/kibit-mode"))
(autoload 'kibit-mode "kibit-mode" "Kibit Mode." t)
(add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.cljs$" . clojure-mode))
(add-hook 'clojure-mode-hook 'kibit-mode)



;; ==== Backup files ====
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))



;; ==== Repos ====
;; Marmalade
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("tromey" . "http://tromey.com/elpa/") t)



;; ==== Vimish things ====
;; This code makes % act like the buffer name in the minibuffer, similar to Vim
(define-key minibuffer-local-map "%"
  (function
   (lambda ()
     (interactive)
     (insert (file-name-nondirectory
              (buffer-file-name
               (window-buffer (minibuffer-selected-window))))))))



;; ==== Unicode stuff ====
(set-language-environment "UTF-8")
(setq slime-net-coding-system 'utf-8-unix)



;; ==== Growl support on OSX ====
(defun growl-notification (title message &optional sticky)
  "Send a Growl notification"
  (do-applescript
   (format "tell application \"GrowlHelperApp\"
              notify with name \"Emacs Notification\" title \"%s\" description \"%s\" application name \"Emacs.app\" sticky %s
           end tell"
           title
           (replace-regexp-in-string "\"" "'" message)
           (if sticky "yes" "no"))))

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

(defun call-growl (matched-type nick msg)
  (let* ((nick (first (split-string nick "!"))))
    (growl-notification nick msg)))

;; only add the hook for Mac
(when (eq window-system 'ns)
  (add-hook 'erc-text-matched-hook 'call-growl))

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
           erc-hide-list '("JOIN"
                           "PART"
                           "QUIT"
                           ;;"NICK"
                           )
           erc-ignore-list '("sonian-chef")
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
           erc-ignore-list '("sonian-chef")
           erc-autojoin-channels-alist
           '(("freenode.net"
              "#clojure"
              "#clojerks"
              "#leiningen"))
           erc-prompt-for-nickserv-password nil)
     (require 'erc-services)
     (require 'erc-spelling)
     (and
      (require 'erc-highlight-nicknames)
      (add-to-list 'erc-modules 'highlight-nicknames)
      (erc-update-modules))
     (erc-services-mode 1)
     (add-to-list 'erc-modules 'highlight-nicknames 'spelling)
     (add-hook 'erc-connect-pre-hook (lambda (x) (erc-update-modules)))))

(setq erc-server-reconnect-timeout 5)
(setq erc-server-reconnect-attempts 4)



;; ==== Appearance ====
(set-default-font "Anonymous Pro")
(set-face-attribute 'default nil :height 115)
;; Anti-aliasing
(setq mac-allow-anti-aliasing t)

;; Put the column in the status bar
(column-number-mode)

;; Transparency
;;(set-frame-parameter (selected-frame) 'alpha '(100 35))
;;(add-to-list 'default-frame-alist '(alpha 100 35))

;; split the way I want
(setq split-height-threshold nil)

;; always turn whitespace mode on
(whitespace-mode t)
(add-hook 'clojure-mode-hook (lambda ()
                               (whitespace-mode t)
                               (subword-mode t)))
(add-hook 'prog-mode-hook (lambda ()
                            (whitespace-mode t)
                            (subword-mode t)
                            (idle-highlight t)))

;; Display the battery level in the bottom bar
(display-battery-mode t)

;; Show Paren Mode
(setq show-paren-style 'expression)
(add-hook 'clojure-mode-hook 'enable-show-paren-mode)
(defun enable-show-paren-mode ()
  (interactive)
  (show-paren-mode t))
;; (defun set-show-paren-face-background ()
;;   (set-face-background 'show-paren-match-face "#232323"))
(defun set-show-paren-face-background ()
  (set-face-background 'show-paren-match-face "#dddddd"))
(defun set-show-paren-face-background-dark ()
  (set-face-background 'show-paren-match-face "#232323"))

(add-hook 'show-paren-mode-hook 'set-show-paren-face-background-dark)

(remove-hook 'prog-mode-hook 'esk-turn-on-hl-line-mode)
;; I'm okay with pretty lambdas
(remove-hook 'prog-mode-hook 'esk-pretty-lambdas)
;; turn off ESK's idle-highlight mode
(remove-hook 'prog-mode-hook 'esk-turn-on-idle-highlight-mode)
;; no pretty fns
(remove-hook 'clojure-mode-hook 'esk-pretty-fn)

;; Fix the closing paren newline thing
(eval-after-load 'paredit
  '(define-key paredit-mode-map (kbd ")") 'paredit-close-parenthesis))



;; ==== IDO ====
(ido-mode t)
(setq ido-enable-flex-matching t)   ; enable fuzzy matching
(setq ido-execute-command-cache nil)
(defun ido-execute-command ()
  (interactive)
  (call-interactively
   (intern
    (ido-completing-read
     "M-x "
     (progn
       (unless ido-execute-command-cache
         (mapatoms (lambda (s)
                     (when (commandp s)
                       (setq ido-execute-command-cache
                             (cons (format "%S" s)
                                   ido-execute-command-cache))))))
       ido-execute-command-cache)))))

(add-hook 'ido-setup-hook
          (lambda ()
            (setq ido-enable-flex-matching t)
            (global-set-key "\M-x" 'ido-execute-command)))



;; ==== Fix ssh-agent ====
(defun find-agent ()
  (first (split-string
          (shell-command-to-string
           (concat
            "ls -t1 "
            "$(find /tmp/ -uid $UID -path \\*ssh\\* -type s 2> /dev/null)"
            "|"
            "head -1")))))

(defun fix-agent ()
  (interactive)
  (let ((agent (find-agent)))
    (setenv "SSH_AUTH_SOCK" agent)
    (message agent)))



;; ==== JSON formatting ====
(defun beautify-json ()
  (interactive)
  (let ((b (if mark-active (min (point) (mark)) (point-min)))
        (e (if mark-active (max (point) (mark)) (point-max))))
    (shell-command-on-region b e
                             "python -mjson.tool" (current-buffer) t)))

(global-set-key (kbd "C-c C-f") 'beautify-json)



;; ==== URL jumping ====
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
      (ffap-next-url t t))))

(global-set-key (kbd "C-c u") 'browse-last-url-in-brower)



;; ==== Ispell/Aspell flyspell stuff ====
;; brew install aspell --lang=en
(setq-default ispell-program-name "/usr/local/bin/aspell")
(setq ispell-extra-args '("--sug-mode=ultra" "--ignore=3"))
(setq flyspell-issue-message-flag nil)
(setq ispell-personal-dictionary "~/.flydict")



;; ==== Scpaste stuff ====
(setq scpaste-http-destination "http://p.writequit.org")
(setq scpaste-scp-destination "p.writequit.org:~/public_html/wq/paste/")



;; ==== Backup directory ====
(setq backup-directory-alist '(("." . "~/.backup")))



;; ==== copy-paste on Mac ====
(defun mac-copy ()
  (shell-command-to-string "pbpaste"))

(defun mac-paste (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(when (eq window-system 'ns)
  (scroll-bar-mode -1)
  (setq interprogram-cut-function 'mac-paste)
  (setq interprogram-paste-function 'mac-copy))



;; ==== path env stuff ====
(defun add-to-path (path-element)
  "Add the specified path element to the Emacs PATH"
  (interactive "DEnter directory to be added to path: ")
  (if (file-directory-p path-element)
      (setenv "PATH"
              (concat (expand-file-name path-element)
                      path-separator (getenv "PATH")))))

(add-to-path (concat "~/bin"))
(add-to-path "/usr/local/bin")



;; ==== Haskell mode ====
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)



;; ==== Emacs Client Setup ====
;; only start the server for the graphical one
(when (eq window-system 'ns)
  (server-start))



;; ==== ledger stuff ====
(add-to-list 'load-path (concat "~/.emacs.d/" (user-login-name) "/ledger"))
(autoload 'ledger-mode "ledger-mode" "Ledger Mode." t)
(add-to-list 'auto-mode-alist '("\\.dat$" . ledger-mode))
;;(require 'ledger)
(setq ledger-binary-path "/usr/local/bin/ledger")
(setenv "LEDGER_FILE" "/Users/hinmanm/data/ledger.dat")



;; ==== Ido stuff ====
;; from http://www.emacswiki.org/emacs/InteractivelyDoThings#toc13
;; Make Ido complete almost anything (except the stuff where it shouldn't)

;; Don't add recent buffers to the ido-list
(setq ido-use-virtual-buffers nil)

(defvar ido-enable-replace-completing-read t
  "If t, use ido-completing-read instead of completing-read if possible.

    Set it to nil using let in around-advice for functions where the
    original completing-read is required.  For example, if a function
    foo absolutely must use the original completing-read, define some
    advice like this:

    (defadvice foo (around original-completing-read-only activate)
      (let (ido-enable-replace-completing-read) ad-do-it))")

;; Replace completing-read wherever possible, unless directed otherwise
;; (defadvice completing-read
;;   (around use-ido-when-possible activate)
;;   (if (or (not ido-enable-replace-completing-read) ; Manual override disable ido
;;           (and (boundp 'ido-cur-list)
;;                ido-cur-list)) ; Avoid infinite loop from ido calling this
;;       ad-do-it
;;     (let ((allcomp (all-completions "" collection predicate)))
;;       (if allcomp
;;           (setq ad-return-value
;;                 (ido-completing-read prompt
;;                                      allcomp
;;                                      nil require-match initial-input hist def))
;;         ad-do-it))))



;; ==== stuff copied from ESK ====
(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (mouse-wheel-mode t)
  (blink-cursor-mode -1))

(ansi-color-for-comint-mode-on)

(setq visible-bell t
      inhibit-startup-message t
      color-theme-is-global t
      shift-select-mode nil
      mouse-yank-at-point t
      x-select-enable-clipboard t
      require-final-newline t ; crontabs break without this
      truncate-partial-width-windows nil
      uniquify-buffer-name-style 'forward
      whitespace-style '(face trailing lines-tail tabs)
      whitespace-line-column 80
      ediff-window-setup-function 'ediff-setup-windows-plain
      oddmuse-directory "~/.emacs.d/oddmuse"
      save-place-file "~/.emacs.d/places")

(add-to-list 'safe-local-variable-values '(lexical-binding . t))
(add-to-list 'safe-local-variable-values '(whitespace-line-column . 80))

;; Save a list of recent files visited.
(recentf-mode 1)

;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)

;; ido-mode is like magic pixie dust!
(when (functionp 'ido-mode)
  (ido-mode t)
  (setq ido-enable-prefix nil
        ido-enable-flex-matching t
        ido-auto-merge-work-directories-length nil
        ido-create-new-buffer 'always
        ido-use-filename-at-point 'guess
        ido-max-prospects 10))

(set-default 'indent-tabs-mode nil)
(set-default 'indicate-empty-lines t)
(set-default 'imenu-auto-rescan t)

(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook 'turn-on-flyspell)

(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'auto-tail-revert-mode 'tail-mode)

(random t) ;; Seed the random-number generator

;; Hippie expand: at times perhaps too hip
(dolist (f '(try-expand-line try-expand-list try-complete-file-name-partially))
  (delete f hippie-expand-try-functions-list))

;; Associate modes with file extensions

(add-to-list 'auto-mode-alist '("COMMIT_EDITMSG$" . diff-mode))
(add-to-list 'auto-mode-alist '("\\.ya?ml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.xml$" . nxml-mode))

(eval-after-load 'grep
  '(when (boundp 'grep-find-ignored-files)
     (add-to-list 'grep-find-ignored-files "*.class")))

;; Default to unified diffs
(setq diff-switches "-u")

;; Rake files are ruby, too, as are gemspecs, rackup files, etc.
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Vagrantfile$" . ruby-mode))

;; We never want to edit Rubinius bytecode
(add-to-list 'completion-ignored-extensions ".rbc")

;; A monkeypatch to cause annotate to ignore whitespace
(defun vc-git-annotate-command (file buf &optional rev)
  (let ((name (file-relative-name file)))
    (vc-git-command buf 0 name "blame" "-w" rev)))

(defun sudo-edit (&optional arg)
  (interactive "p")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:" (ido-read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(font-lock-add-keywords
 nil '(("\\<\\(FIX\\|TODO\\|FIXME\\|HACK\\|REFACTOR\\|NOCOMMIT\\)"
        1 font-lock-warning-face t)))

(add-to-list 'auto-mode-alist '("\\.zsh$" . shell-script-mode))

;; tool-bar-mode, Y U NO EXIST ALL THE TIME??
(when (eq window-system 'ns)
  (tool-bar-mode -1))
(menu-bar-mode -1)

;; Cosmetics for diffs, also contained as part of color-theme-dakrone.el
(eval-after-load 'diff-mode
  '(progn
     (set-face-foreground 'diff-added "green4")
     (set-face-background 'diff-added "gray10")
     (set-face-foreground 'diff-removed "red3")
     (set-face-background 'diff-removed "gray10")))

(eval-after-load 'magit
  '(progn
     (set-face-foreground 'magit-diff-add "green3")
     (set-face-background 'magit-diff-add "gray10")
     (set-face-foreground 'magit-diff-del "red3")
     (set-face-background 'magit-diff-del "gray10")
     (set-face-background 'magit-diff-file-header "gray10")
     (set-face-background 'magit-diff-hunk-header "gray10")))


(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (interactive)
  (indent-buffer)
  (untabify-buffer)
  (delete-trailing-whitespace))

;; Make rgrep actually work
(setq ido-ubiquitous-enabled nil)
;; Require a newline at the end
(setq require-final-newline t)




;; Last thing
;; You can keep system- or user-specific customizations here
(setq system-config (concat user-emacs-directory system-name ".el")
      user-config (concat user-emacs-directory user-login-name ".el")
      user-dir (concat user-emacs-directory user-login-name))

(add-to-list 'load-path user-dir)

(when (file-exists-p system-config) (load system-config))
(when (file-exists-p user-config) (load user-config))
(when (file-exists-p user-dir)
  (dolist (l (directory-files user-dir nil "^[^#].*el$"))
    (load l)))

;; My custom theme
;;(require 'color-theme-zenburn)
;; (if (eq window-system 'ns)
;;     (color-theme-dakrone)
;;   ;;(color-theme-zenburn)
;;   )
(if (eq window-system 'ns)
    (set-face-background 'default "gray10"))
(color-theme-dakrone)
;;(set-face-foreground 'paren-face "DimGrey")



;; ==== Eclim stuff ====
;; (add-to-list 'load-path (expand-file-name "~/src/elisp/emacs-eclim"))
;; only add the vendor path when you want to use the libraries
;; provided with emacs-eclim
;; (add-to-list 'load-path (expand-file-name "~/src/elisp/emacs-eclim/vendor"))
;; (require 'eclim)

;; (setq eclim-auto-save t)
;; (global-eclim-mode)

;; (setq help-at-pt-display-when-idle t)
;; (setq help-at-pt-timer-delay 0.1)
;; (help-at-pt-set-timer)

;; regular auto-complete initialization
;; (require 'auto-complete-config)
;; (ac-config-default)

;; add the emacs-eclim source
;; (require 'ac-emacs-eclim-source)
;; (add-hook 'eclim-mode-hook
;;           (lambda () (add-to-list 'ac-sources 'ac-source-emacs-eclim)))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(magit-set-upstream-on-push (quote dontask)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
