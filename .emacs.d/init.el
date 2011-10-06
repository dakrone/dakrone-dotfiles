;; initalize all ELPA packages
(package-initialize)



;; ==== Imports ====
;; Gist support
(require 'gist)
;; Undo tree support
(require 'undo-tree)
(global-undo-tree-mode)
;; magic dired
(require 'dired-x)
;; smex
(smex-initialize)
;; dim parens
(require 'parenface)
;; notmuch
;; (add-to-list 'load-path (concat "~/.emacs.d/" (user-login-name) "/notmuch"))
;; (require 'notmuch)



;; ==== Repos ====
;; Marmalade
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))



;; ==== Clojure stuff ====
(eval-after-load 'slime '(setq slime-protocol-version 'ignore))
(add-to-list 'auto-mode-alist '("\\.cljs$" . clojure-mode))

(defun lisp-enable-paredit-hook () (paredit-mode 1))
(add-hook 'clojure-mode-hook 'lisp-enable-paredit-hook)
(add-hook 'lisp-mode-hook 'lisp-enable-paredit-hook)

(defmacro defclojureface (name color desc &optional others)
  `(defface
     ,name '((((class color)) (:foreground ,color ,@others)))
     ,desc :group 'faces))

(defclojureface clojure-parens       "DimGrey"   "Clojure parens")
(defclojureface clojure-braces       "DimGrey"   "Clojure braces")
(defclojureface clojure-brackets     "SteelBlue" "Clojure brackets")
(defclojureface clojure-keyword      "#729FCF"   "Clojure keywords")
(defclojureface clojure-namespace    "#c476f1"   "Clojure namespace")
(defclojureface clojure-java-call    "#729FCF"   "Clojure Java calls")
(defclojureface clojure-special      "#1BF21B"   "Clojure special")
(defclojureface clojure-double-quote "#1BF21B"   "Clojure special")

(defun tweak-clojure-syntax ()
  (mapcar (lambda (x) (font-lock-add-keywords nil x))
          '((("#?['`]*(\\|)"       . 'clojure-parens))
            (("#?\\^?{\\|}"        . 'clojure-brackets))
            (("\\[\\|\\]"          . 'clojure-braces))
            ((":\\w+"              . 'clojure-keyword))
            (("#?\""               0 'clojure-double-quote prepend))
            (("nil\\|true\\|false\\|%[1-9]?" . 'clojure-special))
            (("(\\(\\.[^ \n)]*\\|[^ \n)]+\\.\\|new\\)\\([ )\n]\\|$\\)" 1
              'clojure-java-call)))))

(when (eq window-system 'ns)
  (add-hook 'clojure-mode-hook 'tweak-clojure-syntax))

;; Better indention (from Kevin)
(add-hook 'clojure-mode-hook
          (lambda () (setq clojure-mode-use-backtracking-indent t)))

;; syntax in REPL
(add-hook 'slime-repl-mode-hook 'clojure-mode-font-lock-setup)

;; Lazytest indention in clojure
(eval-after-load 'clojure-mode
  '(define-clojure-indent
     (describe 'defun)
     (testing 'defun)
     (given 'defun)
     (it 'defun)
     (do-it 'defun)))

;; compile faster
(setq font-lock-verbose nil)

;; slamhound support https://github.com/technomancy/slamhound
(defun slamhound ()
  (interactive)
  (goto-char (point-min))
  (kill-sexp)
  (insert (first (slime-eval `(swank:eval-and-grab-output
                               (format "(do (require 'slam.hound)
                                          (slam.hound/reconstruct \"%s\"))"
                                       ,buffer-file-name))))))



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



;; ==== Put the column in the status bar ====
(column-number-mode)



;; ==== ERC stuff ====
;; Only track my nick(s)
(defadvice erc-track-find-face
  (around erc-track-find-face-promote-query activate)
  (if (erc-query-buffer-p)
      (setq ad-return-value (intern "erc-current-nick-face"))
    ad-do-it))

(setq erc-keywords '("dakrone"
                     "dakrone_"
                     "dakrone__"))
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

(and
 (require 'erc-highlight-nicknames)
 (add-to-list 'erc-modules 'highlight-nicknames)
 (erc-update-modules))

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
           erc-hide-list '("JOIN"
                           "PART"
                           "QUIT"
                           ;;"NICK"
                           )
           erc-track-exclude-types (append '("324" "329" "332" "333"
                                             "353" "477" "MODE")
                                           erc-hide-list)
           erc-nick '("dakrone" "dakrone_")
           erc-autojoin-timing :ident
           erc-flood-protect nil
           erc-pals '("technomancy" "hiredman" "danlarkin" "drewr" "pjstadig"
                      "scgilardi" "dysinger" "fujin" "joegallo" "jimduey"
                      "leathekd" "dave_chestnutt" "davec" "mikehale" "decklin"
                      "rhickey" "geek00l" "wooby" "zkim" "TeXnomancy" "steve"
                      "davec" "imotov" "portertech" "joekinsella"
                      "joshpasqualetto" "josh" "justin" "scooper")
           erc-autojoin-channels-alist
           '(("freenode.net"
              "#clojure"
              "#leiningen"
              "#rawpacket"))
           erc-prompt-for-nickserv-password nil)
     (require 'erc-services)
     (require 'erc-spelling)
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

;; Transparency
;;(set-frame-parameter (selected-frame) 'alpha '(100 35))
;;(add-to-list 'default-frame-alist '(alpha 100 35))

;; split the way I want
(setq split-height-threshold nil)

;; always turn whitespace mode on
(whitespace-mode t)
(add-hook 'clojure-mode-hook (lambda () (whitespace-mode t)))
(add-hook 'prog-mode-hook (lambda () (whitespace-mode t)))

;; Color Theme
;;(color-theme-initialize)
;; My custom theme
;;(color-theme-dakrone)

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

;; highlights
(add-hook 'prog-mode-hook (lambda () (idle-highlight t)))

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
(require 'ledger)
(setq ledger-binary-path "/usr/local/bin/ledger")
(setenv "LEDGER_FILE" "/Users/hinmanm/data/ledger.dat")



;; ==== Ido stuff ====
;; from http://www.emacswiki.org/emacs/InteractivelyDoThings#toc13
;; Make Ido complete almost anything (except the stuff where it shouldn't)

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

(tool-bar-mode -1)
(menu-bar-mode -1)

;; Cosmetics for diffs
(eval-after-load 'diff-mode
  '(progn
     (set-face-foreground 'diff-added "green4")
     (set-face-foreground 'diff-removed "red3")))

(eval-after-load 'magit
  '(progn
     (set-face-foreground 'magit-diff-add "green3")
     (set-face-foreground 'magit-diff-del "red3")))


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
(if (eq window-system 'ns)
    (color-theme-dakrone)
  ;;(color-theme-zenburn)
  )
(color-theme-dakrone)
;;(set-face-foreground 'paren-face "DimGrey")
