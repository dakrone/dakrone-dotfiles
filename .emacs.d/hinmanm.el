;; Imports
(require 'color-theme)
(require 'offlineimap)
;; Dim parens
(require 'parenface)
;; Gist support
(require 'gist)



;; Clojure stuff
(eval-after-load 'slime '(setq slime-protocol-version 'ignore))

(defun lisp-enable-paredit-hook () (paredit-mode 1))
(add-hook 'clojure-mode-hook 'lisp-enable-paredit-hook)

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
(defclojureface clojure-double-quote "#1BF21B"   "Clojure special"
  (:background "unspecified"))

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

(add-hook 'clojure-mode-hook 'tweak-clojure-syntax)

;; One-offs
;; Better indention (from Kevin)
(add-hook 'clojure-mode-hook
          (lambda () (setq clojure-mode-use-backtracking-indent t)))
;; paredit in REPL
;;(add-hook 'slime-repl-mode-hook (lambda () (paredit-mode +1)))
;; syntax in REPL
(add-hook 'slime-repl-mode-hook 'clojure-mode-font-lock-setup)
;; turn off the ESK highlight line mode
(remove-hook 'esk-coding-hook 'esk-turn-on-hl-line-mode)
(remove-hook 'esk-coding-hook 'esk-pretty-lambdas)
(add-hook 'esk-coding-hook 'esk-turn-on-whitespace)
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


;; Repos
;; Add Phil's ELPA repo to the list
(add-to-list 'package-archives
             '("technomancy" . "http://repo.technomancy.us/emacs/") t)
;; Marmalade
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages"))



;; This code makes % act like the buffer name in the minibuffer, similar to Vim
(define-key minibuffer-local-map "%"
  (function
   (lambda ()
     (interactive)
     (insert (file-name-nondirectory
              (buffer-file-name
               (window-buffer (minibuffer-selected-window))))))))



;; Unicode stuff
(set-language-environment "UTF-8")
(setq slime-net-coding-system 'utf-8-unix)



;; Growl support on OSX
(defun bja-growl-notification (title message &optional sticky)
  "Send a Growl notification"
  (do-applescript
   (format "tell application \"GrowlHelperApp\"
              notify with name \"Emacs Notification\" title \"%s\" description \"%s\" application name \"Emacs.app\" sticky %s
           end tell"
           title
           (replace-regexp-in-string "\"" "'" message)
           (if sticky "yes" "no"))))



;; Put the column in the status bar
(column-number-mode)



;; ERC stuff
;; Only track my nick(s)
(defadvice erc-track-find-face
  (around erc-track-find-face-promote-query activate)
  (if (erc-query-buffer-p)
      (setq ad-return-value (intern "erc-current-nick-face"))
    ad-do-it))

(setq erc-keywords '("dakrone"
                     "dakrone_"
                     "dakrone__"))
(setq erc-hide-list '("JOIN" "PART" "QUIT"))

;;(setq tls-program '("openssl s_client -connect %h:%p"))

(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                "324" "329" "332" "333" "353" "477"))

;; (defun clean-message (s)
;;   (setq s (replace-regexp-in-string "'" "&apos;"
;;   (replace-regexp-in-string "\"" "&quot;"
;;   (replace-regexp-in-string "&" "&"
;;   (replace-regexp-in-string "<" "&lt;"
;;   (replace-regexp-in-string ">" "&gt;" s)))))))

;; (defun call-growl (matched-type nick msg)
;;   (let* ((cmsg  (split-string (clean-message msg)))
;;         (nick   (first (split-string nick "!")))
;;         (msg    (mapconcat 'identity (rest cmsg) " ")))
;;     (bja-growl-notification nick msg)))

(defun call-growl (matched-type nick msg)
  (let* ((nick (first (split-string nick "!"))))
    (bja-growl-notification nick msg)))

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

;; Enable ERC logging
(setq erc-log-channels-directory "~/.erc/logs/")
(setq erc-save-buffer-on-part t)
(defadvice save-buffers-kill-emacs (before save-logs (arg) activate)
  (save-some-buffers t (lambda () (when (eq major-mode 'erc-mode) t))))

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
                      "leathekd" "dave_chestnutt" "davec" "mikehale" "mwilliams"
                      "rhickey" "geek00l" "wooby" "zkim" "TeXnomancy" "steve"
                      "davec" "imotov" "portertech")
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

(add-hook 'erc-mode-hook (lambda () (flyspell-mode t)))



;; Appearance
;;(set-default-font "Monaco")
(set-default-font "Anonymous Pro")
(set-face-attribute 'default nil :height 115)
;; Anti-aliasing
(setq mac-allow-anti-aliasing t)
;;(setq mac-allow-anti-aliasing nil)

;; Transparency
;;(set-frame-parameter (selected-frame) 'alpha '(100 35))
;;(add-to-list 'default-frame-alist '(alpha 100 35))

;; Fullscreen
(when (eq window-system 'ns)
  (defun toggle-fullscreen () (interactive) (ns-toggle-fullscreen))
  (ns-toggle-fullscreen)
  (global-set-key [f11] 'toggle-fullscreen))

;; Color Theme
(color-theme-initialize)
;; My custom theme
(color-theme-dakrone)


;; Show Paren Mode
(setq show-paren-style 'expression)
(add-hook 'clojure-mode-hook 'enable-show-paren-mode)
(defun enable-show-paren-mode ()
  (interactive)
  (show-paren-mode t))
(defun set-show-paren-face-background ()
  (set-face-background 'show-paren-match-face "#333333"))
(add-hook 'show-paren-mode-hook 'set-show-paren-face-background)



;; Magit
(require 'magit)



;; Window switching
(global-set-key [C-tab] 'other-window)
(global-set-key [C-S-tab]
                (lambda ()
                  (interactive)
                  (other-window -1)))



;; Undo tree support
(require 'undo-tree)
(global-undo-tree-mode)



;; IDO
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



;; Unicode stuff
(set-language-environment "UTF-8")
(setq slime-net-coding-system 'utf-8-unix)



;; Change color for background highlight
;; I don't like hl-line-mode
(remove-hook 'coding-hook 'turn-on-hl-line-mode)
;;(set-face-background 'hl-line "#333")



;; Fix the closing paren newline thing
(eval-after-load 'paredit
  '(define-key paredit-mode-map (kbd ")") 'paredit-close-parenthesis))



;; Fix ssh-agent
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



;; Auto-complete (1.3.1)
(add-to-list 'load-path (concat "~/.emacs.d/"
                                (user-login-name)
                                "/auto-complete"))
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories (concat "~/.emacs.d/"
                                                (user-login-name)
                                                "/auto-complete/ac-dict"))
(ac-config-default)



;; Ispell/Aspell flyspell stuff
;; brew install aspell --lang=en
(setq-default ispell-program-name "/usr/local/bin/aspell")
(setq ispell-extra-args '("--sug-mode=ultra" "--ignore=3"))
(setq flyspell-issue-message-flag nil)
(setq ispell-personal-dictionary "~/.flydict")



;; Scpaste stuff
(setq scpaste-http-destination "http://p.writequit.org")
(setq scpaste-scp-destination "p.writequit.org:~/public_html/wq/paste/")



;; Backup directory
(setq backup-directory-alist '(("." . "~/.backup")))



;; copy-paste on Mac
(defun mac-copy ()
(shell-command-to-string "pbpaste"))

(defun mac-paste (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(setq interprogram-cut-function 'mac-paste)
(setq interprogram-paste-function 'mac-copy)



;; path env stuff
(defun add-to-path (path-element)
  "Add the specified path element to the Emacs PATH"
 (interactive "DEnter directory to be added to path: ")
 (if (file-directory-p path-element)
     (setenv "PATH"
             (concat (expand-file-name path-element)
                     path-separator (getenv "PATH")))))

(add-to-path "/Users/hinmanm/bin")



;; Org Mode
;; see ~/.emacs.d/hinmanm/org-stuff.el



;; Haskell mode
(load (concat "~/.emacs.d/"
              (user-login-name)
              "/haskell-mode-2.8.0/haskell-site-file"))
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)



;; M-n, M-p
;; Twitter?
(defun scroll-down-keep-cursor ()
   ;; Scroll the text one line down while keeping the cursor
   (interactive)
   (scroll-down 1))

(defun scroll-up-keep-cursor ()
   ;; Scroll the text one line up while keeping the cursor
   (interactive)
   (scroll-up 1))

(global-set-key (kbd "M-n") 'scroll-down-keep-cursor)
(global-set-key (kbd "M-p") 'scroll-up-keep-cursor)





;; Emacs Client Setup
;; this should always be last
(server-start)



;; Dvorak niceity
(when (eq window-system 'ns)
  (define-key key-translation-map "\C-t" "\C-x"))


