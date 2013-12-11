;; use cl functions and macros in my config files.
(require 'cl)

;; decide system is MacOSX
(defun macosx-p ()
  (eq system-type 'darwin))

(defun linux-p ()
  (eq system-type 'gnu/linux))

;; I use notifications on linux
(when (linux-p)
  (require 'notifications))

;; A little something for https://github.com/railwaycat/emacs-mac-port
(when (eq window-system 'mac)
  (setq mac-option-modifier 'meta)
  (setq mac-command-modifier 'hyper)
  (global-set-key [(hyper a)] 'mark-whole-buffer)
  (global-set-key [(hyper v)] 'yank)
  (global-set-key [(hyper c)] 'kill-ring-save)
  (global-set-key [(hyper s)] 'save-buffer)
  (global-set-key [(hyper l)] 'goto-line)
  (global-set-key [(hyper w)]
                  (lambda () (interactive) (delete-window)))
  (global-set-key [(hyper z)] 'undo))

;; Set (dark|light) background, theming will check this in choose the
;; appropriate theme.
;;(defvar background 'light)
(defvar background 'dark)

;; brew install coreutils
(when (macosx-p)
  (setq insert-directory-program "gls")
  (setq dired-listing-switches "-aBhl --group-directories-first"))
