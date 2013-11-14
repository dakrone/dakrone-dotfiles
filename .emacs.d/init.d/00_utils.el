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
