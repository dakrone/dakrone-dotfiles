;;; magit-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (magit-status) "magit" "magit.el" (20483 39714))
;;; Generated autoloads from magit.el

(autoload 'magit-status "magit" "\
Open a Magit status buffer for the Git repository containing
DIR.  If DIR is not within a Git repository, offer to create a
Git repository in DIR.

Interactively, a prefix argument means to ask the user which Git
repository to use even if `default-directory' is under Git control.
Two prefix arguments means to ignore `magit-repo-dirs' when asking for
user input.

\(fn DIR)" t nil)

;;;***

;;;### (autoloads (magit-blame-mode) "magit-blame" "magit-blame.el"
;;;;;;  (20483 39714))
;;; Generated autoloads from magit-blame.el

(autoload 'magit-blame-mode "magit-blame" "\
Display blame information inline.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (turn-on-magit-stgit magit-stgit-mode) "magit-stgit"
;;;;;;  "magit-stgit.el" (20483 39714))
;;; Generated autoloads from magit-stgit.el

(autoload 'magit-stgit-mode "magit-stgit" "\
StGit support for Magit

\(fn &optional ARG)" t nil)

(autoload 'turn-on-magit-stgit "magit-stgit" "\
Unconditionally turn on `magit-stgit-mode'.

\(fn)" nil nil)

;;;***

;;;### (autoloads (turn-on-magit-svn magit-svn-mode) "magit-svn"
;;;;;;  "magit-svn.el" (20483 39714))
;;; Generated autoloads from magit-svn.el

(autoload 'magit-svn-mode "magit-svn" "\
SVN support for Magit

\(fn &optional ARG)" t nil)

(autoload 'turn-on-magit-svn "magit-svn" "\
Unconditionally turn on `magit-svn-mode'.

\(fn)" nil nil)

;;;***

;;;### (autoloads (turn-on-magit-topgit magit-topgit-mode) "magit-topgit"
;;;;;;  "magit-topgit.el" (20483 39714))
;;; Generated autoloads from magit-topgit.el

(autoload 'magit-topgit-mode "magit-topgit" "\
Topgit support for Magit

\(fn &optional ARG)" t nil)

(autoload 'turn-on-magit-topgit "magit-topgit" "\
Unconditionally turn on `magit-topgit-mode'.

\(fn)" nil nil)

;;;***

;;;### (autoloads (rebase-mode) "rebase-mode" "rebase-mode.el" (20483
;;;;;;  39714))
;;; Generated autoloads from rebase-mode.el

(autoload 'rebase-mode "rebase-mode" "\
Major mode for editing of a Git rebase file.

Rebase files are generated when you run 'git rebase -i' or run
`magit-interactive-rebase'.  They describe how Git should perform
the rebase.  See the documentation for git-rebase (e.g., by
running 'man git-rebase' at the command line) for details.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("git-rebase-todo" . rebase-mode))

;;;***

;;;### (autoloads nil nil ("magit-bisect.el" "magit-key-mode.el"
;;;;;;  "magit-pkg.el" "magit-wip.el") (20483 39714 837439))

;;;***

(provide 'magit-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; magit-autoloads.el ends here
