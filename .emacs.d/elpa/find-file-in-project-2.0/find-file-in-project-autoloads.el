;;; find-file-in-project-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (ffip-project-root find-file-in-project) "find-file-in-project"
;;;;;;  "find-file-in-project.el" (19344 46174))
;;; Generated autoloads from find-file-in-project.el

(autoload 'find-file-in-project "find-file-in-project" "\
Prompt with a completing list of all files in the project to find one.

The project's scope is defined as the first directory containing
an `.emacs-project' file. You can override this by locally
setting the `ffip-project-root' variable.

\(fn)" t nil)

(autoload 'ffip-project-root "find-file-in-project" "\
Find the root of the project defined by presence of `.emacs-project'.

\(fn &optional DIR)" nil nil)

;;;***

;;;### (autoloads nil nil ("find-file-in-project-pkg.el") (19344
;;;;;;  46174 546821))

;;;***

(provide 'find-file-in-project-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; find-file-in-project-autoloads.el ends here
