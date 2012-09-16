;;; ace-jump-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (ace-jump-mode) "ace-jump-mode" "ace-jump-mode.el"
;;;;;;  (20483 39729))
;;; Generated autoloads from ace-jump-mode.el

(autoload 'ace-jump-mode "ace-jump-mode" "\
AceJump mode is a minor mode for you to quick jump to a
position in the curret view.
   There is three submode now:
     `ace-jump-char-mode'
     `ace-jump-word-mode'
     `ace-jump-line-mode'

You can specify the sequence about which mode should enter
by customize `ace-jump-mode-submode-list'.

If you do not want to query char for word mode, you can change
`ace-jump-word-mode-use-query-char' to nil.

If you don't like the default move keys, you can change it by
setting `ace-jump-mode-move-keys'.

You can constrol whether use the case sensitive via
`ace-jump-mode-case-fold'.

\(fn &optional PREFIX)" t nil)

;;;***

;;;### (autoloads nil nil ("ace-jump-mode-pkg.el") (20483 39729 362906))

;;;***

(provide 'ace-jump-mode-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ace-jump-mode-autoloads.el ends here
