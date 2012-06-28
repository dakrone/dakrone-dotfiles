;;; iy-go-to-char-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (iy-go-to-char-continue-backward iy-go-to-char-continue
;;;;;;  iy-go-to-char-backward iy-go-to-char) "iy-go-to-char" "iy-go-to-char.el"
;;;;;;  (20460 44706))
;;; Generated autoloads from iy-go-to-char.el

(autoload 'iy-go-to-char "iy-go-to-char" "\
Move forward to Nth occurence of CHAR.

Typing key of CHAR will move to the next occurence of CHAR.
Typing ; will move to the next occurence of CHAR.
Typing , will move to the previous occurence of CHAR.
Typing C-g will quit and return to the original point.
Typing C-s or C-r will start `isearch` using CHAR.
Typing C-w or M-w will kill/copy between current point and the start point.
Unless quit using C-g or the region is activated before searching, the start
 point is set as mark.

\(fn N CHAR)" t nil)

(autoload 'iy-go-to-char-backward "iy-go-to-char" "\
Move backward to Nth occurence of CHAR.
Typing key of CHAR will move to the previous occurence of CHAR.
Typing ; will move to the next occurence of CHAR.
Typing , will move to the previous occurence of CHAR.
Typing C-g will quit and return to the original point.
Typing C-s or C-r will start `isearch` using CHAR

\(fn N CHAR)" t nil)

(autoload 'iy-go-to-char-continue "iy-go-to-char" "\
Continue last `iy-go-to-char` or `iy-go-to-char-backward`

\(fn N)" t nil)

(autoload 'iy-go-to-char-continue-backward "iy-go-to-char" "\
Continue last `iy-go-to-char` or `iy-go-to-char-backward`

\(fn N)" t nil)

;;;***

;;;### (autoloads nil nil ("iy-go-to-char-pkg.el") (20460 44706 306809))

;;;***

(provide 'iy-go-to-char-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; iy-go-to-char-autoloads.el ends here
