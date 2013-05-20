;;; minimap.el --- Minimap sidebar for Emacs

;; Copyright (C) 2011 Dustin Lacewell

;; Authors: Dustin Lacewell <dlacewell@gmail.com>
;;          David Engster <dengste@eml.cc>
;; Keywords: minimap
;; Version: 0.1

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Heavily based off minimap.el by David Engster however I
;; have refactored it a bit to support only a single global
;; minimap window. Usually this should be opened on startup
;; when there is a single window. Some defaults have also
;; been changed.

;; This file is an implementation of a minimap sidebar, i.e., a
;; smaller display of the current buffer on the left side. It
;; highlights the currently shown region and updates its position
;; automatically. You can navigate in the minibar by dragging the
;; active region with the mouse, which will scroll the corresponding
;; edit buffer.

;; Usage:
;;  * Put minimap.el in your load path.
;;  * (require 'minimap)
;;  * Use 'M-x minimap-toggle' to toggle the minimap.
;;  * Use 'M-x minimap-kill' to create the minimap.
;;  * Use 'M-x minimap-kill' to kill the minimap.
;;  * Use 'M-x customize-group RET minimap RET' to adapt minimap to your needs.

;;; Customizable variables;

(defgroup minimap nil
  "A minimaap sidebar for Emacs."
  :group 'convenience)

(defface minimap-font-face
  '((default :family "DejaVu Sans Mono" :height 30))
  "Face used for text in minimap buffer, notably the font family and height.
This height should be really small.  You probably want to use a
TrueType font for this.  After changing this, you should
recreate the minimap to avoid problems with recentering."
  :group 'minimap)

(defface minimap-active-region-background
  '((((background dark)) (:background "#4517305D0000"))
    (t (:background "#C847D8FEFFFF")))
  "Face for the active region in the minimap.
By default, this is only a different background color."
  :group 'minimap)

(defface minimap-semantic-function-face
  '((((background dark))
     (:box (:line-width 1 :color "white")
           :inherit (font-lock-function-name-face minimap-font-face)
           :height 1.9 :background "gray10"))
    (t (:box (:line-width 1 :color "black")
             :inherit (font-lock-function-name-face minimap-font-face)
             :height 1.9 :background "gray90")))
  "Face used for functions in the semantic overlay.")

(defface minimap-semantic-variable-face
  '((((background dark))
     (:box (:line-width 1 :color "white")
           :inherit (font-lock-variable-name-face minimap-font-face)
           :height 1.9 :background "gray10"))
    (t (:box (:line-width 1 :color "black")
             :inherit (font-lock-function-name-face minimap-font-face)
             :height 1.9 :background "gray90")))
  "Face used for variables in the semantic overlay.")

(defface minimap-semantic-type-face
  '((((background dark))
     (:box (:line-width 1 :color "white")
           :inherit (font-lock-type-face minimap-font-face)
           :height 1.9 :background "gray10"))
    (t (:box (:line-width 1 :color "black")
             :inherit (font-lock-function-name-face minimap-font-face)
             :height 1.9 :background "gray90")))
  "Face used for types in the semantic overlay.")

(defcustom minimap-dedicated-window t
  "Whether the minimap should create a dedicated window."
  :type 'boolean
  :group 'minimap)

(defcustom minimap-width-fraction 0.1
  "Fraction of width which should be used for minimap sidebar."
  :type 'number
  :group 'minimap)

(defcustom minimap-buffer-name-prefix "*MINIMAP*"
  "Prefix for buffer names of minimap sidebar."
  :type 'string
  :group 'minimap)

(defcustom minimap-update-delay 0.2
  "Delay in seconds after which sidebar gets updated.
Setting this to 0 will let the minimap react immediately, but
this will slow down scrolling."
  :type 'number
  :set (lambda (sym value)
         (set sym value)
         (when (and (boundp 'minimap-timer-object)
                    minimap-timer-object)
           (cancel-timer minimap-timer-object)
           (setq minimap-timer-object
                 (run-with-idle-timer
                  minimap-update-delay t 'minimap-update))))
  :group 'minimap)

(defcustom minimap-always-recenter nil
  "Whether minimap sidebar should be recentered after every point movement."
  :type 'boolean
  :group 'minimap)

(defcustom minimap-recenter-type 'middle
  "Specifies the type of recentering the minimap should use.
The minimap can use different types of recentering, i.e., how the
minimap should behave when you scroll in the main window or when
you drag the active region with the mouse.  The following
explanations will probably not help much, so simply try them and
choose the one which suits you best.

`middle' -- The active region will stay fixed in the middle of
the minimap. This the default.

`free' -- The position will be more or less free.  When dragging
the active region, the minimap will scroll when you reach the
bottom or top.

`relative' -- The position of the active region in the minimap
corresponds with the relative position of this region in the
buffer."

  :type '(choice (const :tag "Relative" relative)
                 (const :tag "Middle" middle)
                 (const :tag "Free" free))
  :group 'minimap)

(defcustom minimap-hide-scroll-bar t
  "Whether the minimap should hide the vertical scrollbar."
  :type 'boolean
  :group 'minimap)

(defcustom minimap-hide-fringes t
  "Whether the minimap should hide the fringes."
  :type 'boolean
  :group 'minimap)

(defcustom minimap-display-semantic-overlays t
  "Display overlays from CEDET's semantic analyzer.
If you use CEDET and the buffer's major-mode is supported, the
minimap can display overlays generated by the semantic analyzer.
By default, it will apply the faces `minimap-semantic-<X>-face',
with <X> being \"function\", \"variable\" and \"type\".  Also, it
will display the name of the tag in the middle of the overlay in
the corresponding font-lock face.

See also `minimap-enlarge-certain-faces', which can be used as
fallback."
  :type 'boolean
  :group 'minimap)

(defcustom minimap-enlarge-certain-faces 'as-fallback
  "Whether certain faces should be enlarged in the minimap.
All faces listed in `minimap-normal-height-faces' will be
displayed using the default font height, allowing you to still
read text using those faces.  By default, this should enlarge all
function names in the minimap, given you have font locking
enabled.  This variable can have the following values:

'as-fallback (the default) -- The feature will only be activated
  if information from CEDET's semantic analyzer isn't available
  (see: `minimap-display-semantic-overlays').
'always -- Always active.
nil -- Inactive."
  :type '(choice (const :tag "Fallback if CEDET unavailable." 'as-fallback)
                 (const :tag "Always active." 'always)
                 (const :tag "Inactive." nil))
  :group 'minimap)

(defcustom minimap-normal-height-faces '(font-lock-function-name-face)
  "List of faces which should be displayed with normal height.
When `minimap-enlarge-certain-faces' is non-nil, all faces in
this list will be displayed using the default font height.  By
default, this list contains `font-lock-function-name-face', so
you can still read function names in the minimap."
  :type '(repeat face)
  :group 'minimap)

(defcustom minimap-sync-overlay-properties '(face invisible)
  "Specifies which overlay properties should be synced.
Unlike text properties, overlays are not applied automatically to
the minimap and must be explicitly synced.  This variable
specifies which overlay properties should be synced by
`minimap-sync-overlays'.  Most importantly, this variable should
include 'invisible', so that hidden text does not appear in the
minimap buffer."
  :type '(repeat symbol)
  :group 'minimap)

;;; Internal variables

(defvar minimap-start nil)
(defvar minimap-end nil)
(defvar minimap-active-overlay nil)
(defvar minimap-window nil)
(defvar minimap-timer-object nil)
(defvar minimap-base-overlay nil)
(defvar minimap-numlines nil)
(defvar minimap-pointmin-overlay nil)

(make-variable-buffer-local 'minimap-start)
(make-variable-buffer-local 'minimap-end)
(make-variable-buffer-local 'minimap-active-overlay)
(make-variable-buffer-local 'minimap-base-overlay)
(make-variable-buffer-local 'minimap-numlines)
(make-variable-buffer-local 'minimap-pointmin-overlay)

(defun minimap-toggle ()
  "Toggle the minimap."
  (interactive)
  (if (and minimap-window
           (window-live-p minimap-window))
      (minimap-kill)
    (minimap-create)))

(defun minimap-buffer-name()
  "Get minimap buffer name for current buffer"
  (concat minimap-buffer-name-prefix " " (buffer-name (current-buffer))))

;;;###autoload
(defun minimap-create()
  "Create the minimap sidebar"
  (interactive)
  (let ((was_created)
        (current_buffer (current-buffer))
        (raw_buffer_name (buffer-name (current-buffer)))
        (buffer_name (minimap-buffer-name))
        (original_window (selected-window)))
    ;; let progn

    ;;; WINDOW CREATION
    ;; don't operate on minimap windows
    (unless (string-match minimap-buffer-name-prefix raw_buffer_name)
      ;; if minimap window is open
      (if (and minimap-window
               (window-live-p minimap-window))
          (progn
            ;; switch to it
            (select-window minimap-window)
            ;; kill existing buffer if there is one
            (when (string-match minimap-buffer-name-prefix
                                (buffer-name (current-buffer)))

              (when minimap-dedicated-window
                (set-window-dedicated-p minimap-window nil))
              (kill-buffer)))
        ;; otherwise split current window
        (unless (split-window-horizontally
                 (round (* (window-width) minimap-width-fraction)))
          (message "Failed to create window. Try `delete-other-windows' (C-x 1) first.")
          (return nil))
        ;; save new window to variable
        (setq minimap-window (selected-window))
        (setq was_created t))

      ;;; BUFFER CREATION
      (select-window minimap-window)
      (when minimap-dedicated-window
        (set-window-dedicated-p minimap-window nil))
      ;; if minimap buffer exists
      (if (get-buffer buffer_name)
          ;; show it
          (switch-to-buffer buffer_name t)
        ;; otherwise create new minimap buffer
        (minimap-new-minimap buffer_name current_buffer)
        ;; initialize timer
        (unless minimap-timer-object
          (setq minimap-timer-object
                (run-with-idle-timer minimap-update-delay t 'minimap-update))))
      (if was_created
          (other-window 1)
        (select-window original_window))
      (minimap-sync-overlays)
      (when minimap-dedicated-window
        (set-window-dedicated-p minimap-window 1)))))

(defun minimap-new-minimap (buffer_name target_buffer)
  "Create new minimap indirect-buffer pointing to target"
  (unless (or (string-match minimap-buffer-name-prefix (buffer-name target_buffer))
              (string-match "minibuf" (buffer-name target_buffer)))
    (let ((indirect_buffer (make-indirect-buffer target_buffer buffer_name t))
          (edges (window-pixel-edges)))
      ;; let progn
      (set-buffer indirect_buffer)
      ;; hide scrollbars?
      (when minimap-hide-scroll-bar
        (setq vertical-scroll-bar nil))
      ;; show buffer
      (switch-to-buffer indirect_buffer)
      (setq minimap-base-overlay (make-overlay (point-min) (point-max) nil t t))
      (overlay-put minimap-base-overlay 'face 'minimap-font-face)
      (setq minimap-pointmin-overlay (make-overlay (point-min) (1+ (point-min))))
      (setq minimap-start (window-start)
            minimap-end (window-end)
            minimap-active-overlay (make-overlay minimap-start minimap-end)
            line-spacing 0)
      (overlay-put minimap-active-overlay 'face
                   'minimap-active-region-background)
      (overlay-put minimap-active-overlay 'priority 5)
      (minimap-mode 1)
      (when (and (boundp 'linum-mode)
                 linum-mode)
        (linum-mode 0))
      (when minimap-hide-fringes
        (set-window-fringes nil 0 0))
      ;; Calculate the actual number of lines displayable with the minimap face.

      (setq minimap-numlines
            (floor
             (/
              (- (nth 3 edges) (nth 1 edges))
              (car (progn (redisplay) (window-line-height)))))))))

;;;###autoload
(defun minimap-kill ()
  "Kill minimap for current buffer.
Cancel the idle timer if no more minimaps are active."
  (interactive)
  (if (null minimap-window)
      (message "No minimap window found.")
    ;; kill all minimap buffers
    (set-window-dedicated-p minimap-window nil)
    (dolist (ele (buffer-list))
      (when (string-match minimap-buffer-name-prefix (buffer-name ele))
        (kill-buffer ele)))
    (delete-window minimap-window)
    (setq minimap-window nil)
    (when minimap-timer-object
      (cancel-timer minimap-timer-object)
      (setq minimap-timer-object nil))
    (message "Minimap killed.")))


(defun minimap-valid-target()
  "Return t if valid minimap target"
  (interactive)
  (not (or (string-match minimap-buffer-name-prefix  (buffer-name (current-buffer)))
           (string-match "minibuf" (buffer-name (current-buffer))))))


(defun minimap-update (&optional force)
  "Update the minimap sidebar."
  (interactive)
  (when (and minimap-window
             (window-live-p minimap-window)
             (minimap-valid-target))
    (set-window-dedicated-p minimap-window nil)
    (let (start end pt ov)
      (unless (get-buffer (minimap-buffer-name))
        (minimap-create))
      (set-window-buffer minimap-window (minimap-buffer-name))
      (setq start (window-start)
            end (window-end)
            pt (point)
            ov)
      (with-selected-window minimap-window
        (unless (and (not force)
                     (= minimap-start start)
                     (= minimap-end end))
          (move-overlay minimap-active-overlay start end)
          (setq minimap-start start
                minimap-end end)
          (minimap-recenter (line-number-at-pos (/ (+ end start) 2))
                            (/ (- (line-number-at-pos end)
                                  (line-number-at-pos start))
                               2)))
        (goto-char pt)
        (when minimap-always-recenter
          (recenter (round (/ (window-height) 2))))
        (when minimap-dedicated-window
          (set-window-dedicated-p minimap-window 1))))))

;;; Overlay movement
(defun minimap-move-overlay-mouse (start-event)
  "Move overlay by tracking mouse movement."
  (interactive "e")
  (mouse-set-point start-event)
  (when (get-buffer-window (buffer-base-buffer (current-buffer)))
    (let* ((echo-keystrokes 0)
           (end-posn (event-end start-event))
           (start-point (posn-point end-posn))
           (make-cursor-line-fully-visible nil)
           (cursor-type nil)
           (pcselmode (when (boundp 'pc-selection-mode)
                        pc-selection-mode))
           pt ev)
      (when (and pcselmode (fboundp 'pc-selection-mode))
        (pc-selection-mode -1))
      (move-overlay minimap-active-overlay start-point minimap-end)
      (track-mouse
        (minimap-set-overlay start-point)
        (while (and
                (consp (setq ev (read-event)))
                (eq (car ev) 'mouse-movement))
          (setq pt (posn-point (event-start ev)))
          (when (numberp pt)
            (minimap-set-overlay pt))))
      (select-window (get-buffer-window (buffer-base-buffer)))
      (minimap-update)
      (when (and pcselmode (fboundp 'pc-selection-mode))
        (pc-selection-mode 1)))))

(defun minimap-set-overlay (pt)
  "Set overlay position, with PT being the middle."
  (goto-char pt)
  (let* ((ovstartline (line-number-at-pos minimap-start))
         (ovendline (line-number-at-pos minimap-end))
         (ovheight (round (/ (- ovendline ovstartline) 2)))
         (line (line-number-at-pos))
         (winstart (window-start))
         (winend (window-end))
         newstart newend)
    (setq pt (point-at-bol))
    (setq newstart (minimap-line-to-pos (- line ovheight)))
    ;; Perform recentering
    (minimap-recenter line ovheight)
    ;; Set new position in main buffer and redisplay
    (with-selected-window (get-buffer-window (buffer-base-buffer))
      (goto-char pt)
      (set-window-start nil newstart)
      (redisplay t)
      (setq newend (window-end)))
    (when (eq minimap-recenter-type 'free)
      (while (> newend winend)
        (scroll-up 5)
        (redisplay t)
        (setq winend (window-end))))
    (move-overlay minimap-active-overlay newstart newend)))

(defun minimap-line-to-pos (line)
  "Return point position of line number LINE."
  (save-excursion
    (goto-char 1)
    (if (eq selective-display t)
        (re-search-forward "[\n\C-m]" nil 'end (1- line))
      (forward-line (1- line)))
    (point)))

(defun minimap-recenter (middle height)
  "Recenter the minimap according to `minimap-recenter-type'.
MIDDLE is the line number in the middle of the active region.
HEIGHT is the number of lines from MIDDLE to begin/end of the
active region."
  (cond
   ;; Relative recentering
   ((eq minimap-recenter-type 'relative)
    (let* ((maxlines (line-number-at-pos (point-max)))
           percentage relpos newline start numlines)
      (setq numlines (count-lines (window-start) (window-end)))
      (setq percentage (/ (float middle) (float maxlines)))
      (setq newline (ceiling (* percentage numlines)))
      (setq start (minimap-line-to-pos
                   (- middle height
                      (floor (* percentage
                                (- numlines height height))))))
      (or (> start (point-min))
          (setq start (point-min)))
      ;; If (point-max) already visible, don't go further
      (if (and (> start (window-start))
               (with-selected-window (get-buffer-window (buffer-base-buffer))
                 (= (point-max) (window-end))))
          (save-excursion
            (goto-char (point-max))
            (recenter -1))
        (unless (and (> start (window-start))
                     (= (point-max) (window-end)))
          (set-window-start nil start)))))
   ;; Middle recentering
   ((eq minimap-recenter-type 'middle)
    (let ((start (- middle height
                    (floor (* 0.5
                              (- minimap-numlines height height))))))
      (if (< start 1)
          (progn
            ;; Hack: Emacs cannot scroll down any further, so we fake
            ;; it using an overlay.  Otherwise, the active region
            ;; would move to the top.
            (overlay-put minimap-pointmin-overlay
                         'display (concat
                                   (make-string (abs start) 10)
                                   (buffer-substring (point-min) (1+ (point-min)))))
            (overlay-put minimap-pointmin-overlay
                         'face `(:background ,(face-background 'default)))
            (overlay-put minimap-pointmin-overlay
                         'priority 10)
            (setq start 1))
        (overlay-put minimap-pointmin-overlay 'display "")
        (overlay-put minimap-pointmin-overlay 'face nil))
      (set-window-start nil (minimap-line-to-pos start))))
   ;; Free recentering
   ((eq minimap-recenter-type 'free)
    (let ((newstart (minimap-line-to-pos (- middle height)))
          (winstart (window-start)))
      (while (< newstart winstart)
        (scroll-down 5)
        (redisplay t)
        (setq winstart (window-start)))))))

;;; satellite minor mode

(defvar minimap-mode-map (make-sparse-keymap)
  "Keymap used by `minimap-mode'.")

(define-key minimap-mode-map [down-mouse-1] 'minimap-move-overlay-mouse)
(define-key minimap-mode-map [down-mouse-2] 'minimap-move-overlay-mouse)
(define-key minimap-mode-map [down-mouse-3] 'minimap-move-overlay-mouse)

(define-minor-mode minimap-mode
  "Minor mode for minimap sidebar."
  nil "minimap" minimap-mode-map)

;;; Sync minimap with modes which create/delete overlays.

(defun minimap-sync-overlays ()
  "Synchronize overlays between base and minimap buffer.
Apply semantic overlays or face enlargement if necessary."
  (interactive)
  (let ((baseov (overlays-in (point-min) (point-max)))
        (semantic (and (boundp 'semantic-version)
                       (semantic-active-p)))
        ov props p)
    (set-window-dedicated-p minimap-window nil)
    (with-current-buffer (minimap-buffer-name)
      (remove-overlays)
      (while baseov
        (when (setq props (minimap-get-sync-properties (car baseov)))
          (setq ov (make-overlay (overlay-start (car baseov))
                                 (overlay-end (car baseov))))
          (while (setq p (car props))
            (overlay-put ov (car p) (cadr p))
            (setq props (cdr props))))
        (setq baseov (cdr baseov)))
      (move-overlay minimap-pointmin-overlay (point-min) (1+ (point-min)))
      ;; Re-apply font overlay
      (move-overlay minimap-base-overlay (point-min) (point-max)))
    ;; Face enlargement
    (when (and font-lock-mode
               (or (eq minimap-enlarge-certain-faces 'always)
                   (and (eq minimap-enlarge-certain-faces 'as-fallback)
                        (or (not minimap-display-semantic-overlays)
                            (not semantic)))))
      (when (eq font-lock-support-mode 'jit-lock-mode)
        (jit-lock-fontify-now))
      (with-current-buffer (minimap-buffer-name)
        (minimap-enlarge-faces)))
    ;; Semantic overlays
    (when (and semantic
               minimap-display-semantic-overlays)
      (minimap-apply-semantic-overlays)))
  (minimap-update t))

(defun minimap-get-sync-properties (ov)
  "Get properties from overlay OV which should be synced.
You can specify those properties with
`minimap-sync-overlay-properties'."
  (delq nil
        (mapcar
         (lambda (p)
           (let ((val (overlay-get ov p)))
             (if val
                 (list p val)
               nil)))
         minimap-sync-overlay-properties)))

(defun minimap-enlarge-faces ()
  "Apply default font to all faces in `minimap-normal-height-faces'.
This has to be called in the minimap buffer."
  (let ((pos (next-single-property-change (point-min) 'face))
        next ov face)
    (while pos
      (setq face (get-text-property pos 'face))
      (when (delq nil (mapcar (lambda (x) (equal x face))
                              minimap-normal-height-faces))
        (setq ov
              (make-overlay pos
                            (setq pos (next-single-property-change pos 'face))))
        (overlay-put ov 'face `(:family ,(face-font 'default)))
        (overlay-put ov 'priority 5))
      (setq pos (next-single-property-change pos 'face)))))

(defun minimap-apply-semantic-overlays ()
  "Apply semantic overlays to the minimap.
This has to be called from the base buffer."
  (let ((tags (semantic-fetch-tags))
        tag class ov ovnew)
    (while tags
      (setq tag (car tags))
      (setq class (semantic-tag-class tag))
      (setq ov (semantic-tag-overlay tag))
      (when (and (overlayp ov)
                 (or (eq class 'function)
                     (eq class 'type)
                     (eq class 'variable)))
        (message (minimap-buffer-name))
        (with-current-buffer (minimap-buffer-name)
          (let ((start (overlay-start ov))
                (end (overlay-end ov))
                (name (semantic-tag-name tag)))
            (overlay-put
             (setq ovnew (make-overlay start end))
             'face `(:background ,(face-background
                                   (intern (format "minimap-semantic-%s-face"
                                                   (symbol-name class))))))
            (overlay-put ovnew 'priority 1)
            (setq start
                  (minimap-line-to-pos (/ (+ (line-number-at-pos start)
                                             (line-number-at-pos end)) 2)))
            (setq end (progn (goto-char start) (point-at-eol)))
            (setq ovnew (make-overlay start end))
            (overlay-put ovnew 'face (format "minimap-semantic-%s-face"
                                             (symbol-name class)))
            (overlay-put ovnew 'display (concat "  " name "  "))
            (overlay-put ovnew 'priority 6))))
      (setq tags (cdr tags)))))

;; outline-(minor-)mode
(add-hook 'outline-view-change-hook 'minimap-sync-overlays)

;; hideshow
(add-hook 'hs-hide-hook 'minimap-sync-overlays)
(add-hook 'hs-show-hook 'minimap-sync-overlays)

(provide 'minimap)
;;; minimap.el ends here
