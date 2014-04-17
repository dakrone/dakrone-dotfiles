;;; setup.el --- helper macros to write faster, portable and robust init script

;; Copyright (C) 2013 zk_phi

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA

;; Author: zk_phi
;; URL: http://hins11.yu-yake.com/
;; Version: 1.0.0

;;; Commentary:

;; Require this script when compile,
;;
;;   (eval-when-compile (require 'setup))
;;
;; and put
;;
;;   (setup-initialize)
;;
;; at the beginning of your init script. Then you can use following macros :
;;
;; - setup-eval (alias: !)
;; - setup-if (alias: !if)
;; - setup-when (alias: !when)
;; - setup-unless (alias: !unless)
;; - setup-case (alias: !case)
;; - setup-cond (alias: !cond)
;; - setup
;; - setup-include
;; - setup-lazy
;; - setup-after
;; - setup-expecting
;; - setup-in-idle
;; - setup-keybinds

;; For more informations, see "Readme".

;;; Change Log:

;; 1.0.0 first released

;;; Code:

(require 'cl-lib)
(require 'find-func)
(require 'macroexp)

(defconst setup-version "1.0.0")

;; + customizable vars

(defvar setup-include-allow-runtime-load 'undef
  "When non-nil, setup-include can load libraries in runtime, if
the source file is not found.")

(defvar setup-environ-warning-alist
  '(((system-name)
     . "Init script is not compiled with this system. Really continue ? ")
    (window-system
     . "Init script is not compiled with this window system. Really continue ? ")
    (user-login-name
     . "Init script is not compiled by this user. Really continue ? ")
    (emacs-version
     . "Init script is not compiled with this version of Emacs. Really continue ? "))
  "Alist of expressions that must be evaluated to the \"equal\"
value between compile-time and runtime, and warning message shown
when the value differes.")

;; + font-lock keywords for elisp mode

(eval-after-load "lisp-mode"
  '(font-lock-add-keywords
    'emacs-lisp-mode
    '(("(\\(setup\\(?:-\\(?:in\\(?:clude\\|-idle\\)\\|after\\|expecting\\|lazy\\)\\)?\\)\\_>"
       1 font-lock-keyword-face)
      ("(\\(!\\(?:when\\_>\\|if\\_>\\|unless\\_>\\|cond\\_>\\|case\\_>\\|[^\s\t\n]\\)?\\)"
       1 font-lock-keyword-face))))

;; + initialize

(defmacro setup-initialize ()
  "This macro is replaced with an initializing expression during compile.
PUT THIS MACRO AT THE BEGINNING OF YOUR INIT SCRIPT."
  `(progn
     (unless (and ,@(mapcar (lambda (pair)
                              `(or (equal ',(eval (car pair)) ,(car pair))
                                   (y-or-n-p ,(cdr pair))))
                            setup-environ-warning-alist))
       (error "Setup canceled."))
     ;; setup stopwatch
     (let ((beg-time (current-time)))
       (add-hook 'after-init-hook
                 `(lambda  ()
                    (message ">> [init] TOTAL: %d msec"
                             (let ((now (current-time)))
                               (+ (* (- (nth 1 now) ,(nth 1 beg-time)) 1000)
                                  (/ (- (nth 2 now) ,(nth 2 beg-time)) 1000)))))))))

;; + compile-time execution

(defmacro setup-eval (sexp)
  "Eval during compile."
  `(quote ,(eval sexp)))

(defmacro setup-if (test then &rest else)
  "Like \"if\" but anaphoric and expanded during compile."
  (declare (indent 2))
  (setq test (eval test))
  (macroexpand-all (if test then `(progn ,@else)) `((it . (lambda () ,test)))))

(defmacro setup-when (test &rest body)
  "Like \"when\" but anaphoric and expanded during compile."
  (declare (indent 1))
  (setq test (eval test))
  (macroexpand-all (when test `(progn ,@body)) `((it . (lambda () ,test)))))

(defmacro setup-unless (test &rest body)
  "Like \"unless\" but anaphoric and expanded during compile."
  (declare (indent 1))
  (setq test (eval test))
  (macroexpand-all (unless test `(progn ,@body)) `((it . (lambda () ,test)))))

(defmacro setup-cond (&rest clauses)
  "Like \"cond\" but anaphoric and expanded during compile."
  (let (val)
    (while (and clauses
                (not (setq val (eval (caar clauses)))))
      (setq clauses (cdr clauses)))
    (macroexpand-all `(progn ,@(cdar clauses)) `((it . (lambda () ,val))))))

(defmacro setup-case (expr &rest clauses)
  "Like \"case\" but anaphoric and expanded during compile."
  (declare (indent 1))
  (setq expr (eval expr))
  (while (and clauses
              (let ((keylist (caar clauses)))
                (and (not (and (null (cdr clauses))
                               (memq keylist '(t otherwise))))
                     (not (and (consp keylist)
                               (memql expr keylist)))
                     (not (and (atom keylist)
                               (eql expr keylist))))))
    (setq clauses (cdr clauses)))
  (macroexpand-all `(progn ,@(cdar clauses)) `((it . (lambda () ,expr)))))

(defalias '! 'setup-eval)
(defalias '!if 'setup-if)
(defalias '!when 'setup-when)
(defalias '!cond 'setup-cond)
(defalias '!case 'setup-case)
(defalias '!unless 'setup-unless)

;; + load and configure libraries

(defmacro setup (file &rest body)
  "Load FILE. Iff succeeded, eval BODY."
  (declare (indent defun))
  (let ((absfile (locate-library file)))
    (if (not (and absfile (file-exists-p absfile)))
        (progn (byte-compile-warn "%s not found" file)
               `(message "XX [init] %s: not found" ,file))
      (let* ((feature (intern file))
             (load-expr (if (featurep feature)
                            `(require ',feature nil t)
                          `(load ,absfile t t)))
             (beg-time (cl-gensym)))
        (eval load-expr)
        `(let ((,beg-time (current-time)))
           ,load-expr
           (condition-case err
               (progn ,@body
                      (message ">> [init] %s: succeeded in %d msec"
                               ,file
                               (let ((now (current-time)))
                                 (+ (* (- (nth 1 now) (nth 1 ,beg-time)) 1000)
                                    (/ (- (nth 2 now) (nth 2 ,beg-time)) 1000)))))
             (error (message "XX [init] %s: %s" ,file (error-message-string err)))))))))

(defun setup--read-all (stream)
  (condition-case nil
      (cons (read stream) (setup--read-all stream))
    (error nil)))

;; taken from "cl-lib.el"
(defun setup--byte-compiling-p ()
  (and (boundp 'byte-compile--outbuffer)
       (bufferp (symbol-value 'byte-compile--outbuffer))
       (equal (buffer-name (symbol-value 'byte-compile--outbuffer)) " *Compiler Output*")))

(defmacro setup-include (file &rest body)
  "Like \"setup\", but inserts the file source there instead of
loading it in runtime. \"eval-after-load\" works correctly."
  (declare (indent 1))
  (let ((feature (intern file))
        (srcfile (or (ignore-errors (find-library-name file))
                     (expand-file-name file)))
        (libfile (locate-library file)))
    (if (not (and srcfile (file-exists-p srcfile)))
        ;; => source file is not available
        (if (and libfile
                 (if (eq setup-include-allow-runtime-load 'undef)
                     (setq setup-include-allow-runtime-load
                           (or (not (setup--byte-compiling-p))
                               (y-or-n-p (concat "Some libraries are not includable."
                                                 " Load them in runtime ?"))))
                   setup-include-allow-runtime-load))
            ;; => compiled file is available
            `(setup ,file ,@body)
          ;; => compiled file is also not available
          (byte-compile-warn "%s not found" file)
          `(message "XX [init] %s: not found" ,file))
      ;; load before compiling (*FIXME* not very smart)
      (if (featurep feature) (require feature) (load libfile))
      (let* ((history (assoc libfile load-history))
             (source (with-temp-buffer
                       (insert-file-contents srcfile)
                       (setup--read-all (current-buffer))))
             (beg-time (cl-gensym)))
        `(let ((,beg-time (current-time)))
           (unless (assoc ,libfile load-history)
             (with-no-warnings
               ;; to suppress warnings on compilation, macroexpand source (without warnings)
               ;; before starting compilation
               ,@(cdr (with-no-warnings
                        (macroexpand-all (cons 'progn source)))))
             ;; add to load-history so that loading will not be performed again and
             ;; "eval-after-load" for this library will run immediately.
             (push ',history load-history)
             ;; run "eval-after-load"s
             (do-after-load-evaluation ,libfile))
           (condition-case err
               (progn ,@body
                      (message ">> [init] %s: succeeded in %d msec"
                               ,file
                               (let ((now (current-time)))
                                 (+ (* (- (nth 1 now) (nth 1 ,beg-time)) 1000)
                                    (/ (- (nth 2 now) (nth 2 ,beg-time)) 1000)))))
             (error (message "XX [init] %s: %s" ,file (error-message-string err)))))))))

(defmacro setup-lazy (triggers file &rest body)
  "Load FILE on TRIGGERS. When loaded, eval BODY."
  (declare (indent defun))
  (if (not (locate-library file))
      (progn (byte-compile-warn "%s not found" file)
             `(message "XX [init] %s: not found" ,file))
    (let ((prepare (when (and body (eq (car body) :prepare))
                     (prog1 (cadr body) (setq body (cddr body))))))
      `(progn
         (dolist (trigger ,triggers) (autoload trigger ,file nil t))
         ,(when prepare
            `(condition-case err ,prepare
               (error (message "XX [init] %s: %s" ,file (error-message-string err)))))
         (eval-after-load ,file
           ',(macroexpand-all
              `(condition-case err
                   (progn ,@body (message "<< [init] %s: loaded" ,file))
                 (error (message "XX [init] %s: %s" ,file (error-message-string err))))))
         (message "-- [init] %s will be autoloaded" ,file)))))

(defmacro setup-after (file &rest body)
  "Eval BODY after FILE is loaded."
  (declare (indent defun))
  (when (locate-library file)
    `(eval-after-load ,file
       ',(macroexpand-all
          `(condition-case err (progn ,@body)
             (error (message "XX [init] %s: %s" ,file (error-message-string err))))))))

(defmacro setup-expecting (file &rest body)
  "Eval BODY only when FILE exists."
  (declare (indent defun))
  (when (locate-library file)
    `(condition-case err (progn ,@body)
       (error (message "XX [init] %s: %s" ,file (error-message-string err))))))

(defmacro setup-alternative (file &rest body)
  "Eval BODY unless FILE exists."
  (declare (indent defun))
  (unless (locate-library file)
    `(condition-case err (progn ,@body)
       (error (message "XX [init] %s: %s" ,file (error-message-string err))))))

(defmacro setup-in-idle (file)
  "Load FILE in idle-time."
  (when (locate-library file)
    `(progn (message "-- [init] %s: ... will be loaded in idle-time" ,file)
            (run-with-idle-timer 15 nil (lambda () (require ',file))))))

;; + other utilities

(defun setup--list->tuples (lst)
  (when lst (cons (cons (car lst) (cadr lst)) (setup--list->tuples (cddr lst)))))

(defmacro setup-keybinds (keymap &rest binds)
  "Add BINDS to KEYMAP. BINDS must be a list of (KEYS DEF KEYS
DEF ...) where KEYS can be one of a string accepted by \"kbd\",
an event accepted by \"define-key\", or a list of above, and
COMMAND can be an object that \"define-key\" accepts or a list in
form (\"FILE\" THENCOMMAND :optional ELSECOMMAND])."
  (declare (indent 1))
  (let ((kmap (cl-gensym)))
    `(let ((,kmap (or ,keymap (current-global-map))))
       ,@(mapcar
          (lambda (bind)
            (let* ((keys (eval (car bind)))
                   (def (eval (cdr bind)))
                   (command (if (and (listp def)
                                     (stringp (car def)))
                                (if (locate-library (car def)) (cadr def)
                                  (or (nth 2 def) 'ignore))
                              def)))
              (cond
               ((listp keys)
                (setq keys (mapcar (lambda (k) (if (stringp k) (kbd k) k)) keys))
                `(dolist (key ',keys) (define-key ,kmap key ',command)))
               (t
                (setq keys (if (stringp keys) (kbd keys) keys))
                `(define-key ,kmap ,keys ',command)))))
          (setup--list->tuples binds)))))

;; + (provide)

(provide 'setup)

;;; setup.el ends here
