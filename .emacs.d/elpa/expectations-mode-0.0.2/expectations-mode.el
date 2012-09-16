;;; expectations-mode.el --- Minor mode for expectations tests

;; Author: Gareth Jones <gareth.e.jones@gmail.com>
;; Version: 0.0.2
;; Keywords: languages, lisp, test
;; Package-Requires: ((slime "20091016") (clojure-mode "1.7"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file provides support for running Clojure tests (using the
;; expectations framework) via SLIME and seeing feedback in the test
;; buffer about which tests failed or errored.

;; This library is based on the clojure-test-mode by Phil Hagelberg.

;;; History:

;; 0.0.1: 2012-04-10
;;  * initial release

;; 0.0.2: 2012-04-21
;;  * dont remove clojure-mode-hook for clojure-test-mode
;;  * you must now have your expectations files in an 'expectations'
;;    ns for the mode to automatically turn on.

;;; Code:

(require 'clojure-mode)
(require 'slime)

(defface expectations-failure-face
  '((((class color) (background light))
     :background "orange red")
    (((class color) (background dark))
     :background "firebrick"))
  "Face for failures in expectations tests."
  :group 'expectations-mode)

(defface expectations-error-face
  '((((class color) (background light))
     :background "orange1")
    (((class color) (background dark))
     :background "orange4"))
  "Face for errors in expectations tests."
  :group 'expectations-mode)

(defface expectations-success-face
  '((((class color) (background light))
     :foreground "black"
     :background "green")
    (((class color) (background dark))
     :foreground "black"
     :background "green"))
  "Face for success in expectations tests."
  :group 'expectations-mode)

;; vars to keep count of all/failed/errored tests

(defvar expectations-count         0)
(defvar expectations-failure-count 0)
(defvar expectations-error-count   0)

(defconst expectations-valid-results
  '(:success :fail :error)
  "Results we are interested in reporting on")

(defun expectations-eval (string &optional handler)
  (slime-eval-async `(swank:eval-and-grab-output ,string)
    (or handler #'identity)))

(defun expectations-eval-sync (string)
  (slime-eval `(swank:eval-and-grab-output ,string)))

(defun expectations-test-clear (&optional callback)
  "Clear all counters and unmap generated vars for expectations"
  (interactive)
  (remove-overlays)
  (setq expectations-count         0
        expectations-failure-count 0
        expectations-error-count   0)
  (expectations-eval
   "(require 'expectations)
    (expectations/disable-run-on-shutdown)
    (doseq [[a b] (ns-interns *ns*)
            :when ((meta b) :expectation)]
      (ns-unmap *ns* a))"
   callback))

(defun expectations-highlight-problem (line event msg)
  (save-excursion
    (goto-line line)
    (let ((beg (point)))
      (end-of-line)
      (let ((overlay (make-overlay beg (point))))
        (overlay-put overlay 'face (if (equal event :fail)
                                       'expectations-failure-face
                                     'expectations-error-face))
        (overlay-put overlay 'message msg)))))

(defun expectations-inc-counter-for (event)
  (when (member event expectations-valid-results)
    (incf expectations-count))
  (cond
   ((equal :fail event)  (incf expectations-failure-count))
   ((equal :error event) (incf expectations-error-count))))

(defun expectations-extract-result (result)
  (let ((event (car result)))
    (expectations-inc-counter-for event)
    (when (or (eq :fail (car result))
              (eq :error (car result)))
      (destructuring-bind (event msg line) (coerce result 'list)
        (expectations-highlight-problem line event msg)))))

(defun expectations-echo-results ()
  (message
   (propertize
    (format "Ran %s tests. %s failures, %s errors."
            expectations-count expectations-failure-count
            expectations-error-count)
    'face
    (cond ((not (= expectations-error-count 0)) 'expectations-error-face)
          ((not (= expectations-failure-count 0)) 'expectations-failure-face)
          (t 'expectations-success-face)))))

(defun expectations-extract-results (results)
  (let ((result-vars (read (cadr results))))
    (mapc #'expectations-extract-result result-vars)
    (expectations-echo-results)))

(defun expectations-get-results (result)
  (expectations-eval "(for [[n s] (ns-interns *ns*)
                            :let [m (meta s)]
                            :when (:expectation m)]
                        (apply list (:status m)))"
   #'expectations-extract-results))

(defun expectations-run-tests ()
  "Run all the tests in the current namespace."
  (interactive)
  (save-some-buffers nil (lambda () (equal major-mode 'clojure-mode)))
  (message "Testing...")
  (save-window-excursion
    (expectations-test-clear
     (lambda (&rest args)
       (slime-eval-async `(swank:load-file
                           ,(slime-to-lisp-filename
                             (expand-file-name (buffer-file-name))))
         (lambda (&rest args)
           (slime-eval-async '(swank:interactive-eval
                               "(expectations/run-tests [*ns*])")
             #'expectations-get-results)))))))

(defun expectations-show-result ()
  (interactive)
  (let ((overlay (find-if (lambda (o) (overlay-get o 'message))
                          (overlays-at (point)))))
    (if overlay
        (message (replace-regexp-in-string "%" "%%"
                                           (overlay-get overlay 'message))))))

(defvar expectations-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c ,")   'expectations-run-tests)
    (define-key map (kbd "C-c C-,") 'expectations-run-tests)
    (define-key map (kbd "C-c k")   'expectations-test-clear)
    (define-key map (kbd "C-c '")   'expectations-show-result)
    map))

;;;###autoload
(define-minor-mode expectations-mode
  "A minor mode for running expectations tests"
  nil " Expectations" expectations-mode-map)

;;;###autoload
(progn
  (defun expectations-maybe-enable ()
    "Enable expectations-mode and disable clojure-test-mode if
the current buffer contains a namespace with a \"test.\" bit on
it."
    (let ((ns (clojure-find-package)))  ; defined in clojure-mode.el
      (when (search "expectations." ns)
        (save-window-excursion
          (expectations-mode t)
          (clojure-test-mode 0)))))
  (add-hook 'clojure-mode-hook 'expectations-maybe-enable))

(provide 'expectations-mode)

;;; expectations-mode.el ends here
