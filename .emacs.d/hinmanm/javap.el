 ;;; javap.el --- Javap major mode

;; Copyright (C) 2011 Kevin Downey

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.

(defconst javap-font-lock-keywords
  (eval-when-compile
    `(
      (,(regexp-opt '("Exception table"
                      "LocalVariableTable"
                      "LineNumberTable"))
       0 font-lock-warning-face)
      
      ("\\<[a-zA-Z]+\\.[a-zA-Z0-9._]*[A-Z]+[a-zA-Z0-9/.$]*\\>" 0 font-lock-type-face) ;; borrowed from clojure-mode
      ("\\<[a-zA-Z]+/[a-zA-Z0-9/_]*[A-Z]+[a-zA-Z0-9/$]*\\>" 0
       font-lock-type-face)
      ("invoke\\w+" 0 font-lock-function-name-face)
      ("\\(\\w\\|_\\)+(" 0 font-lock-preprocessor-face)
      (")" 0 font-lock-preprocessor-face)
      (,(regexp-opt '("boolean" "int" "void" "char"))
       0 font-lock-type-face)

      (".load_\\w+" 0 font-lock-keyword-face)
      (".load " 0 font-lock-keyword-face)
      (".store_\\w+" 0 font-lock-keyword-face)
      (".const_\\w+" 0 font-lock-keyword-face)
      (".return" 0 font-lock-keyword-face)
      ("if_icmpne" 0 font-lock-keyword-face)
      (".add" 0 font-lock-keyword-face)

      (,(concat (regexp-opt '("i" "l" "d" "f" "c" "b" "s"))
                "2"
                (regexp-opt '("i" "l" "d" "f" "c" "b" "s")))
       0 font-lock-keyword-face)
      
      (,(regexp-opt
         '("ifne" "athrow" "new" "dup" "aastore" "anewarray" "ifnull" "ifeq" "ifnonnull"
           "getstatic" "putfield" "getfield" "checkcast" "astore" "aload" "ldc" "goto" "putstatic"
           "pop" "instanceof" "ldc_w" "sipush" "bipush" "aaload" "bastore" "baload" "arraylength"
           "castore" "saload" "lastore" "daload" "dastore" "ifle" "istore" "lookupswitch" "iinc"
           "if_icmpge" "isub" "if_icmpgt" "if_acmpne" "iflt" "if_icmplt" "if_icmple" "dcmpg"
           "dcmpl" "ldc2_w" "lcmp" "fcmpg" "fcmpl" "ifge" "jsr" "ifgt" "ret"))
       0 font-lock-keyword-face)

      (,(regexp-opt
         '("public" "static" "final" "volatile" ";" "transient" "class" "extends" "implements"
           "synchronized" "protected" "private" "abstract" "interface" "Code:"))
       0 font-lock-comment-face)
      ("line [0-9]+:" 0 font-lock-comment-face)
      ("[0-9]+:" 0 font-lock-comment-face)
      ))
  "Default expressions to highlight in javap mode.")

 ;;;###autoload
(define-derived-mode javap-mode fundamental-mode "javap"
  "A major mode for viewing javap files."
  :syntax-table javap-mode-syntax-table
  (set (make-local-variable 'comment-start) "#")
  (set (make-local-variable 'comment-start-skip) "#.+")
  (set (make-local-variable 'font-lock-defaults) '(javap-font-lock-keywords)))

(defun javap-buffer ()
  "run javap on contents of buffer"
  (interactive)
  (lexical-let* ((b-name (file-name-nondirectory (buffer-file-name)))
                 (b-name (substring b-name 0 (- (length b-name) 6)))
                 (new-b-name (concat "*javap " b-name ".class" "*"))
                 (new-buf (get-buffer new-b-name))
                 (old-buf (buffer-name))
                 (done (lambda (&rest args)
                         (interactive)
                         (progn
                           (kill-buffer (current-buffer))
                           (kill-buffer old-buf)))))
    (progn
      (if new-buf
          (switch-to-buffer-other-window new-buf)
        (let ((new-buf (get-buffer-create new-b-name)))
          (progn
            (switch-to-buffer-other-window new-buf)
            (call-process "javap" nil new-buf nil "-c" "-l" "-package" "-protected" "-private" "-classpath" "." b-name)
            (setq buffer-read-only 't)
            (set-window-point (selected-window) 0))))
      (javap-mode)
      (local-set-key [(q)] done))))

(add-hook 'find-file-hook
          (lambda (&rest args)
            (if (string= ".class" (substring (buffer-file-name) -6 nil))
                (javap-buffer))))

(provide 'javap-mode)

