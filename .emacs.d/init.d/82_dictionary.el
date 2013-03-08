;; SDIC dictonary for Linux
(autoload 'sdic-describe-word "sdic" "search word" t nil)
(global-set-key (kbd "C-c w") 'sdic-describe-word)

(eval-after-load "sdic"
  '(progn
     (global-set-key (kbd "C-c W") 'sdic-describe-word-at-point)
     (setq sdic-default-coding-system 'utf-8)

     (setq sdicf-array-command "/usr/bin/sary") ;; my modified sary command path
     (setq sdic-eiwa-dictionary-list
           '((sdicf-client "/home/syohei/local/dict/eijiro127.sdic"
                           (strategy array))))
     (setq sdic-waei-dictionary-list
           '((sdicf-client "/home/syohei/local/dict/waeiji127.sdic"
                           (strategy array))))
     ;; saryを直接使用できるように sdicf.el 内に定義されている
     ;; arrayコマンド用関数を強制的に置換
     (fset 'sdicf-array-init 'sdicf-common-init)
     (fset 'sdicf-array-quit 'sdicf-common-quit)
     (fset 'sdicf-array-search
           (lambda (sdic pattern &optional case regexp)
             (sdicf-array-init sdic)
             (if regexp
                 (signal 'sdicf-invalid-method '(regexp))
               (save-excursion
                 (set-buffer (sdicf-get-buffer sdic))
                 (delete-region (point-min) (point-max))
                 (apply 'sdicf-call-process
                        sdicf-array-command
                        (sdicf-get-coding-system sdic)
                        nil t nil
                        (if case
                            (list "-i" pattern (sdicf-get-filename sdic))
                          (list pattern (sdicf-get-filename sdic))))
                 (goto-char (point-min))
                 (let (entries)
                   (while (not (eobp)) (sdicf-search-internal))
                   (nreverse entries))))))

     (defadvice sdic-search-eiwa-dictionary (after highlight-phrase (arg) activate)
       (highlight-phrase arg "hi-yellow"))
     (defadvice sdic-search-waei-dictionary (after highlight-phrase (arg) activate)
       (highlight-phrase arg "hi-yellow"))

     ;; omake
     (defadvice sdic-forward-item (after sdic-forward-item-always-top activate)
       (recenter 0))
     (defadvice sdic-backward-item (after sdic-backward-item-always-top activate)
       (recenter 0))))

(defun sdic-popup-last-word ()
  (interactive)
  (pop-to-buffer "*sdic*"))
