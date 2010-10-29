;; Sonian-speific stuff
(let ((sonian-nav-file "~/safe/sa-safe/.elisp/sonian-navigation.el"))
  (when (file-exists-p sonian-nav-file)
     (load (expand-file-name sonian-nav-file))))
