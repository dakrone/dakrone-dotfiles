;; setting alignment
(require 'align)
(add-to-list 'align-rules-list
             '(camma-assignment
               (regexp . ",\\( *\\)")
               (repeat . t)
               (modes  . '(cperl-mode))))
