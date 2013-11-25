;; Font things!
(when (eq window-system 'ns)
  (set-fontset-font "fontset-default" 'symbol "Monaco")
  (set-default-font "Anonymous Pro")
  (set-face-attribute 'default nil :height 115))
(when (eq window-system 'mac)
  (set-fontset-font "fontset-default" 'symbol "Monaco")
  (set-default-font "Anonymous Pro")
  (set-face-attribute 'default nil :height 125))
(when (eq window-system 'x)
  ;; Font family
  (set-fontset-font "fontset-default" 'symbol "Bitstream Vera Sans Mono")
  (set-default-font "Bitstream Vera Sans Mono")
  ;; Font size
  ;; 100 is too small, 105 is too big, 103 is juuuuuust right
  (set-face-attribute 'default nil :height 90))
;; Anti-aliasing
(setq mac-allow-anti-aliasing t)
