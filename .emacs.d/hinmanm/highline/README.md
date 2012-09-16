highline
=========

Emacs version of the Vim powerline.


I changed the name of this version to differentiate it from the original [Emacs Powerline](http://www.emacswiki.org/emacs/PowerLine) which is a fork of [vim-powerline](https://github.com/Lokaltog/vim-powerline).

This is a rewrite of the Emacs version because I thought the code was a little messy.  It also didn't allow the speparators (arrows) to be dynamically sized.  That means the mode-line was always required to be a particular size.  This version also allows the *right* side of the mode-line to be calculated ahead of time and the fill to be more accurate.  Thus, people can do more tweaking of the status bar.

In essence, this extensions just provides functions that can be used in the `mode-line-format`.  The default version for highline is:

    (setq-default mode-line-format
                  '("%e"
                    (:eval
                     (let* ((active (eq (frame-selected-window) (selected-window)))
                            (face1 (if active 'highline-active1 'highline-inactive1))
                            (face2 (if active 'highline-active2 'highline-inactive2))
                            (lhs (concat
                                  (highline-raw "%*" nil 'l)
                                  (highline-buffer-size nil 'l)
                                  (highline-raw "%12b" nil 'l)
    
                                  (highline-arrow-right nil face1)
    
                                  (highline-major-mode face1 'l)
                                  (highline-minor-modes face1 'l)
                                  (highline-raw mode-line-process face1 'l)
    
                                  (highline-narrow face1 'l)
    
                                  (highline-raw " " face1)
                                  (highline-arrow-right face1 face2)
    
                                  (highline-vc face2 'l)
                                  ))
                            (rhs (concat
                                  (highline-raw global-mode-string face2 'r)
    
                                  (highline-arrow-left face2 face1)
                                  (highline-raw " " face1)
    
                                  (highline-raw "%4l" face1 'r)
                                  (highline-raw ":" face1)
                                  (highline-raw "%3c" face1 'r)
    
                                  (highline-arrow-left face1 nil)
                                  (highline-raw " ")
    
                                  (highline-raw "%6p" nil 'r)
    
                                  (highline-hud face2 face1))))
                       (concat lhs (highline-fill face2 (length (format-mode-line rhs))) rhs)))))

                       
The last line of this is what actually puts it all together.  But notice we pre-compile the `rhs` of the statusline and this allows us to more accurately right justify the text.  There are currently no other "separators", just the arrows.  I will be introducing more gradually.  This version should be easier to modify.



