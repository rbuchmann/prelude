(defun set-small-font ()
  (interactive)
  (set-frame-font (font-spec :name "Inconsolata"
                             :size 18
                             :weight 'normal
                             :width 'normal
                             :powerline-scale 1.0)
                  nil t))
