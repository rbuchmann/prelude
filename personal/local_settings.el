(defun set-small-font ()
  (interactive)
  (set-frame-font (font-spec :name "Input Mono Medium"
                             :size 16
                             :weight 'normal
                             :width 'normal
                             :spacing 90
                             :powerline-scale 1.0)
                  nil t))
