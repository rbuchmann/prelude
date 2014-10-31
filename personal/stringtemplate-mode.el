;;; Adapted from the mode by Stefan Birman <stefan@amiq.ro>

;; define a group

(defgroup stringtemplate nil
  "Major mode for StringTemplate files."
  :group 'languages
  :link '(emacs-commentary-link "stringtemplate-mode.el")
  :link '(url-link "http://stringtemplate-mode.sourceforge.net/")
  :prefix "stringtemplate-")

(defun stringtemplate-align-defs ()
  (interactive)
  (align-regexp (point-min) (point-max) "\\(\s-*\\)::="))

;; allow user to run it's own code
(defvar stringtemplate-mode-hook nil)

;; allows both you and users to define their own keymaps.
;; using define-key, we insert an example keybinding into the keymap
(defvar stringtemplate-mode-map
  (let ((stringtemplate-mode-map (make-sparse-keymap)))
    (define-key stringtemplate-mode-map "\C-j" 'newline-and-indent)
    (define-key stringtemplate-mode-map (kbd "s-a") 'stringtemplate-align-defs)
    stringtemplate-mode-map)
  "Keymap for stringtemplate major mode")

;; tells emacs that when a buffer with a name ending with 'st'
;; is opened, then stringtemplate-mode should be started in that buffer
(add-to-list 'auto-mode-alist '("\\.st\\'" . stringtemplate-mode))
(add-to-list 'auto-mode-alist '("\\.stg\\'" . stringtemplate-mode))

;; define minimal set of keywords to highlight
(defconst stringtemplate-font-lock-keywords-1
  (list
   '("\\(<<\\|>>\\|::=\\)" . font-lock-constant-face)
   '("\\$[^$]*\\$" 0 font-lock-keyword-face t)
   '("^\\w+" . font-lock-function-name-face))
  "Minimal highlighting expressions for stringtemplate mode")

;;  defined the default level of highlighting to be 1.
(defvar stringtemplate-font-lock-keywords stringtemplate-font-lock-keywords-1
  "Default highlighting expressions for stringtemplate mode")

;; create an empty syntax mode and initializes it
(defvar stringtemplate-mode-syntax-table
  (let ((stringtemplate-mode-syntax-table (make-syntax-table)))
    (modify-syntax-entry ?_ "w" stringtemplate-mode-syntax-table)
    (modify-syntax-entry ?< ". 12" stringtemplate-mode-syntax-table)
    (modify-syntax-entry ?> ". 34" stringtemplate-mode-syntax-table)
    (modify-syntax-entry ?! ". 23" stringtemplate-mode-syntax-table)
    stringtemplate-mode-syntax-table)
  "Syntax table for stringtemplate-mode")

;; Here we define our entry function, give it a documentation string,
;; make it interactive, and call our syntax table creation function.
;; We also set our keymap for the mode.
(defun stringtemplate-mode ()
  "Major mode for editing StringTemplate files"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table stringtemplate-mode-syntax-table)
  (use-local-map stringtemplate-mode-map)
  (setq font-lock-defaults
        '(stringtemplate-font-lock-keywords))
  (set (make-local-variable font-lock-comment-face)
       'font-lock-string-face)
  (setq major-mode 'stringtemplate-mode)
  (setq mode-name "StringTemplate")
  (add-hook 'before-save-hook 'stringtemplate-align-defs nil t)
  (run-hooks 'stringtemplate-mode-hook))

;; finally, we use provide to expose our mode to the Emacs environment.
(provide 'stringtemplate-mode)

;;;;; stringtemplate-mode.el ends here
