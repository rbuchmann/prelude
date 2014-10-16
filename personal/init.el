;;;;;;;;;;;;;;;;;;
;; Global Stuff ;;
;;;;;;;;;;;;;;;;;;

(setq conf-dir (concat
                ;; expanded for stuff like java class path
                (expand-file-name user-emacs-directory)
                "personal/"))

(setq inhibit-splash-screen t)
(push '("." . "~/.emacs-backups") backup-directory-alist)
(setq kill-whole-line t)
(show-paren-mode t)
;(setq-default transient-mark-mode t)
(setq show-trailing-whitespace t)
(setq prelude-flyspell nil)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq-default indent-tabs-mode t)

(setq-default cursor-type 'bar)
(blink-cursor-mode nil)
(fset 'yes-or-no-p 'y-or-n-p)
(setq search-highlight t
      query-replace-highlight t)
(setq x-select-enable-clipboard t)

(custom-set-variables
 '(initial-frame-alist (quote ((fullscreen . maximized)))))


(setq-default frame-title-format "%b %f")

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package Autoinstall ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(prelude-require-packages '(auctex clojure-mode clojure-snippets color-theme livescript-mode org-trello clj-refactor org-pomodoro wgrep slamhound))

;;;;;;;;;;;;;;;
;; yasnippet ;;
;;;;;;;;;;;;;;;

(add-hook 'clojure-mode-hook (lambda () (yas/minor-mode 1)))

;;;;;;;;;;;;;;;;;;
;; Key Bindings ;;
;;;;;;;;;;;;;;;;;;

(global-set-key [f8]   (lambda()(interactive)(find-file (concat conf-dir "init.el"))))
(add-hook 'clojure-mode-hook '(lambda ()
                                (local-set-key (kbd "RET") 'newline-and-indent)))

;;;;;;;;;;;;;;;;
;; livescript ;;
;;;;;;;;;;;;;;;;

(require 'livescript-mode)

;;;;;;;;;;;;;;
;; uniquify ;;
;;;;;;;;;;;;;;

(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t) ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

;;;;;;;;;;
;; Look ;;
;;;;;;;;;;

;(tool-bar-mode -1)
;(menu-bar-mode -1)
(set-scroll-bar-mode 'right)
(set-default-font "Monospace-12")

(require 'color-theme)
;(load "color-theme-blue")
(color-theme-blue)
;(load-theme 'manoj-dark)
;; (load "color-theme-dark-bliss")(color-theme-dark-bliss)
;; (load "color-theme-twilight")(color-theme-twilight)
;;(load "color-theme-sunburst")(color-theme-tm)

(global-font-lock-mode 1)
;; maximum colors
(setq font-lock-maximum-decoration t)

;;;;;;;;;;;;
;; AucTex ;;
;;;;;;;;;;;;

(load "auctex.el" nil t t)
(setq prelude-guru nil)

(add-hook 'LaTeX-mode-hook
          (lambda ()
            (ispell-change-dictionary "english")
            (setq TeX-open-quote "\"`")
            (setq TeX-close-quote "\"'")
            (LaTeX-math-mode)))

(setq TeX-auto-save t)

;Support for Emacs, and XEmacs on MS-Windows with the development version, only. Support for XEmacs on MS-Windows/native is limited due to missing fonts (support for Latin-1, Latin-5 and half the math symbols only, no super- and subscripts).
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(setq font-latex-quotes 'english)
                                        ;(add-hook 'LaTeX-mode-hook 'flyspell-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Working with .emacs ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dotemacs-header ()
  (interactive)
  (let* ((header (read-from-minibuffer "Header name: "))
         (text-line (concat ";; " header " ;;"))
         (count (length text-line))
         (comment-line (make-string count (string-to-char ";"))))
    (insert (concat
             comment-line "\n"
             text-line "\n"
             comment-line "\n\n"))))


(add-hook 'cider-repl-mode-hook 'paredit-mode)
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(setq nrepl-hide-special-buffers t)
(setq cider-repl-use-clojure-font-lock t)

;;;;;;;;;;;
;; wgrep ;;
;;;;;;;;;;;

(require 'wgrep)

;;;;;;;;;;;;;;
;; pomodoro ;;
;;;;;;;;;;;;;;

(require 'org-pomodoro)

;;;;;;;;;;;;;
;; orgmode ;;
;;;;;;;;;;;;;

(setq org-agenda-files (list "~/org/"))

(setq org-todo-keywords
     '((sequence "TODO(t)" "|" "DONE(d!)" "CANCELED(c@)")))


;;;;;;;;;;;;;;;;;;
;; clj-refactor ;;
;;;;;;;;;;;;;;;;;;

(require 'clj-refactor)
(add-hook 'clojure-mode-hook (lambda ()
                               (clj-refactor-mode 1)
			       (cljr-add-keybindings-with-prefix "C-c C-f")))

;;;;;;;;;;;;
;; trello ;;
;;;;;;;;;;;;

(require 'org-trello)
(add-hook 'org-mode-hook (lambda () (org-trello-mode 1)))
