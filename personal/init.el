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

(setq-default indent-tabs-mode nil)

(setq-default cursor-type 'bar)
(blink-cursor-mode nil)
(fset 'yes-or-no-p 'y-or-n-p)
(setq search-highlight t
      query-replace-highlight t)

(load-file "~/.emacs.d/personal/local_settings.el")

(add-to-list 'default-frame-alist (cons 'font local-font))
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(setq-default frame-title-format "%b %f")

(setq prelude-whitespace nil)

(setq magit-last-seen-setup-instructions "1.4.0")

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package Autoinstall ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(prelude-require-packages '(auctex auctex-latexmk clojure-snippets color-theme livescript-mode org-present clj-refactor slamhound magit-gerrit tup-mode magit-gh-pulls))

;;;;;;;;;;
;; helm ;;
;;;;;;;;;;

(require 'helm-files)
(setq helm-boring-file-regexp-list (append '("\\.$" "\\.\\.$" "\\.bst$" "\\.cls$" "\\.fdb_latexmk$" "\\.fls$" "\\.bbl$" "\\.blg$" "\\.aux$") 'helm-boring-file-regexp-list))
(setq helm-ff-skip-boring-files t)

;;;;;;;;;;;;;;;
;; yasnippet ;;
;;;;;;;;;;;;;;;

(yas-global-mode 1)
(setq yas-snippet-dirs (append '("~/.emacs.d/personal/snippets") yas-snippet-dirs))
(yas-load-directory "~/.emacs.d/personal/snippets")

;;;;;;;;;;;;;;;;;;
;; Key Bindings ;;
;;;;;;;;;;;;;;;;;;

(global-set-key [f7]   (lambda()(interactive)(find-file "~/org/todo.org")))
(global-set-key [f8]   (lambda()(interactive)(find-file (concat conf-dir "init.el"))))
(global-set-key [f9]   (lambda()(interactive)(find-file "~/.lein/profiles.clj")))
(global-set-key [f10] (lambda()(interactive)(comment-or-uncomment-region (region-beginning) (region-end))))
(add-hook 'clojure-mode-hook '(lambda ()
                                (local-set-key (kbd "RET") 'newline-and-indent)))

(global-set-key (kbd "C-x ,") 'comment-or-uncomment-region)

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
(set-default-font local-font)

(require 'color-theme)
;(load "color-theme-blue")
(color-theme-blue)
;(load-theme 'manoj-dark)
;; (load "color-theme-dark-bliss")(color-theme-dark-bliss)
;; (load "color-theme-twilight")(color-theme-twilight)
;;(load "color-theme-sunburst")(color-theme-tm)

(global-font-lock-mode 1)
(global-hl-line-mode -1)
;; maximum colors
(setq font-lock-maximum-decoration t)

;;;;;;;;;;;;
;; AucTex ;;
;;;;;;;;;;;;

(require 'tex-site)
(setq prelude-guru nil)

(require 'reftex)

(add-to-list 'reftex-label-alist
             '("theorem" ?h "thr:" "~\\ref{%s}" t   ("theorem" "th.") -3))

(add-hook 'LaTeX-mode-hook
          (lambda ()
            (ispell-change-dictionary "english")
            (setq TeX-open-quote "\"`")
            (setq TeX-close-quote "\"'")
            (LaTeX-math-mode)
            (outline-minor-mode)
            (reftex-mode)
            (auto-fill-mode)
            (LaTeX-add-environments
             '("theorem" LaTeX-env-label))))

(setq reftex-plug-into-AUCTeX t)

(setq TeX-auto-save t)

(require 'auctex-latexmk)
(auctex-latexmk-setup)

(defun add-latexmkrc ()
  (interactive)
  (with-temp-file (concat (file-name-directory (buffer-file-name)) ".latexmkrc")
    (insert "$pdf_mode=1;")))


(setq TeX-electric-sub-and-superscript t)
(setq TeX-electric-math (cons "$" "$"))

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


;;;;;;;;;;;;;
;; clojure ;;
;;;;;;;;;;;;;

(load-file "~/.emacs.d/personal/clovenience.el")

(require 'clj-refactor)

(add-hook 'clojure-mode-hook
          (lambda ()
            (paredit-mode 1)
            (clj-refactor-mode 1)
            (cljr-add-keybindings-with-prefix "C-c C-f")
            (define-key clojure-mode-map
              (kbd "C-c C-s")
              'clojure-goto-test-or-back)
            (define-key clojure-mode-map
              (kbd "C-c C-a")
              'clojure-add-ns)))


(add-to-list 'auto-mode-alist '("\.pxi$" . clojure-mode))

(add-hook 'cider-repl-mode-hook 'paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'paredit-mode-hook
          (lambda ()
            (local-set-key (kbd "M-q") 'indent-sexp)))
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(setq cider-repl-use-clojure-font-lock t)

(define-clojure-indent
  (describe 'defun)
  (it 'defun)
  (facts 'defun)
  (fact 'defun))

;;;;;;;;;;;;;;;;;
;; go language ;;
;;;;;;;;;;;;;;;;;

(defun my-go-mode-hook ()
  (setq gofmt-command "goimports")
  (local-set-key (kbd "M-.") 'godef-jump))

(add-hook 'go-mode-hook 'my-go-mode-hook)

;;;;;;;;;;;;;;;;;;;;
;; stringtemplate ;;
;;;;;;;;;;;;;;;;;;;;

(load-file "~/.emacs.d/personal/stringtemplate-mode.el")
(require 'stringtemplate-mode)

;;;;;;;;;;;;;
;; orgmode ;;
;;;;;;;;;;;;;

(setq org-agenda-files (list "~/org/"))

(setq org-src-fontify-natively t)


(setq org-todo-keywords
      '((sequence "TODO(t)" "|" "DONE(d!)" "CANCELED(c@)")))

(eval-after-load "org-present"
  '(progn
     (add-hook 'org-present-mode-hook
               (lambda ()
                 (org-present-big)
                 (org-display-inline-images)
                 (org-present-hide-cursor)
                 (org-present-read-only)))
     (add-hook 'org-present-mode-quit-hook
               (lambda ()
                 (org-present-small)
                 (org-remove-inline-images)
                 (org-present-show-cursor)
                 (org-present-read-write)))))
;;;;;;;;;;;
;; magit ;;
;;;;;;;;;;;

(setq magit-push-always-verify nil)
(require 'magit-gerrit)
(require 'magit-gh-pulls)
(add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls)
