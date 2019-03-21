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

(prefer-coding-system 'utf-8)
;; (setq coding-system-for-read 'utf-8)
;; (setq coding-system-for-write 'utf-8)


;; Fixes a wtf where it pings domains instead of autocompleting Oo
(setq ido-use-filename-at-point nil)
(setq company-dabbrev-downcase nil)


;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package Autoinstall ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(prelude-require-packages '(auctex auctex-latexmk color-theme clj-refactor magit-gerrit magit-gh-pulls use-package prettier-js color-theme ein pytest))

;;;;;;;;;;
;; helm ;;
;;;;;;;;;;

(require 'helm-files)

(setq helm-ff-skip-boring-files t)
(setq helm-grep-ag-command "rg --vimgrep --no-heading")

;;;;;;;;;;;;;;;
;; yasnippet ;;
;;;;;;;;;;;;;;;

(yas-global-mode 1)
(setq yas-snippet-dirs (append '("~/.emacs.d/personal/snippets") yas-snippet-dirs))
(yas-load-directory "~/.emacs.d/personal/snippets")

;;;;;;;;;;;;;;;;;;
;; Key Bindings ;;
;;;;;;;;;;;;;;;;;;

(global-set-key [f6] (lambda()(interactive)
                       (switch-to-buffer (make-temp-name "story"))
                       (insert-file-contents "~/org/story_template.txt")))
(global-set-key [f7]   (lambda()(interactive)(find-file "~/org/todo.org")))
(global-set-key [f8]   (lambda()(interactive)(find-file (concat conf-dir "init.el"))))
(global-set-key [f9]   (lambda()(interactive)(find-file "~/org/interruptions.org")))
(global-set-key [f10] (lambda()(interactive)(comment-or-uncomment-region (region-beginning) (region-end))))
(add-hook 'clojure-mode-hook '(lambda ()
                                (local-set-key (kbd "RET") 'newline-and-indent)))

(global-set-key (kbd "C-x ,") 'comment-or-uncomment-region)

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

(setq prelude-guru nil)

;;;;;;;;;;;;
;; AucTex ;;
;;;;;;;;;;;;

(require 'tex-site)

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

(add-to-list 'interpreter-mode-alist
             '("trile" . clojure-mode))

(load-file "~/.emacs.d/personal/clovenience.el")

(require 'clj-refactor)

(defun pretty-fns ()
  "make some word or string show as pretty Unicode symbols"
  (setq prettify-symbols-alist
        '(
          ("fn" . 955) ; λ
          )))

(global-prettify-symbols-mode 1)

(add-hook 'clojure-mode-hook
          (lambda ()
            (pretty-fns)
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

(setq cider-repl-use-clojure-font-lock t)
(setq cider-repl-use-pretty-printing t)
(setq cider-pprint-fn 'pprint)

; (set-variable 'cider-lein-parameters "with-profile +dev repl :headless")


(define-clojure-indent
  (describe 'defun)
  (it 'defun)
  (facts 'defun)
  (fact 'defun))

(setq cider-cljs-lein-repl "(do (use 'figwheel-sidecar.repl-api) (start-figwheel!) (cljs-repl))")

;;;;;;;;
;; js ;;
;;;;;;;;

;; (require 'flycheck-flow)
;; (defun my-js-mode-hook ()
;;   (jsx-mode)
;;   (setq-default)
;;   (setq tab-width 2)
;;   (setq standard-indent 2)
;;   (setq indent-tabs-mode nil)
;;   (electric-pair-mode t))

;; (add-hook 'js-mode-hook 'my-js-mode-hook)

(use-package jsx-flow-mode
  :load-path "/home/rasmus/programming/libs/jsx-flow-mode"
  :config
  (progn
    (add-to-list 'auto-mode-alist '("\\.js\\'" . jsx-flow-mode))
    (add-to-list 'auto-mode-alist '("\\.jsx\\'" . jsx-flow-mode))))

(require 'prettier-js)
(setq prettier-js-args '(
                         "--print-width" "120"
                         "--tab-width" "4"
                         ;  parser" "babylon"
                         "--single-quote"  "false"
                         "--trailing-comma" "none"
                         "--bracket-spacing" "true"
                         "--jsx-bracket-same-line" "true"
                         ))

(add-hook 'jsx-flow-mode-hook
          (lambda ()
            (prettier-js-mode t)
            (setq-default)
            (setq c-basic-offset 4)
            (setq tab-width 4)
            (setq standard-indent 4)
            (setq indent-tabs-mode nil)))

;;;;;;;;;;;;;;;;;
;; go language ;;
;;;;;;;;;;;;;;;;;

(defun my-go-mode-hook ()
  (setq gofmt-command "goimports")
  (setq-default)
  (setq tab-width 2)
  (setq standard-indent 2)
  (setq indent-tabs-mode nil)
  (local-set-key (kbd "M-.") 'godef-jump))

(add-hook 'go-mode-hook 'my-go-mode-hook)

;;;;;;;;;;;;;
;; orgmode ;;
;;;;;;;;;;;;;

(setq org-agenda-files (list "~/org/todo.org"))

(setq org-refile-targets '(("~/org/todo.org" :maxlevel . 1)
                           ("~/org/home.org" :maxlevel . 1)
                           ("~/gtd/backlog.org" :level . 1)))

(setq org-src-fontify-natively t)

;; (setq org-agenda-skip-function #'my-org-agenda-skip-all-siblings-but-first)

;; (setq org-agenda-custom-commands
;;       '(("o" "At the office"
;;          ((agenda "" ((org-agenda-files '("~/org/todo.org"))))
;;           (org-agenda-overriding-header "Office")
;;           (org-agenda-skip-function #'my-org-agenda-skip-all-siblings-but-first)))
;;         ("h" "At home"
;;          ((agenda "" ((org-agenda-files '("~/org/home.org"))))
;;           (org-agenda-overriding-header "Home")
;;           (org-agenda-skip-function #'my-org-agenda-skip-all-siblings-but-first)))))

(defun my-org-agenda-skip-all-siblings-but-first ()
  "Skip all but the first non-done entry."
  (let (should-skip-entry)
    (unless (org-current-is-todo)
      (setq should-skip-entry t))
    (save-excursion
      (while (and (not should-skip-entry) (org-goto-sibling t))
        (when (org-current-is-todo)
          (setq should-skip-entry t))))
    (when should-skip-entry
      (or (outline-next-heading)
          (goto-char (point-max))))))

(defun org-current-is-todo ()
  (string= "TODO" (org-get-todo-state)))

(setq org-todo-keywords
      '((sequence "TODO" "|" "DONE" "CANCELED")))

(setq org-log-done nil)

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

;;;;;;;;;;;;;;;;;;
;; start server ;;
;;;;;;;;;;;;;;;;;;

;; Standard emacs server
(server-start)
