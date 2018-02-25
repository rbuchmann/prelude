;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Clojure Convenience Functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun clojure-ns->file (ns)
  (concat
   (replace-regexp-in-string "-" "_"
    (replace-regexp-in-string "[.]" "/" ns))
   ".clj"))

(defun clojure-file->ns (file)
  (replace-regexp-in-string "_" "-"
   (replace-regexp-in-string "\\.clj$" ""
    (replace-regexp-in-string "/" "." file))))

(defun clojure-add-ns ()
  (interactive)
  (let* ((current-project (locate-dominating-file (buffer-file-name)
                                                  "project.clj"))
         (prompt           (if current-project
                              (concat "Add ns to Project (default " current-project "): ")
                            "Add ns to Project: "))
         (target-project  (file-name-as-directory
                           (read-directory-name
                            prompt
                            (file-name-as-directory development-dir)
                            current-project)))
         (ns              (read-from-minibuffer "Namespace: "))
         (file            (concat target-project
                                  "src/"
                                  (clojure-ns->file ns)))
         (dir             (file-name-directory file)))
    (if (file-exists-p file)
        (find-file file)
      (progn (make-directory dir t)
             (find-file file)))))

(defun clojure-remove-extension (s)
  (let ((without-extension (file-name-sans-extension s)))
    (replace-regexp-in-string "_test$" "" without-extension)))

(defun clojure-goto-test-or-back ()
  (interactive)
  (let* ((current-file    (buffer-file-name))
         (current-project (locate-dominating-file current-file
                                                  "project.clj"))
         (test-p          (string-match "_test" current-file))
         (extension       (file-name-extension current-file))
         (rel-filename    (file-relative-name
                           current-file
                           (concat current-project
                                   (if test-p "test/" "src/"))))
         (outer-path      (concat current-project
                                  (if test-p "src/" "test/")))
         (target-file     (concat outer-path
                                  (clojure-remove-extension rel-filename)
                                  (if test-p "" "_test")
                                  "."
                                  extension)))
    (if (or test-p
            (file-exists-p target-file)
            (find-buffer-visiting target-file))
        (find-file-other-window target-file)
      (progn (make-directory (file-name-directory target-file) t)
             (find-file-other-window target-file)))))


(defun clojure-align-requires ()
  (save-excursion
    (catch 'exit
      (beginning-of-buffer)
      (search-forward ":require "
                      nil
                      (lambda ()
                        (throw 'exit t)))
      (let ((beg (point)))
        (paredit-close-round)
        (let ((end (point)))
          (align-regexp beg end "\\(\\s-*\\):\\(as\\|refer\\)"))))))

;;; TODO: cljr-sort-ns has gone, need to reimplement this stuff

;; (eval-after-load "clj-refactor"

;; ;; hack, wait for clj-refactor updates to make it better
;;   '(defun cljr--search-forward-within-sexp (s &optional save-excursion)
;;      "Searches forward for S in the current sexp.
;;       S must be followed by a space, ), or ] character.
;;       If SAVE-EXCURSION is T POINT does not move."
;;      (let ((bound (save-excursion (forward-list 1) (point)))
;;            (search-regex (concat s "[] )]")))
;;        (cl-flet ((do-search ()
;;                             (when (search-forward-regexp search-regex bound t)
;;                               (backward-char)
;;                               (point))))
;;          (if save-excursion
;;              (save-excursion
;;                (do-search))
;;            (do-search))))))

(defun clojure-cleanup-ns ()
  (interactive)
  (when (or
         (eq major-mode 'clojure-mode)
         (eq major-mode 'clojurec-mode)
         (eq major-mode 'clojurescript-mode))
    ;; (cljr-sort-ns)
    (clojure-align-requires)))

(add-hook 'before-save-hook
          'clojure-cleanup-ns)

(defun cider-start-browser-repl ()
  "Start a browser repl"
  (interactive)
  (remove-hook 'cider-connected-hook 'cider-start-browser-repl)
  (let ((buffer (cider-current-repl-buffer)))
    (cider-eval "(do (use 'figwheel-sidecar.repl-api)
                     (cljs-repl))"
                (lambda (x)
                  (message "connected"))
                (cider-current-connection)
                (cider-current-session))))

(defun clojurescript-repl ()
  (interactive)
  ;(add-hook 'cider-connected-hook 'cider-start-browser-repl)
  (cider-connect "localhost" 7888))

(setq cider-prompt-for-project-on-connect nil)

(add-to-list 'projectile-globally-ignored-directories "target")
(add-to-list 'projectile-globally-ignored-directories "out")
(add-to-list 'projectile-globally-ignored-directories "node_modules")

(add-to-list 'auto-mode-alist '("\\.trq\\'" . clojure-mode))
