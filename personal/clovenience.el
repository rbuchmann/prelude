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

(defun clojure-goto-test-or-back ()
  (interactive)
  (let* ((current-file    (buffer-file-name))
         (current-project (locate-dominating-file current-file
                                                  "project.clj"))
         (test-p          (string-match "/test/" current-file))
         (rel-filename    (file-relative-name
                           current-file
                           (concat current-project
                                   (if test-p "test/" "src/"))))
         (outer-path      (concat current-project
                                  (if test-p "src/" "test/")))
         (target-file     (concat outer-path (file-name-sans-extension rel-filename)
                                  (if test-p ".clj" "_test.clj"))))
    (other-window 1)
    (if (or test-p
            (file-exists-p target-file)
            (find-buffer-visiting target-file))
        (find-file target-file)
      (progn (make-directory (file-name-directory target-file) t)
             (find-file target-file)))))
