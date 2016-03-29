;; Don't load old .elc files when the .el file is newer
(setq load-prefer-newer t)
;; This sets up the load path so that we can override it
(package-initialize nil)
;; Override the packages with the git version of Org and other packages
(add-to-list 'load-path (concat user-emacs-directory "elisp"))
(add-to-list 'load-path (concat user-emacs-directory "git/org-mode/lisp"))
;; Load the rest of the packages
(package-initialize nil)
(setq package-enable-at-startup nil)

(setq inhibit-startup-screen t)

(defun my-tangle-config-org ()
  "This function will write all source blocks from =config.org= into
=config.el= that are ...

- not marked as =tangle: no=
- doesn't have the TODO state =CANCELLED=
- have a source-code of =emacs-lisp="
  (require 'org)
  (let* ((body-list ())
         (output-file "config.el")
         (org-babel-default-header-args (org-babel-merge-params org-babel-default-header-args
                                                                (list (cons :tangle output-file)))))
    (message "Writing %s ..." output-file)
    (save-restriction
      (save-excursion
        (org-babel-map-src-blocks "config.org"
                                  (let* ((info (org-babel-get-src-block-info 'light))
                                         (tfile (cdr (assq :tangle (nth 2 info))))
                                         (match))
                                    (save-excursion
                                      (catch 'exit
                                        (org-back-to-heading t)
                                        (when (looking-at org-outline-regexp)
                                          (goto-char (1- (match-end 0))))
                                        (when (looking-at (concat " +" org-todo-regexp "\\( +\\|[ \t]*$\\)"))
                                          (setq match (match-string 1)))))
                                    (unless (or (string= "no" tfile)
                                                (string= "CANCELED" match)
                                                (not (string= "emacs-lisp" lang)))
                                      (add-to-list 'body-list body)))))
      (with-temp-file output-file
        (insert ";; Don't edit this file, edit config.org' instead ...\n\n")
        (insert (apply 'concat (reverse body-list))))
      (message "Wrote %s ..." output-file))))

(let ((el-file (concat user-emacs-directory "config.el"))
      (gc-cons-threshold most-positive-fixnum))
  (unless (file-exists-p el-file)
    (my-tangle-config-org))
  (load-file (concat user-emacs-directory "config.el")))
(setq gc-cons-threshold (* 8 1024 1024))
