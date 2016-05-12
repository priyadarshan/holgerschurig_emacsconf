(defvar my-start-time (current-time)
  "Time when Emacs was started")


;; Don't load old .elc files when the .el file is newer
(setq load-prefer-newer t)
;; This sets up the load path so that we can override it
(package-initialize nil)
;; Override the packages with the git version of Org and other packages
(add-to-list 'load-path (concat user-emacs-directory "elisp"))
(add-to-list 'load-path (concat user-emacs-directory "git/org-mode/lisp"))
(add-to-list 'load-path (concat user-emacs-directory "git/org-mode/contrib/lisp"))

(setq inhibit-startup-screen t)

;; Just here for benchmark purposes, uncomment the rest of this file
;; if you use it:
;; (org-babel-load-file "config.org")

(defun my-tangle-config-org (orgfile elfile)
  "This function will write all source blocks from =config.org= into
=config.el= that are ...

- not marked as =tangle: no=
- doesn't have the TODO state =CANCELED=
- have a source-code of =emacs-lisp="
  (require 'org)
  (let* ((body-list ())
		 (gc-cons-threshold most-positive-fixnum)
		 (org-babel-default-header-args (org-babel-merge-params org-babel-default-header-args
																(list (cons :tangle elfile)))))
    (message "Writing %s ..." elfile)
    (save-restriction
      (save-excursion
        (org-babel-map-src-blocks orgfile
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
      (with-temp-file elfile
        (insert (format ";; Don't edit this file, edit %s instead ...\n\n" orgfile))
        (insert (apply 'concat (reverse body-list))))
      (message "Wrote %s ..." elfile))))

(let ((orgfile (concat user-emacs-directory "config.org"))
      (elfile (concat user-emacs-directory "config.el"))
      (gc-cons-threshold most-positive-fixnum))
  (when (or (not (file-exists-p elfile))
            (file-newer-than-file-p orgfile elfile))
    (message "Tangling %s into %s ..." orgfile elfile)
    (my-tangle-config-org orgfile elfile))
  (load-file elfile))


(message "Start up time %.2fs" (float-time (time-subtract (current-time) my-start-time)))
