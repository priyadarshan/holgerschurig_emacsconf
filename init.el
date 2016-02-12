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

(let ((el-file (concat user-emacs-directory "config.el"))
      (gc-cons-threshold most-positive-fixnum))
  (if (file-exists-p el-file)
      (load-file (concat user-emacs-directory "config.el"))
    (progn
      (require 'cl)
      (org-babel-load-file (concat user-emacs-directory "config.org"))
      )))
(setq gc-cons-threshold (* 8 1024 1024))
