(defconst emacs-d
  (file-name-directory
   (file-chase-links load-file-name))
  "The giant turtle on which the world rests.")

;; (setq package-user-dir
;;       (expand-file-name "elpa" emacs-d))
(package-initialize)
(setq package-archives
      '(("melpa-stable" . "http://stable.melpa.org/packages/")
        ("gnu" . "http://elpa.gnu.org/packages/")))
(package-refresh-contents)

(defconst my-packages
  '(use-package
     afternoon-theme
     powerline
     expand-region
     cycbuf
     unbound
     ace-jump-buffer
     avy
     org
     helm
     helm-descbinds
     savehist
     markdown-mode
     ;; rust-mode
     web-mode
     column-marker
     smooth-scrolling
     )
  "List of packages that I like.")

;; install required
(dolist (package my-packages)
  (unless (package-installed-p package)
    (ignore-errors
      (package-install package))))

;; upgrade installed
(save-window-excursion
  (package-list-packages t)
  (package-menu-mark-upgrades)
  (condition-case nil
      (package-menu-execute t)
    (error
     (package-menu-execute))))

nil
