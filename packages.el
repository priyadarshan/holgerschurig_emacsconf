(defconst emacs-d
  (file-name-directory
   (file-chase-links load-file-name))
  "The giant turtle on which the world rests.")

(setq package-archives
      '(("melpa-stable" . "http://stable.melpa.org/packages/")
	("melpa"        . "http://melpa.org/packages/")
        ("gnu"          . "http://elpa.gnu.org/packages/")))

(setq package-pinned-packages
      '((use-package . "melpa-stable")
        (ace-jump-buffer  . "melpa")
        (avy              . "melpa")
        (column-marker    . "melpa")
        (cycbuf           . "melpa")
        (expand-region    . "melpa-stable")
        (helm             . "melpa-stable")
        (helm-descbinds   . "melpa-stable")
        (markdown-mode    . "melpa-stable")
        (org              . "melpa-stable")
        (powerline        . "melpa-stable")
        (savehist         . "melpa-stable")
        (smooth-scrolling . "melpa-stable")
        (swiper           . "gnu")
        (unbound          . "melpa-stable")
        (web-mode         . "melpa-stable")
	))

(package-initialize t)
(package-refresh-contents)

(defun my-install (p-alist)
  (let ((package (car p-alist)))
  (unless (package-installed-p package)
    (message "Installing %s" package)
    (ignore-errors
      (package-install package)))))
(mapcar 'my-install package-pinned-packages)

nil
