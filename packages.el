(defconst emacs-d
  (file-name-directory
   (file-chase-links load-file-name))
  "My emacs dotfiles directory, ~/.emacs.d on Linux")

;; Use new byte codes from Emacs 24.4
(setq byte-compile--use-old-handlers nil)

(setq package-archives
      '(("melpa-stable" . "http://stable.melpa.org/packages/")
	("melpa"        . "http://melpa.org/packages/")
        ("gnu"          . "http://elpa.gnu.org/packages/")))

(setq package-pinned-packages
      '((use-package . "melpa-stable")
        (afternoon-theme  . "melpa")
        (auto-compile     . "melpa-stable")
        (ace-jump-buffer  . "melpa")
        (avy              . "melpa")
        (circe            . "melpa-stable")
        (column-marker    . "melpa")
        (expand-region    . "melpa-stable")
        (guide-key        . "melpa-stable")
        (iflipb           . "melpa-stable")
        (git-timemachine  . "melpa-stable")
        (helm             . "melpa")
        (helm-descbinds   . "melpa")
        (helm-flyspell    . "melpa")
        (helm-swoop       . "melpa")
        (markdown-mode    . "melpa-stable")
        (org              . "melpa-stable")
        (org-bullets      . "melpa-stable")
        (smart-mode-line  . "melpa")
        (smart-mode-line-powerline-theme . "melpa")
        (savehist         . "melpa-stable")
        (smooth-scrolling . "melpa-stable")
        (unbound          . "melpa-stable")
        (undo-tree        . "gnu")
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
(mapc 'my-install package-pinned-packages)

nil
