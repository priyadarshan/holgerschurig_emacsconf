;;; My home directory
(setq dotfiles-dir (file-name-directory (or (buffer-file-name) load-file-name)))




;;; Debugging
(setq message-log-max 10000)




;;; Byte-Code cache
(eval-after-load "byte-code-cache"
  '(setq bcc-cache-directory (concat dotfiles-dir "tmp/byte-cache")
        bcc-blacklist '("/recentf\\.el$" "/history\\.el$" "/desktop\\.data$")
        byte-compile-warnings t
        byte-compile-verbose nil))

;; HINT: byte-code-cache.el comes from EmacsWiki, you can update it
;; with (auto-install-from-emacswiki)
; TODO (load (concat dotfiles-dir "elisp/byte-code-cache.el"))




;;; Package management

;; Please don't load outdated byte code
(setq load-prefer-newer t)

;; ELPA might use Emacs-W3 to get files, and this in turn sets cookies.
;; Move the cookie file out into the =tmp/= directory.
(setq url-configuration-directory (concat dotfiles-dir "tmp/"))

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)




;;; Load emacs.org

;; Some values must be set before emacs.org auto-loads org.el ...
(setq ;; Only load these org modules:
      org-modules (;; 'org-bbdb
		   ;; 'org-bibtex
		   ;; 'org-docview
		   ;; 'org-gnus
		   ;; 'org-info
		   ;; 'org-jsinfo
		   ;; 'org-irc
		   ;; 'org-mew
		   ;; 'org-mhe
		   ;; 'org-rmail
		   ;; 'org-vm
		   ;; 'org-w3m
		   ;; 'org-wl
		   )
      org-replace-disputed-keys t)

(org-babel-load-file (concat dotfiles-dir "holger.org"))
