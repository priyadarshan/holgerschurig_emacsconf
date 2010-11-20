;; Minimal startup file, the core is in start.el

(eval-after-load "byte-code-cache"
  '(setq bcc-cache-directory "~/.emacs.d/tmp/byte-cache"
	 bcc-blacklist '("/recentf\\.el$" "/history\\.el$")
	 byte-compile-warnings t
	 byte-compile-verbose nil))

;; HINT: byte-code-cache.el comes from EmacsWiki, you can update it
;; with (auto-install-from-emacswiki)
(load "~/.emacs.d/elisp/byte-code-cache.el")


;; HINT: fold.el comes from EmacsWiki, you can update it
;; with (auto-install-from-emacswiki)
(autoload 'fold-mode "fold" "Fold mode" t)
(load "~/.emacs.d/start.el")
