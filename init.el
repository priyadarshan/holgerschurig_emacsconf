;; Minimal startup file, the core is in start.el

(eval-after-load "byte-code-cache"
  '(setq bcc-cache-directory "~/.emacs.d/tmp/byte-cache"
	 bcc-blacklist '("/recentf\\.el$" "/history\\.el$")
	 byte-compile-warnings t
	 byte-compile-verbose nil))
(load "~/.emacs.d/elisp/byte-code-cache.el")

(autoload 'fold-mode "fold" "Fold mode" t)
(load "~/.emacs.d/start.el")
