;; Minimal startup file, the core is in start.el

(load (expand-file-name "~/.emacs.d/elisp/byte-code-cache.el"))
(setq bcc-cache-directory "~/.emacs.d/tmp/byte-cache"
      bcc-blacklist '("/recentf\\.el$" "/history\\.el$")
      byte-compile-warnings t
      byte-compile-verbose t)

(load (expand-file-name "~/.emacs.d/start.el"))
