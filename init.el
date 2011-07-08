;; Minimal startup file, the core is in start.el

(setq dotfiles-dir (file-name-directory (or (buffer-file-name) load-file-name)))

(eval-after-load "byte-code-cache"
  '(setq bcc-cache-directory (concat dotfiles-dir "tmp/byte-cache")
	 bcc-blacklist '("/recentf\\.el$" "/history\\.el$" "/desktop\\.data$")
	 byte-compile-warnings t
	 byte-compile-verbose nil))

;; HINT: byte-code-cache.el comes from EmacsWiki, you can update it
;; with (auto-install-from-emacswiki)
(load (concat dotfiles-dir "elisp/byte-code-cache.el"))


;; HINT: fold.el comes from EmacsWiki, you can update it
;; with (auto-install-from-emacswiki)
(add-to-list 'load-path (concat dotfiles-dir "elisp/"))
(autoload 'fold-mode "fold" "Fold mode" t)
(load (concat dotfiles-dir "start.el"))
