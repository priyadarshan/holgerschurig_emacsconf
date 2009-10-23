;; -*- mode: emacs-lisp; mode: fold -*-

;; SEE: http://github.com/mina86/dot-files/raw/master/dot-emacs
;; SEE: http://www.emacswiki.org/emacs/download/.emacs-thierry.el
;; SEE: http://www.jurta.org/en/emacs/dotemacs
;; SEE: http://www.djcbsoftware.nl/dot-emacs.html



;;{{{ Debugging

;; Provide a useful error trace if loading this monster fails
;(setq debug-on-error t)
(defun debug-on-error ()
  "Turn on debug on error"
  (interactive "P")
  (setq debug-on-error t))



;;}}}
;;{{{ OS - Environment

(defconst ms-windows (equal window-system 'w32))


;;}}}
;;{{{ Load path

(dolist (i '(
	     "~/.emacs.d/elisp/"
	     "~/.emacs.d/magit/"
	     "~/.emacs.d/elpa/"
	     "~/.emacs.d/"
	   ))
  (when (not (member i load-path))
    (add-to-list 'load-path (expand-file-name i))))



;;}}}
;;{{{ Byte compile

;; This has been known to break some packages, so use with care.

(require 'byte-code-cache)
(eval-after-load "byte-code-cache"
  '(setq bcc-cache-directory "~/.emacs.d/tmp/byte-cache"
	 bcc-blacklist '("/recentf\\.el$" "/history\\.el$")))
(setq byte-compile-warnings t)
(setq byte-compile-verbose t)




;;}}}
(load "startup")
