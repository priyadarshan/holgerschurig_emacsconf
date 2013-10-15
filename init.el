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


(add-to-list 'load-path (mapconcat 'identity (file-expand-wildcards "elpa/org-20*" dotfiles-dir) ""))
(require 'ob-tangle)
(defun load-org-elisp (file &optional compile)
  "Load Emacs Lisp source code blocks in the Org-mode FILE.
This function exports the source code using `org-babel-tangle'
and then loads the resulting file using `load-file'.  With prefix
arg (noninteractively: 2nd arg) COMPILE the tangled Emacs Lisp
file to byte-code before it is loaded."
  (interactive "fFile to load: \nP")
  (require 'ob-core)
  (let* ((age (lambda (file)
                (float-time
                 (time-subtract (current-time)
                                (nth 5 (or (file-attributes (file-truename file))
                                           (file-attributes file)))))))
         (base-name (file-name-sans-extension file))
         (exported-file (concat base-name ".el")))
    ;; tangle if the org-mode file is newer than the elisp file
    (unless (and (file-exists-p exported-file)
                 (> (funcall age file) (funcall age exported-file)))
      (org-babel-tangle-file file exported-file "emacs-lisp"))
    (message "%s %s"
             (if compile
                 (progn (byte-compile-file exported-file 'load)
                        "Compiled and loaded")
               (progn (load-file exported-file) "Loaded"))
             exported-file)))


(load (concat dotfiles-dir "start.el"))

(load-org-elisp "emacs.org")
