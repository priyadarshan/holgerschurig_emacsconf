(defvar md-mode-hook nil)

(defvar md-font-lock-keywords ;; concat
      (list
       (eval-when-compile (concat "^"
				  (regexp-opt '("title" "linktitle" "keywords" "description"
						"parent" "links" "order"
						"sitemap_changefreq" "sitemap_priority"
						"ctime" "mtime" "change"
						"template" "hide"
						"main_url"
						"lang"
						) t)
				  ":")))
  "Expressions to highlight in MD mode.")

(defun md-header-end ()
  "Return the buffer location of the end of headers, as a number."
  (save-restriction
    (widen)
    (save-excursion
      (rfc822-goto-eoh)
      (point))))

(defun md-mode-auto-fill ()
  "Carry out Auto Fill for MD mode.
If within the headers, this makes the new lines into continuation lines."
  (if (< (point) (md-header-end))
      (let ((old-line-start (save-excursion (beginning-of-line) (point))))
        (if (do-auto-fill)
            (save-excursion
              (beginning-of-line)
              (while (not (eq (point) old-line-start))
                ;; Use insert-before-markers in case we're inserting
                ;; before the saved value of point (which is common).
                (insert-before-markers "   ")
                (forward-line -1))
              t)))
    (do-auto-fill)))

(defun md-mode-fill-paragraph (arg)
  ;; Do something special only if within the headers.
  (if (< (point) (md-header-end))
      (let (beg end fieldname)
        (when (prog1 (re-search-backward "^[-a-zA-Z]+:" nil 'yes)
                (setq beg (point)))
	  (setq fieldname
                (downcase (buffer-substring beg (1- (match-end 0))))))
        (forward-line 1)
        ;; Find continuation lines and get rid of their continuation markers.
        (while (looking-at "[ \t]")
          (delete-horizontal-space)
          (forward-line 1))
        (setq end (point-marker))
        (goto-char beg)
        ;; If this field contains addresses,
        ;; make sure we can fill after each address.
        (if (member fieldname
                    '("to" "cc" "bcc" "from" "reply-to"
                      "md-reply-to" "md-followup-to"
                      "resent-to" "resent-cc" "resent-bcc"
                      "resent-from" "resent-reply-to"))
            (while (search-forward "," end t)
              (or (looking-at "[ \t]")
                  (insert " "))))
        (fill-region-as-paragraph beg end arg)
        ;; Mark all lines except the first as continuations.
        (goto-char beg)
        (forward-line 1)
        (while (< (point) end)
          (insert "  ")
          (forward-line 1))
        (move-marker end nil)
        t)))


(define-derived-mode md-mode text-mode "MD"
  "Major mode for editing MD files"
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(md-font-lock-keywords t t))
  (make-local-variable 'normal-auto-fill-function)
  (setq normal-auto-fill-function 'md-mode-auto-fill)
  (make-local-variable 'adaptive-fill-regexp)
  (setq adaptive-fill-regexp
        (concat "[ \t]*[-[:alnum:]]+>+[ \t]*\\|"
                adaptive-fill-regexp))
  (make-local-variable 'adaptive-fill-first-line-regexp)
  (setq adaptive-fill-first-line-regexp
        (concat "[ \t]*[-[:alnum:]]*>+[ \t]*\\|"
                adaptive-fill-first-line-regexp))
  )

(provide 'md-mode)
