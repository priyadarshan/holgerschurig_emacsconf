;; print the key bindings in a tabular form
;; [from http://www-xray.ast.cam.ac.uk/~gmorris/dotemacs.html]
(defun my-keytable (arg)
  "Print the key bindings in a tabular form."
  (interactive "sEnter a modifier string:")
  (with-output-to-temp-buffer "*Key table*"
    (let* ((i 0)
	   (keys (list "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n"
		       "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"
		       "<return>" "<down>" "<up>" "<right>" "<left>"
		       "<home>" "<end>" "<f1>" "<f2>" "<f3>" "<f4>" "<f5>"
		       "<f6>" "<f7>" "<f8>" "<f9>" "<f10>" "<f11>" "<f12>"
		       "1" "2" "3" "4" "5" "6" "7" "8" "9" "0"
		       "`" "~" "!" "@" "#" "$" "%" "^" "&" "*" "(" ")" "-" "_"
		       "=" "+" "\\" "|" "{" "[" "]" "}" ";" "'" ":" "\""
		       "<" ">" "," "." "/" "?"))
	   (n (length keys))
	   (modifiers (list "" "S-" "C-" "M-" "M-C-"))
	   (k))
      (or (string= arg "") (setq modifiers (list arg)))
      (setq k (length modifiers))
      (princ (format " %-10.10s |" "Key"))
      (let ((j 0))
	(while (< j k)
	  (princ (format " %-28.28s |" (nth j modifiers)))
	  (setq j (1+ j))))
      (princ "\n")
      (princ (format "_%-10.10s_|" "__________"))
      (let ((j 0))
	(while (< j k)
	  (princ (format "_%-28.28s_|"
			 "_______________________________"))
	  (setq j (1+ j))))
      (princ "\n")
      (while (< i n)
	(princ (format " %-10.10s |" (nth i keys)))
	(let ((j 0))
	  (while (< j k)
	    (let* ((binding
		    (key-binding (read-kbd-macro (concat (nth j modifiers)
							 (nth i keys)))))
		   (binding-string "_"))
	      (when binding
		(if (eq binding 'self-insert-command)
		    (setq binding-string (concat "'" (nth i keys) "'"))
		  (setq binding-string (format "%s" binding))))
	      (setq binding-string
		    (substring binding-string 0 (min (length
						      binding-string) 28)))
	      (princ (format " %-28.28s |" binding-string))
	      (setq j (1+ j)))))
	(princ "\n")
	(setq i (1+ i)))
      (princ (format "_%-10.10s_|" "__________"))
      (let ((j 0))
	(while (< j k)
	  (princ (format "_%-28.28s_|"
			 "_______________________________"))
	  (setq j (1+ j))))))
  (delete-window)
  (hscroll-mode)
  (setq truncate-lines t))
