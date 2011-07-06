;; -*- mode: emacs-lisp; mode: fold -*-
;; toggle entries with C-t

;;{{{ Debugging

;; Provide a useful error trace if loading this monster fails
					;(setq debug-on-error t)
(defun debug-on-error ()
  "Turn on debug on error"
  (interactive)
  (setq debug-on-error t))



;;}}}
;;{{{ OS - Environment

(defconst ms-windows (equal window-system 'w32))


;;}}}
;;{{{ Load path

(add-to-list 'load-path dotfiles-dir)
(add-to-list 'load-path (concat dotfiles-dir "elisp/"))
(add-to-list 'load-path (concat dotfiles-dir "elpa/"))



;;}}}
;;{{{ Functions: Moving cursor

;; http://www.emacswiki.org/cgi-bin/wiki/DoubleKeyBinding
(defvar seq-times 0
  "Stores number of times command was executed.  It cotnains
random data before `seq-times' macro is called.")

(defmacro seq-times (&optional name max &rest body)
  "Returns number of times command NAME was executed and updates
`seq-times' variable accordingly.  If NAME is nil `this-command'
will be used.  If MAX is specified the counter will wrap around
at the value of MAX never reaching it.  If body is given it will
be evaluated if the command is run for the first time in a
sequence."
  (declare (indent 2))

  ;; Build incrementation part
  (setq max (cond ((null max) '(setq seq-times (1+ seq-times)))
		  ((atom max) (if (and (integerp max) (> max 0))
				  `(setq seq-times (% (1+ seq-times) ,max))
				'(setq seq-times (1+ seq-times))))
		  (t          `(let ((max ,max))
				 (if (and (integerp max) (> max 0))
				     (setq seq-times (% (1+ seq-times) max))
				   (setq seq-times (1+ seq-times)))))))

  ;; Make macro
  (if (eq name 'last-command)
      max
    (cond ((null  name) (setq name 'this-command))
	  ((consp name) (setq name `(or ,name this-command))))
    `(if (eq last-command ,name)
	 ,max
       ,@body
       (setq seq-times 0))))

(defmacro seq-times-nth (name body &rest list)
  "Calls `seq-times' with arguments NAME, length and BODY
and (where length is the number of elements in LIST) then returns
`seq-times'th element of LIST."
  (declare (indent 2))
  `(nth (seq-times ,name ,(length list) ,body) ',list))

(defmacro seq-times-do (name body &rest commands)
  "Calls `seq-times' with arguments NAME, length and BODY (where
length is the number of COMMANDS) and then runs `seq-times'th
command from COMMANDS."
  (declare (indent 2))
  `(eval (nth (seq-times ,name ,(length commands) ,body) ',commands)))

;; My home
(defvar my--previous-position 0)
(defun my-home ()
  "Depending on how many times it was called moves the point to:

* begin of indentation
* beginning of line
* begin of function
* beginning of buffer
* back to where it was"
  (interactive)
  (seq-times-do nil (setq my--previous-position (point))
    (back-to-indentation)
    (beginning-of-line)
    (beginning-of-defun)
    (goto-char (point-min))
    (goto-char my--previous-position)))
(substitute-key-definition 'move-beginning-of-line 'my-home (current-global-map))

;; My end
(defun my-end ()
  "Depending on how many times it was called moves the point to:

* end of line
* end of function
* end of buffer
* back to where it was"
  (interactive)
  (seq-times-do nil (setq my--previous-position (point))
    ;;(if folding-mode (folding-end-of-line) (end-of-line))
    (end-of-line)
    (forward-paragraph)
    (end-of-defun)
    (goto-char (point-max))
    (goto-char my--previous-position)))
(substitute-key-definition 'move-end-of-line 'my-end (current-global-map))

;; Centering
;;
;; This is built-in into Emacs 23, but doesn't work as nice,
;; because they have a different ordering of end-of-buffer,
;; start-of-buffer.

(defun my-recenter ()
  "Depending on how many times it was called moves the point to:

* center of buffer
* end of buffer
* start of buffer
* back to where it was"
  (interactive)
  (let ((i 0) (old (window-start)))
    (while (and (<= (setq i (1+ i)) 6) (equal (window-start) old))
      (seq-times-do nil (setq my--previous-position (window-start))
	(recenter)
	(recenter -1)
	(recenter 0)
	(set-window-start (selected-window) my--previous-position)))))
(substitute-key-definition 'recenter-top-bottom 'my-recenter (current-global-map))

;; Nicer scroll handling
(setq scroll-conservatively 1000000
      scroll-preserve-screen-position 1)


;; Like goto-line, but doesn't modify minibuffer-history, but use it's
;; own little history list.

(setq my-goto-line-history '())
(defun my-goto-line (line &optional buffer)
  "Goto LINE, counting from line 1 at beginning of buffer.
Normally, move point in the current buffer, and leave mark at the
previous position.  With just \\[universal-argument] as argument,
move point in the most recently selected other buffer, and switch to it.

If there's a number in the buffer at point, it is the default for LINE.

This function is usually the wrong thing to use in a Lisp program.
What you probably want instead is something like:
  (goto-char (point-min)) (forward-line (1- N))
If at all possible, an even better solution is to use char counts
rather than line counts."
  (interactive
   (if (and current-prefix-arg (not (consp current-prefix-arg)))
       (list (prefix-numeric-value current-prefix-arg))
     ;; Look for a default, a number in the buffer at point.
     (let* ((default
	      (save-excursion
		(skip-chars-backward "0-9")
		(if (looking-at "[0-9]")
		    (buffer-substring-no-properties
		     (point)
		     (progn (skip-chars-forward "0-9")
			    (point))))))
	    ;; Decide if we're switching buffers.
	    (buffer
	     (if (consp current-prefix-arg)
		 (other-buffer (current-buffer) t)))
	    (buffer-prompt
	     (if buffer
		 (concat " in " (buffer-name buffer))
	       "")))
       ;; Read the argument, offering that number (if any) as default.
       (list (read-from-minibuffer (format (if default "Goto line%s (%s): "
					     "Goto line%s: ")
					   buffer-prompt
					   default)
				   nil nil t
				   'my-goto-line-history
				   default)
	     buffer))))
  ;; Switch to the desired buffer, one way or another.
  (if buffer
      (let ((window (get-buffer-window buffer)))
	(if window (select-window window)
	  (switch-to-buffer-other-window buffer))))
  ;; Leave mark at previous position
  (or (region-active-p) (push-mark))
  ;; Move to the specified line number in that buffer.
  (save-restriction
    (widen)
    (goto-char (point-min))
    (if (eq selective-display t)
	(re-search-forward "[\n\C-m]" nil 'end (1- line))
      (forward-line (1- line)))))

(global-set-key (kbd "M-g g") 'my-goto-line)
(global-set-key (kbd "M-g M-g") 'my-goto-line)



;;}}}
;;{{{ Functions: Yank and Delete

;; The following may be of interest to people who (a) are happy with
;; "C-w" and friends for killing and yanking, (b) use
;; "transient-mark-mode", (c) also like the traditional Unix tty
;; behaviour that "C-w" deletes a word backwards and (d) use
;; GnuEmacs. It tweaks "C-w" so that, if the mark is inactive, it
;; deletes a word backwards instead of killing the region:
;; http://www.emacswiki.org/cgi-bin/wiki/DefaultKillingAndYanking
(defadvice kill-region (before unix-werase activate compile)
  "When called interactively with no active region, delete a single word
    backwards instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (save-excursion (backward-word 1) (point)) (point)))))

;; Deleting past a tab changes tab into spaces
(setq backward-delete-char-untabify-method nil)

;; Use delete-selection mode.
(delete-selection-mode t)

;; Be silent when killing text from RO buffer
(setq kill-read-only-ok t)

;; Delete annoying spaces when kill-line at end of line and the
;; next line is indented
(defun kill-and-join-forward (&optional arg)
  "If at end of line, join with following; otherwise kill line.
Deletes whitespace at join."
			     (interactive "P")
			     (if (and (eolp) (not (bolp)))
				 (delete-indentation t)
			       (kill-line arg)))

(global-set-key (kbd "C-k") 'kill-and-join-forward)
;; ORIGINAL: deleteline

(defun my-yank (&optional arg)
  "Reinsert (\"paste\") the last stretch of killed text.
More precisely, reinsert the stretch of killed text most recently
killed OR yanked. Put mark at end, and set point at
beginning (the opposite of `yank'). With just
\\[universal-argument] as argument, same but put mark at
beginning (and point at end). With argument N, reinsert the Nth
most recently killed stretch of killed text.

When this command inserts killed text into the buffer, it honors
`yank-excluded-properties' and `yank-handler' as described in the
doc string for `insert-for-yank-1', which see.

See also the command `yank-pop' (\\[yank-pop])."
  (interactive "*P")
  (setq yank-window-start (window-start))
  ;; If we don't get all the way thru, make last-command indicate that
  ;; for the following command.
  (setq this-command t)
  (push-mark (point))
  (insert-for-yank (current-kill (cond
				  ((listp arg) 0)
				  ((eq arg '-) -2)
				  (t (1- arg)))))
  (unless (consp arg)
      ;; This is like exchange-point-and-mark, but doesn't activate the mark.
      ;; It is cleaner to avoid activation, even though the command
      ;; loop would deactivate the mark because we inserted text.
      (goto-char (prog1 (mark t)
		   (set-marker (mark-marker) (point) (current-buffer)))))
  ;; If we do get all the way thru, make this-command indicate that.
  (if (eq this-command t)
      (setq this-command 'yank))
  nil)

(global-set-key "\C-y" 'my-yank)
;; ORIGNAL: yank


;; http://www.reddit.com/r/emacs/comments/b1r8a/remacs_tell_us_about_the_obscure_but_useful/
(defun delete-char-dynamic (&optional arg)
  "If at end of line, intelligently join to the following;
otherwise delete."
  (interactive "p")
  (if (or (not (eolp)) (bolp))
      (delete-char arg)
    (let ((start (point))
          (in-comment (eq (get-text-property (point) 'face)
                          'font-lock-comment-face)))
      (forward-char)
      (skip-chars-forward " \  ")
      (if (and in-comment (looking-at comment-start-skip))
          (goto-char (match-end 0)))
      (delete-region start (point))
      (when (and (not (eolp))
                 (/= (char-before) ? )
                 (/= (char-before) ?\  ))
        (insert-char ?  1)
        (backward-char)))))
;; Make delete-selection-mode work with it
(put 'delete-char-dynamic 'delete-selection 'supersede)

;; Rebind DELETE and friends to our version
(define-key global-map [(deletechar)] 'delete-char-dynamic)
(define-key global-map [(delete)] 'delete-char-dynamic)
(define-key global-map [(control ?d)] 'delete-char-dynamic)
;; ORIGINAL: delete-char



;;}}}
;;{{{ Functions: Indentation

;; Let yanked text immediately be indented
;; http://www.emacswiki.org/emacs/AutoIndentation
(defadvice yank (after indent-region activate)
  (if (member major-mode
              '(emacs-lisp-mode scheme-mode lisp-mode
                                c-mode c++-mode objc-mode
                                latex-mode plain-tex-mode))
      (let ((mark-even-if-inactive t))
        (indent-region (region-beginning) (region-end) nil))))

(defadvice yank-pop (after indent-region activate)
  (if (member major-mode
              '(emacs-lisp-mode scheme-mode lisp-mode
                                c-mode c++-mode objc-mode
                                latex-mode plain-tex-mode))
      (let ((mark-even-if-inactive t))
	(indent-region (region-beginning) (region-end) nil))))

;; Swap RET and \C-j (newline and newline-and-indent)
(global-set-key (kbd "RET") 'newline-and-indent)
;; ORIGINAL: newline

(global-set-key (kbd "\C-j") 'newline)
;; ORIGINAL: newline-and-indent



;;}}}
;;{{{ Functions: Searching

(setq ;; Scrolling while searching
      isearch-allow-scroll t

      ;; Save Isearch stuff
      isearch-resume-in-command-history t)
(define-key isearch-mode-map (kbd "C-y") 'isearch-yank-kill)
;; ORIGINAL: isearch-yank-line


;; Prompts you for an expression, defaulting to the symbol that your
;; cursor is on, and greps for that in the current directory and all
;; subdirectories.
(defun my-grep ()
  "grep the whole directory for something defaults to term at cursor position"
  (interactive)
  (let ((default (thing-at-point 'symbol)))
    (let ((needle (or (read-string (concat "grep for '" default "': ")) default)))
      (setq needle (if (equal needle "") default needle))
      (grep (concat "egrep -s -i -n -r " needle " *")))))

(global-set-key (kbd "C-x g") 'my-grep)
;; ORIGINAL: undefined


(defun isearch-occur ()
  "Invoke `occur' from within isearch."
  (interactive)
  (let ((case-fold-search isearch-case-fold-search))
    (occur (if isearch-regexp isearch-string (regexp-quote isearch-string))))
  (pop-to-buffer "*Occur*"))

(define-key isearch-mode-map (kbd "C-o") 'isearch-occur)
;; ORIGINAL: isearch-other-meta-char



;;}}}
;;{{{ Functions: Miscelleanous

(defun dos2unix()
  "convert dos (^M) end of line to unix end of line"
  (interactive)
  (goto-char(point-min))
  (while (search-forward "\r" nil t) (replace-match "")))

; Fri,  1 Dec 2006 15:41:36 +0100
(defun 822date ()
  "Insert date at point format the RFC822 way."
  (interactive)
  (insert (format-time-string "%a, %e %b %Y %H:%M:%S %z")))


;;}}}
;;{{{ Functions: Windows

;; http://www.emacswiki.org/emacs/frame-cmds.el

;; Deleting frames ("X11-Windows") isn't as easy as it should
;;
;; Override delete-window to actually delete the frame if the buffer is
;; the only currently displayed buffer.

(defadvice delete-window (around delete-window (&optional window) activate)
  (interactive)
  (save-current-buffer
    (setq window (or window (selected-window)))
    (select-window window)
    (if (one-window-p t)
	(delete-frame)
      ad-do-it (selected-window))))


;; Candidate as replacement for `kill-buffer', at least when used
;; interactively. Should not just redefine `kill-buffer', because some
;; programs count on a specific other buffer taking the place of the
;; killed buffer (in the window).
(defun my--kill-buffer-and-window (&optional buffer)
  "Kill buffer BUFFER-OR-NAME.
The argument may be a buffer or the name of an existing buffer.
Argument nil or omitted means kill the current buffer. Return t
if the buffer is actually killed, nil otherwise.

Unlike `kill-buffer', this also will delete the current window if
there are several windows open."
  (interactive)
  (setq buffer (or buffer (current-buffer)))
  (unless (one-window-p)
    (delete-window))
  (kill-buffer buffer)
  ;; TODO: only delete the frame if it isn't the first one
  ;; (when (> (length (frame-list)) 1)
  ;;   (delete-frame))
  )

(global-set-key "\C-xk" 'my--kill-buffer-and-window)
;; ORIGINAL: kill-buffer


(global-set-key (kbd "<M-down>") 'enlarge-window)
;; ORIGINAL: undefined

(global-set-key (kbd "<M-up>") 'shrink-window)
;; ORIGINAL: undefined


(defun my-zoom-next-buffer2 ()
  (let ((curbuf (current-buffer))
	(firstbuf nil))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
	;(princ (format "name %s, fn %s\n" (buffer-name) buffer-file-name))
	(unless (or
		 ;; Don't mention internal buffers.
		 (string= (substring (buffer-name) 0 1) " ")
		 ;; No buffers without files.
		 (not buffer-file-name)
		 ;; Skip the current buffer
		 (eq buffer curbuf)
		 )
	  ;(princ (format " nme %s, fn %s\n" (buffer-name) buffer-file-name))
	  (unless firstbuf
	    (setq firstbuf buffer))
	    ;;(print buffer)
	  )))
    (when firstbuf
      ;(princ (format "new buffer: %s.\n" firstbuf))
      (bury-buffer)
      (switch-to-buffer firstbuf)
      )
  )
)

(defun my-explode-window ()
  "If there is only one window displayed, act like C-x2. If there
are two windows displayed, act like C-x1:"
  (interactive)
  (if (one-window-p t)
      (progn
	(split-window-vertically)
	(other-window 1)
	(my-zoom-next-buffer2)
	(other-window -1))
    (delete-other-windows)
  ))

(global-set-key [(f5)] 'my-explode-window)
;; ORIGINAL: undefined



;;}}}
;;{{{ Functions: Buffers

;; http://www.emacswiki.org/emacs/mybuffers.el
(defvar mybuffers-repetitions 0
  "Number of times `mybuffers-switch' was repeated.")

(defvar mybuffers-list ()
  "List of non-special buffers open.")

(defun mybuffers-filter-buffers (filter-function)
  "Returns a list of buffers that match FILTER-FUNCTION."
  (delq nil
        (mapcar (lambda (buffer)
                  (if (funcall filter-function buffer) buffer nil))
                (buffer-list))))

(defun mybuffers-special-buffer-p (buffer)
  "Returns t if BUFFER is one of the special buffers, `nil' otherwise.
A special buffer is one whose name starts with an asterisk. And `TAGS'."
  (let ((name (buffer-name buffer)))
    (or (string-match "^ ?\\*" name)
        (equal "TAGS" name))))

(defun mybuffers-normal-buffer-p (buffer)
  "This is the complement of `mybuffers-special-buffer-p'."
  (not (mybuffers-special-buffer-p buffer)))

(defun mybuffers-reorder-buffer-list (new-list)
  "Reorder buffer list using NEW-LIST."
  (while new-list
    (bury-buffer (car new-list))
    (setq new-list (cdr new-list))))

(defun mybuffers-swap (the-list i j)
  "Swap I and J elements in THE-LIST."
  (let ((tmp (nth j the-list))
        (vec (vconcat the-list)))
    (aset vec i tmp)
    (aset vec j (nth i the-list))
    (append vec nil)))

(defun mybuffers-rotate-next (the-list)
  "Delete first elem in THE-LIST and append it to the end."
  (append (cdr the-list) (list (car the-list))))

(defun mybuffers-rotate-prev (the-list)
  "Delete last elem in THE-LIST and append it to the start."
  (append (last the-list) (butlast the-list)))

(defun mybuffers--switch (dir-next)
  "Switch to buffer in my buffer list.
You should bind this function to Ctrl-Tab or something."
  (interactive)
  ;; if the last command wasn't a switch buffer, reset
  (when (not (or (eq last-command 'mybuffers-switch-next)
		 (eq last-command 'mybuffers-switch-prev)))
    (setq mybuffers-repetitions 0
          mybuffers-list (mybuffers-filter-buffers 'mybuffers-normal-buffer-p)))
  ;; if the current buffer is not a special buffer
  (when (not (mybuffers-special-buffer-p (current-buffer)))
    (setq mybuffers-repetitions (1+ mybuffers-repetitions))
    ;; swap or rotate
    (if (< mybuffers-repetitions (length mybuffers-list))
        (setq mybuffers-list (mybuffers-swap mybuffers-list 0 mybuffers-repetitions))
      (setq mybuffers-list (if dir-next
			       (mybuffers-rotate-next mybuffers-list)
			     (mybuffers-rotate-prev mybuffers-list)))
      (setq mybuffers-repetitions 0)))
  ;; switch to 1st buffer
  (switch-to-buffer (elt mybuffers-list 0))
  (mybuffers-reorder-buffer-list
   (append mybuffers-list
	   (mybuffers-filter-buffers 'mybuffers-special-buffer-p))))

(defun mybuffers-switch-next ()
  "Switch to next buffer."
  (interactive)
  (mybuffers--switch t))

(defun mybuffers-switch-prev ()
  "Switch to previous buffer."
  (interactive)
  (mybuffers--switch nil))


;; http://www.xsteve.at/prg/emacs/bubble-buffer.el
(defvar bubble-buffer-max-display-length (- (frame-width) 5)
  "Maximum number of characters to display in the minibuffer when bubbling.")

(defvar bubble-buffer-omit-regexp "\\*"
  "Regexp for buffer-names that should be skipped when bubbling buffers with
bubble-buffer-next and bubble-buffer-previous.
For example you could use \"\\\\*.+\\\\*\" to exclude all buffers that contain two *'s.")

(defun bubble-buffer-omit-buffer (buffer)
  "return nil if the buffer should be omitted, otherwise the buffer name"
  (let ((buf-name (buffer-name buffer)))
    (unless (and bubble-buffer-omit-regexp (string-match bubble-buffer-omit-regexp buf-name))
      buf-name)))


(defun bubble-buffer-next-2()
  (if (not (eq last-command 'bubble-buffer-next))
      (progn (setq bubble-buffer-list (copy-alist (buffer-list)))
             (delq (get-buffer " *Minibuf-0*") bubble-buffer-list)
             (delq (get-buffer " *Minibuf-1*") bubble-buffer-list)
             (setq bubble-buffer-buried-list nil)))
  (let* ((cur-buf (current-buffer))
         (b-list (delq nil (mapcar 'bubble-buffer-omit-buffer (cdr bubble-buffer-list))))
         (doit b-list)
         (rest nil)
         (s))
    (while doit
      (add-to-list 'bubble-buffer-buried-list (car bubble-buffer-list))
      (bury-buffer (car bubble-buffer-list))
      (setq bubble-buffer-list (cdr bubble-buffer-list))
      (switch-to-buffer (car bubble-buffer-list))
      (setq rest (cdr (copy-alist bubble-buffer-list)))
      (while rest
        (bury-buffer (car rest))
        (setq rest (cdr rest)))
      (setq doit (not (bubble-buffer-omit-buffer (current-buffer)))))
    ;;(message "%S" bubble-buffer-list)
    (if b-list
        (progn
          (setq b-list (cdr b-list))
          (setq s (concat
                   "Next: "
                   (if b-list (format "%S" b-list "") "")
                   "[end]"))
          (message "%s" (concat
			 (substring s 0 (min bubble-buffer-max-display-length (length s)))
			 " ...")))
      (message "Already at the end of the buffer-list"))))


(defun bubble-buffer-previous-2()
  (unless (eq last-command 'bubble-buffer-next)
    (setq bubble-buffer-buried-list nil))
  (setq this-command 'bubble-buffer-next)
  (if bubble-buffer-buried-list
      (progn
        (let ((doit t)
              (s)
              (b-list))
          (while doit
            (add-to-list 'bubble-buffer-list (car bubble-buffer-buried-list))
            (switch-to-buffer (car bubble-buffer-buried-list))
            (setq bubble-buffer-buried-list (cdr bubble-buffer-buried-list))
            (setq doit (not (bubble-buffer-omit-buffer (current-buffer))))))
        (setq b-list (delq nil (mapcar 'bubble-buffer-omit-buffer bubble-buffer-buried-list)))
        (setq s (concat
                 "Prev: "
                 (if b-list (format "%S" b-list "") "")
                 "[start]"))
        (message "%s" (concat
		       (substring s 0 (min bubble-buffer-max-display-length (length s))) " ...")))
    (message "Already at the start of the bubble-buffer-list")))


(defun bubble-buffer-next()
  "If you have only one window open, then bubble down one entry in the buffer list.
Switch to the next buffer on the list.

If more than one window is open, then just move to the next one."
  (interactive)
  (if (eq (count-windows) 1)
      (bubble-buffer-next-2)
    (other-window 1)
    ))


(defun bubble-buffer-previous()
  "If you have only one window open, then undo one bubbling step from bubble-buffer-next.
Switch to the buffer before the bubbled up buffer in the buffer list

If more than one window is open, then just move to the previous one."
  (interactive)
  (if (eq (count-windows) 1)
      (bubble-buffer-prev-2)
    (other-window -1)
    ))


(global-set-key [(f6)] 'bubble-buffer-next)
(global-set-key [(shift f6)] 'bubble-buffer-previous)


;; Insert buffer at current position
(global-set-key "\C-xI" 'insert-buffer)
;; ORIGINAL: undefined


;; Protect *scratch*
;; http://www.emacswiki.org/emacs/ProtBuf

(defvar protect-buffer-from-kill-mode nil
  "*If non-`nil', then prevent buffer from being accidentally killed.
This variable is local to all buffers.")
(progn
  (make-variable-buffer-local 'protect-buffer-from-kill-mode)
  (put 'protect-buffer-from-kill-mode 'permanent-local t)
  (or (assq 'protect-buffer-from-kill-mode minor-mode-alist)
      (setq minor-mode-alist (cons '(protect-buffer-from-kill-mode " prot")
                                   minor-mode-alist))))

(defun protect-buffer-from-kill-mode (&optional prefix buffer)
  "Protect buffer from being killed.
To remove this protection, call this command with a negative prefix argument."
  (interactive "P")
  (or buffer (setq buffer (current-buffer)))
  (save-excursion
    ;; Each cond does its own set-buffer *after* comparing prefix just in
    ;; case there's a buffer-local variable `prefix' to screw up the works.
    (cond
     ((null prefix)
      (set-buffer buffer)
      (setq protect-buffer-from-kill-mode
            (not protect-buffer-from-kill-mode)))
     ((>= prefix 0)
      (set-buffer buffer)
      (setq protect-buffer-from-kill-mode t))
     (t
      (set-buffer buffer)
      (setq protect-buffer-from-kill-mode nil)))
    ;; This is always done because kill-buffer-query-functions might have
    ;; been buffer-local when this package was initially loaded, leaving
    ;; the global value unchanged.
    (add-hook 'kill-buffer-query-functions 'protect-buffer-from-kill)))

;; This function is listed in kill-buffer-query-functions; it should return
;; nil if the buffer should not be killed, t otherwise.
(defun protect-buffer-from-kill ()
  (cond
   (protect-buffer-from-kill-mode
    (message "Buffer \"%s\" is protected from being killed." (buffer-name))
    nil)
   (t)))

(add-hook 'kill-buffer-query-functions 'protect-buffer-from-kill)

(protect-buffer-from-kill-mode nil (get-buffer "*scratch*"))




;;}}}
;;{{{ Functions: Compilation


(defun my--bcc-compile-source-file (fullname)
  "Compiles an elisp file into the byte-cache"
  (let (cachename
        hist-ent loaded-from-bcc-cache
        bcc-loaded-fake-cache-entry)

    (when (and bcc-enabled
               (not (save-match-data
                      (bcc-in-blacklist fullname bcc-blacklist))))

      (setq cachename (file-truename (bcc-cache-file-name fullname)))
      (make-directory (file-name-directory cachename) t)

      (when (and bcc-regenerate-toplevel
                 (file-newer-than-file-p fullname cachename))

        (bcc-regenerate-cache fullname cachename nil))

      (when (file-readable-p cachename)
        (unless bcc-loaded-fake-cache-entry
          (setq loaded-from-bcc-cache t))))
    ))


(defun my-compile ()
  "Compile elisp or cpp"
  (interactive)
  (delete-other-windows)
  (save-buffer)
  (if (or (eq major-mode 'lisp-mode) (eq major-mode 'emacs-lisp-mode))
      (progn
	(ignore-errors (my--kill-buffer-and-window (get-buffer-create "*Compile-Log*")))
	; Now try to compile this file
	(my--bcc-compile-source-file (buffer-file-name))
	)
    (progn
      (if (fboundp 'eproject-root)
	  (let ((default-directory (eproject-root)))
		  (compile compile-command))
	(compile compile-command)))))

(global-set-key [(f7)] 'my-compile)
;; ORIGINAL: undefined


(defun set-compile-command (&optional cmd)
  "Helper for to set compile-command"
  (interactive "scmd: ")
  (setq compile-command cmd))



;; Helper for compilation. Close the compilation window if there was
;; no error at all.
;; http://www.emacswiki.org/emacs/ModeCompile
(defun compile-autoclose (buffer string)
  (cond ((string-match "finished" string)
	 ;; (message "Build maybe successful: closing window.")
	 (run-with-timer 1 nil
			 'delete-window
			 (get-buffer-window buffer t)))
	(t
	 (message "Compilation exited abnormally: %s" string))))
(setq compilation-finish-functions 'compile-autoclose
      compilation-ask-about-save nil
      compilation-scroll-output t)

(global-set-key [(f8)] 'next-error)
;; ORIGINAL: undefined

(global-set-key [(shift f8)] 'previous-error)
;; ORIGINAL: undefined

;; Help should search more than just commands
(global-set-key (kbd "C-h a") 'apropos)
;; ORIGINAL: apropos-command


;;}}}
;;{{{ Load private data

(load (concat dotfiles-dir "private.el") 'noerror 'nomessage)



;;}}}
;;{{{ Mouse

;; paste at text-cursor, not at mouse-cursor
(setq mouse-yank-at-point t)

;; Show the text pointer in text areas
;;(setq void-text-area-pointer nil)

;; (eval-after-load "avoid"
;;   '(progn
;;      ;; Move the mouse to the lower-right corner instead of default upper-right
;;      ;; (defun mouse-avoidance-banish-destination ()
;;      ;;   (cons (- (frame-width) 1) (- (frame-height) 1)))
;;      (setq mouse-avoidance-timer-delay 0.1)
;;      (mouse-avoidance-mode 'banish)))
;; (unless ms-windows
;;   (when (display-mouse-p) (require 'avoid nil t)))



;;}}}
;;{{{ X-Windows cut'n'paste

;; Use clipboard of X
;; (setq x-select-enable-clipboard t
;;       interprogram-paste-function 'x-cut-buffer-or-selection-value)



;;}}}
;;{{{ Entering/exiting Emacs

;; get rid of yes-or-no questions - y or n is enough
(fset 'yes-or-no-p 'y-or-n-p)

;; Delete 'process-kill-buffer-query-function from kill-buffer-query-function.
;; http://www.masteringemacs.org/articles/2010/11/14/disabling-prompts-emacs/
(setq kill-buffer-query-functions
  (remq 'process-kill-buffer-query-function kill-buffer-query-functions))

(setq ;; Do without annoying startup msg.
      inhibit-startup-message t

      ;; This inhibits the initial startup echo area message.
      ;;(setq inhibit-startup-echo-area-message "schurig")

      ;; Don't ask when running revert-buffer
      revert-without-query (quote (""))

      ;; Empty scratch message
      initial-scratch-message nil

      ;; Include current buffer name in the title bar
      frame-title-format '(buffer-file-name "%f" ("%b"))

      ;; Don't ask for killing emacs
      ;;(setq confirm-kill-emacs t)

      ;; Set up default editing mode.
      default-major-mode 'indented-text-mode

      ;; Custom file, part one
      custom-file (concat dotfiles-dir "custom.el")
      )

;; Custom file, part two
(if (file-exists-p custom-file) (load-file custom-file))

;; Don't run vc-git & friends, we have magit
(defun vc-find-file-hook ()
  "Dummy, overriding the one in vc-hooks.el"
  (setq vc-mode nil))
(setq vc-handled-backends nil)



;;}}}
;;{{{ Entering text

;; use decimal for `C-q'
(setq read-quoted-char-radix 10)



;;}}}
;;{{{ File opening/saving

;; find file at point
(require 'ffap)

(global-set-key (kbd "C-x C-p") 'find-file-at-point)
;; ORIGINAL: mark-page

;; rebind C-x C-f and others to the ffap bindings (see variable ffap-bindings)
;; (ffap-bindings)
;; C-u C-x C-f finds the file at point
;; (setq ffap-require-prefix t)

;; Never show GTK file open dialog
(setq use-file-dialog nil)

;; don't add newlines to end of buffer when scrolling, but show them
(setq next-line-add-newlines nil)

;; Auto decompress compressed files.
(auto-compression-mode t)

(setq
 ;; Preserve hard links to the file you´re editing (this is
 ;; especially important if you edit system files)
 backup-by-copying-when-linked t
 ;; Just never create backup files at all
 ;;make-backup-files nil
 backup-directory-alist (list (cons "." (concat dotfiles-dir "tmp/bak/")))
 )

;; Emacs is a text editor, make sure your text files end in a newline
(setq require-final-newline t)

;; Disable auto-save (#init.el# file-names)
(setq auto-save-default nil)

;; Don't open Qt's *.pro files as IDLWAVE files
(add-to-list 'auto-mode-alist '("\\.pro$" . fundamental-mode))

;; Open *.h files normally in c++ mode
(add-to-list 'auto-mode-alist '("\\.h$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.inl\\'" . c++-mode))


;; Auto-saving into some global directory

;; (defvar autosave-dir
;;   (file-name-as-directory (concat dotfiles-dir "tmp/autosave"))
;;   "The directory in which to place auto-save (i.e. #foo#) files.")

;; (defun auto-save-file-name-p (filename)
;;   "Return non-nil if filename can be yielded by `make-auto-save-file-name'.
;; filename should lack slashes."
;;   (string-match "^#.*#$" (file-name-nondirectory filename)))

;; (defun make-auto-save-file-name ()
;;   (concat autosave-dir
;;           (if buffer-file-name
;;               (concat "#" (file-name-nondirectory buffer-file-name) "#")
;;             (expand-file-name
;;              (concat "#%" (buffer-name) "#")))))

(setq auto-save-list-file-prefix (concat dotfiles-dir "tmp/auto-save-list/saves-"))


;; quickly safe
(global-set-key [(f2)] 'save-buffer)
;; ORIGINAL: 2C-commands
(global-set-key [(shift f2)] '2C-command)
;; ORIGINAL: undefined


;; Registers allow you to jump to a file or other location quickly. Use
;; C-x r j followed by the letter of the register (i for init.el, s
;; for this file) to jump to it.
;;
;; You should add registers here for the files you edit most often.

(dolist (r `((?s (file . (concat dotfiles-dir "start.el")))
	     ;;(?o (file . ,(concat dotfiles-dir "emacs-kit.org")))
	     ))
  (set-register (car r) (cadr r)))



;;}}}
;;{{{ Help

;; Make 'l' go to the previous position in Emacs HELP
;; http://www.emacswiki.org/cgi-bin/wiki/EmacsNiftyTricks
(add-hook 'help-mode-hook
	  '(lambda ()
	     (define-key help-mode-map "l" 'help-go-back)))

;; check all variables and non-interactive functions as well
(setq apropos-do-all t)

(defun my-help ()
  "If function given tries to `describe-function' otherwise uses
`manual-entry' to display manpage of a `current-word'."
  (interactive)
  (let ((var (variable-at-point)))
    (if (symbolp var)
	(describe-variable var)
      (let ((fn (function-called-at-point)))
	(if fn
	    (describe-function fn)
	  (man (current-word)))))))

(global-set-key [(f1)] 'my-help)
;; ORIGINAL: help-command



;;}}}
;;{{{ Display: Frame display

;; ~/.Xresources
;; Emacs.geometry: 120x55
;; Emacs.Font:	terminus 11
;; Emacs.verticalScrollBars: right
;; Emacs.toolBar: off
;; Emacs*Background: #000000
;; Emacs*Foreground: #7f7f7f

;; Automatically merge ~/.Xresources after changes
(defun merge-x-resources ()
  (let ((file (file-name-nondirectory (buffer-file-name))))
    (when (or (string= file ".Xdefaults")
	      (string= file ".Xresources"))
      (start-process "xrdb" nil "xrdb" "-merge" (buffer-file-name))
      (message (format "Merged %s into X resource database" file)))))
(add-hook 'after-save-hook 'merge-x-resources)

;; Where to position a new frame (C-x 5 2)
;; Also where to open a new frame from emacsclient
;; (setq default-frame-alist '((xxx . 10)
;; 			    (left-fringe . 1)
;; 			    (right-fringe . 0)
;; 			    (menu-bar-lines . 1)
;; 			    (tool-bar-lines . 1)
;; 			    (left . 0))

(if ms-windows
    (setq initial-frame-alist
	  '(
	    ;;(background-color . "black")
	    ;;(foreground-color . "LightGray")
	    (horizontal-scroll-bars . nil)
	    (vertical-scroll-bars . right)
	    (tool-bar-lines . 0)
	    (left-fringe . 1)
	    (right-fringe . 0)))
  (setq initial-frame-alist
	`(;;(background-color . "black")
	  ;;(foreground-color . "LightGray")
	  (horizontal-scroll-bars . nil)
	  (vertical-scroll-bars . right)
	  (tool-bar-lines . 0)
	  (left-fringe . 1)
	  (right-fringe . 0)
	  ;;(height . ,(if (or (not my-win32)
	  ;;		   have-win32-sixbyten-font)
	  ;;	       (my-frame-percent-to-char-height 97)
	  ;;	     70))
	  (width . 120)
	  (height . 55)
	  )))

;; default-frame-alist is defined in terms of initial-frame-alist.  Don't
;; use copy-sequence here -- it doesn't copy the list elements, just the
;; list's cons cells.  Use copy-alist instead.
(setq default-frame-alist (copy-alist initial-frame-alist))



;;}}}
;;{{{ Display: Font lock and faces

;; http://www.emacswiki.org/cgi-bin/wiki/EightyColumnRule
(defface my--todo-face
  '((t :foreground "red"
       :weight bold))
  "Font for showing TODO words."
  :group 'basic-faces)

;; Highlight each of TODO TODO: FIXME FIXME: XXX XXX: \todo
(defun my--hint-facify ()
   (unless (or (eq 'diff-mode major-mode) (eq 'script-mode major-mode))
     (font-lock-add-keywords nil '(
	 ("\\(\\<\\(\\(FIXME\\|TODO\\|XXX\\):?\\>\\)\\|\\\\todo\\)" 1 'my--todo-face t)
	 ))))

(add-hook 'font-lock-mode-hook 'my--hint-facify)

;; tips from http://www.reddit.com/r/emacs/comments/9nh64/ask_emacs_which_color_theme_do_you_use/
(custom-set-faces

 '(font-lock-constant-face
   ((((class color) (min-colors 88) (background light))
     (:foreground "SlateBlue4"))))

 '(font-lock-string-face
   ((((class color) (min-colors 88) (background light))
     (:foreground "Forest Green"))))

 '(font-lock-keyword-face
   ((t (:weight bold))))
 )

;; Column-Marker http://www.emacswiki.org/emacs/ColumnMarker
;;
;; HINT: column-marker.el comes from EmacsWiki, you can update it
;; with (auto-install-from-emacswiki)
(eval-after-load "column-marker"
  '(add-hook 'c-mode-hook (lambda () (interactive) (column-marker-1 80))))
(require 'column-marker nil 'nomsg)



;; Activate font-lock-mode (syntax coloring)
(setq global-font-lock-mode t
      font-lock-verbose nil)

(defconst font-lock-maximum-decoration t)



;;}}}
;;{{{ Display: Truncation lines

(setq default-truncate-lines t)
(setq truncate-partial-width-windows nil)
(defun my-wrap-mode-on ()
  "Minor mode for making buffer not wrap long lines to next line."
  (interactive)
  (setq truncate-lines nil))

(defun my-wrap-mode-off ()
  "Minor mode for making buffer wrap long lines to next line."
  (interactive)
  (setq truncate-lines t))

(defun my-toggle-wrap-mode ()
  "Switch wrap mode from wrap to non-wrap, or vice-versa."
  (interactive)
  (if (eq truncate-lines nil)
      (my-wrap-mode-off)
    (my-wrap-mode-on)))



;;}}}
;;{{{ Display: Whitespace

;; Let typing errors be obvious
(add-hook 'find-file-hook
	  '(lambda ()
	     ;; This hack allows the diff-mode hook to set the
	     ;; variable to -1. The find-file-hook fires after the
	     ;; diff-mode hook, so we get the -1 and are able to turn
	     ;; off the display of trailing whitespaces.
	     (if (eq show-trailing-whitespace -1)
		 (setq show-trailing-whitespace nil)
	       (setq show-trailing-whitespace t))))


;;}}}
;;{{{ Display: General

;; Avoid Emacs hanging for a while changing default font
(modify-frame-parameters nil '((wait-for-wm . nil)))

;; Display various non-editing buffers in their own frames
(add-to-list 'special-display-buffer-names "*Backtrace*")

;; Display those special buffer frames without a tool bar
;; now in .Xresources
;; (add-to-list 'special-display-frame-alist '(tool-bar-lines . 0))

(if window-system
    (progn
      ;; Windows systems are fast enought
      (column-number-mode t)
      ;; Turn off blinking
      (blink-cursor-mode -1)
      ))

;; Visible bell, beeps are annoying
(setq visible-bell t)

;; Let parenthesis behave
(show-paren-mode 1)
(setq show-paren-delay 0
      blink-matching-parent nil)
(set-face-background 'show-paren-match-face "#d0d0d0")


;; Display page delimiter ^L as a horizontal line
(or standard-display-table (setq standard-display-table (make-display-table)))
(aset standard-display-table ?\f (vconcat (make-vector 72 ?-) "^L"))

;; Let emacs react way faster
(setq echo-keystrokes 0.1
      idle-update-delay 0.35)



;;}}}
;;{{{ Display: Minibuffer

;; C-c clears minibuffer
(define-key minibuffer-local-map "\C-c" (lambda () (interactive) (delete-minibuffer-contents)))

(setq
 ;; Don't insert current directory into minubuffer
 insert-default-directory nil
 ;; enable recursive minibuffer
 enable-recursive-minibuffers nil
 ;; minibuffer window expands vertically as necessary to hold the text that
 ;; you put in the minibuffer
 resize-mini-windows t
 )

;; dim the ignored part of the file name
(file-name-shadow-mode 1)

;; allow to type space chars in minibuffer input
;; (for `timeclock-in', for example)
(define-key minibuffer-local-completion-map " " nil)
(define-key minibuffer-local-must-match-map " " nil)


;; Minibuffer history
(require 'savehist)
(setq savehist-file (concat dotfiles-dir "tmp/history.el")
      history-length 1000)
(savehist-mode 1)



;;}}}
;;{{{ Display: Menue

;; get rid of the Games in the Tools menu
(define-key menu-bar-tools-menu [games] nil)

;; Default was 80000
(setq undo-limit 30000)


;;}}}
;;{{{ Printing

(setq lpr-command "kprinter")


;;}}}
;;{{{ Misc

;; Delete previous identical history entries
(setq history-delete-duplicates t)



;;}}}
;;{{{ Language, German localisation

;; A sentence doesn't end with two spaces (in german)
(setq sentence-end-double-space nil)

;; Allow german umlaut characters
(unless ms-windows
  (set-language-environment "Latin-1"))

(set-input-mode (car (current-input-mode))
 		(nth 1 (current-input-mode))
 		0
 		;; Default for above is 't, which let us not enter
		;; umlaut characters on a german keyboard
		(nth 3 (current-input-mode))
 		)

(unless window-system
  ;; Without this, "emacs -nw" only shows ??? and not äöü
  (setq-default default-enable-multibyte-characters nil))



;;}}}
;;{{{ Completion

(setq ;; ignore case when reading a file name completion
      read-file-name-completion-ignore-case t
      ;; do not consider case significant in completion (GNU Emacs default)
      completion-ignore-case t
      ;; lets TAB do completion as well
      tab-always-indent 'complete
      completions-format 'vertical)

;; Doesn't work with ido
;; (partial-completion-mode 1)
;; (setq completion-auto-help 'lazy)




;;}}}
;;{{{ Mode: C, C++

;; Tabs mode
(setq indent-tabs-mode t)

;; http://www.emacswiki.org/emacs-en/SmartTabs
;;(setq cua-auto-tabify-rectangles nil)
(defadvice align (around smart-tabs activate)
  (let ((indent-tabs-mode nil)) ad-do-it))
(defadvice align-regexp (around smart-tabs activate)
  (let ((indent-tabs-mode nil)) ad-do-it))
(defadvice indent-relative (around smart-tabs activate)
  (let ((indent-tabs-mode nil)) ad-do-it))
(defadvice indent-according-to-mode (around smart-tabs activate)
  (let ((indent-tabs-mode indent-tabs-mode))
    (if (memq indent-line-function
	      '(indent-relative
		indent-relative-maybe))
	(setq indent-tabs-mode nil))
    ad-do-it))
(defmacro smart-tabs-advice (function offset)
  (defvaralias offset 'tab-width)
  `(defadvice ,function (around smart-tabs activate)
     (cond
      (indent-tabs-mode
       (save-excursion
	 (beginning-of-line)
	 (while (looking-at "\t*\\( +\\)\t+")
	   (replace-match "" nil nil nil 1)))
       (setq tab-width tab-width)
       (let ((tab-width fill-column)
	     (,offset fill-column))
	 ad-do-it))
      (t
       ad-do-it))))


;; Silence byte-compiler
(require 'cc-mode)


(defun my-c-mode-common-setup ()
  (define-key c-mode-map (kbd "RET") 'newline)
  (turn-off-auto-fill)
  (c-toggle-auto-newline 1)
  (modify-syntax-entry ?_ "w")
  ;; c-mode overrides the global newline-and-indent. Strangely,
  ;; cc-mode keeps the global. We don't care, we always set it :-)
  (local-set-key (kbd "RET") 'newline-and-indent)
  (setq fill-column 76
	;; Let RET break and continue a comment
	;; C doesn't start functions with a ( in the first column
	open-paren-in-column-0-is-defun-start nil
	;; Insert TABs inside literals
	c-tab-always-indent 1
	;; Tell cc-mode not to check for old-style (K&R) function
	;; declarations. This speeds up indenting a lot (I hear).
	c-recognize-knr-p nil
	;; Jump to errors, please
	compilation-auto-jump-to-first-error t
	;; Turn of elect, TODO: consider hungry-delete
	c-electric-flag nil
	;; But if it's on, let a "#" go to the left, for #if/#else/#endif
	c-electric-pound-behavior '(alignleft)
	;; No abbrevs
	abbrev-mode nil
	;; Preferred tab width
	tab-width 4
	c-basic-offset 4
	;; Default style
	c-default-style '((java-mode . "java")
			  (awk-mode . "awk")
			  (other . "linux"))
	)
  (smart-tabs-advice c-indent-line c-basic-offset)
  (smart-tabs-advice c-indent-region c-basic-offset)
  )
(add-hook 'c-mode-common-hook 'my-c-mode-common-setup)

;; Search .obj dir as well:
(require 'find-file)
(add-to-list 'cc-search-directories ".obj")
(add-to-list 'cc-search-directories "..")



;;}}}
;;{{{ Mode: CEDET

;; http://cedet.sourceforge.net/
;; http://xtalk.msk.su/~ott/en/writings/emacs-devenv/EmacsCedet.html

(defvar srecode-map-save-file nil)
(defvar semanticdb-default-save-directory nil)
(setq srecode-map-save-file (concat dotfiles-dir "tmp/srecode-map")
      semanticdb-default-save-directory (concat dotfiles-dir "/tmp/semanticdb"))

(eval-after-load "cedet"
  '(progn
     ;; Enable one of those:
     ;;(semantic-load-enable-minimum-features)
     ;;(semantic-load-enable-code-helpers)
     (semantic-load-enable-gaudy-code-helpers)
     ;;(semantic-load-enable-excessive-code-helpers)

     ;; Enable exuberant ctags
     ;;(semantic-load-enable-all-exuberent-ctags-support)

     ;; Enable the Project management system
     ;; (global-ede-mode 1)

     ;; Enable template insertion menu
     ;; (global-srecode-minor-mode 1)

     ;; Use GCC include paths
     (semantic-gcc-setup)

     ;; Increase the delay before activation
     ;;(setq semantic-idle-scheduler-idle-time 10)

     ;; Idle after 1 second
     ;;(setq semantic-idle-scheduler-idle-time 1)
     ))

(defun load-cedet ()
  (interactive)
  (load "cedet" 'noerror 'nomessage))



;;}}}
;;{{{ Mode: CSV

(add-to-list 'auto-mode-alist '("\\.[Cc][Ss][Vv]\\'" . csv-mode))
(autoload 'csv-mode "csv-mode" "Major mode for editing comma-separated value files." t)



;;}}}
;;{{{ Mode: Diff

(add-hook 'diff-mode-hook
	  '(lambda ()
	     (setq show-trailing-whitespace -1)
	     (setq def-file-header '((t nil)))
	     (setq def-header '((t nil)))
	     ))
(setq diff-switches "-u")



;;}}}
;;{{{ Mode: Dired

;; HINT: next expression is very useful for M-( in Dired mode:
;; (string-match "perl" (shell-command-to-string (concat "file " name)))

;; use 'e' to edit filenames
(eval-after-load "dired"
  '(progn
     ;; provide some dired goodies
     ;; http://www.gnu.org/software/emacs/manual/html_mono/dired-x.html
     ;; dired-jump        C-x C-j
     (require 'dired-x)
     (message "XXXXXXXXXXXXXXX dired")

     (setq dired-auto-revert-buffer t)
     (define-key dired-mode-map "e" 'wdired-change-to-wdired-mode)))

(define-key global-map "\C-x\C-d" 'dired)
;; ORIGINAL: list-directory


;;}}}
;;{{{ Mode: ELisp

(add-hook 'emacs-lisp-mode-hook
	  '(lambda ()
	     ;; automatically give help about function syntax
	     (eldoc-mode t)
	     ;; "-" is almost always part of a function- or variable-name
	     (modify-syntax-entry ?- "w")))



;;}}}
;;{{{ Mode: Fold

;; SEE http://www.emacswiki.org/emacs/download/fold.el

(autoload 'fold-mode "fold" "Fold mode" t)
(autoload 'fold-toggle "fold" "Fold toggle" t)



(eval-after-load "fold"
  '(progn
     (define-key fold-mode-map "\C-t" 'fold-toggle)
     (setq fold-automatic-show nil)
     (setq fold-autoclose-other-folds nil)
     (setq fold-whole-buffer-moves-point t)))



;;}}}
;;{{{ Mode: Generic-X

(require 'generic-x)



;;}}}
;;{{{ Mode: Info

;; (add-to-list 'Info-default-directory-list (concat dotfiles-dir "info"))

(defun my-next-link-or-scroll-page-forward (next-point)
  "Scroll one page forward when no more next links on the current page."

  (if (and (> (window-end) next-point) (> next-point (point)))
      (goto-char next-point)
    (if (>= (window-end) (point-max))
        (goto-char (point-max))
      (progn (View-scroll-page-forward-set-page-size) (move-to-window-line 0)))))

(defun my-prev-link-or-scroll-page-backward (prev-point)
  "Scroll one page backward when no more previous links on the current page."

  (if (and (< (window-start) prev-point) (< prev-point (point)))
      (goto-char prev-point)
    (if (<= (window-start) (point-min))
        (goto-char (point-min))
      (progn (View-scroll-page-backward-set-page-size)))))

(eval-after-load "info"
  '(progn
     ;; TRY:
     (add-hook 'Info-selection-hook (lambda () (recenter 45)))
     ;; Keys
     (define-key Info-mode-map "\M-s" 'Info-search-next) ; obsoleted by C-M-s
     ;; Mozilla-like navigation:
     (define-key Info-mode-map [(meta right)] 'Info-follow-nearest-node)
     (define-key Info-mode-map [(meta left)]  'Info-last)
     (define-key Info-mode-map [(tab)]  'Info-next-reference)
     (define-key Info-mode-map [(shift tab)] 'Info-prev-reference)
     (define-key Info-mode-map [(shift iso-lefttab)] 'Info-prev-reference)
					;(define-key Info-mode-map [(shift f7)] (lambda () (interactive) (Info-search (car Info-search-history))))
     ;; Lynx-like navigation:
     (define-key Info-mode-map [(meta up)]
       (lambda ()
         (interactive)
         (my-prev-link-or-scroll-page-backward
          (save-excursion
            (ignore-errors
	     (Info-prev-reference))
            (point)))))
     (define-key Info-mode-map [(meta down)]
       (lambda ()
         (interactive)
         (my-next-link-or-scroll-page-forward
          (save-excursion
            (ignore-errors
	     (Info-next-reference))
            (point)))))
     ;; more/less scrolling style
     ;;(define-key Info-mode-map [return] 'View-scroll-line-forward)
     ))

;; Open Info-Screen in extra frame
(defadvice info (before info activate)
  (select-frame (make-frame))
  )


;;}}}
;;{{{ Mode: Lisp

(defun my-reindent-then-newline-and-indent-and-indent-sexp ()
  "Reindent current line, insert newline, then indent the new line.
Move backward out of one level of parentheses.
Indent each line of the list starting just after point."
  (interactive "*")
  (reindent-then-newline-and-indent)
  (save-excursion
    (backward-up-list)
    (indent-sexp)))

(defun my-join-line-and-indent-sexp ()
  "Join this line to previous and fix up whitespace at join.
Move backward out of one level of parentheses.
Indent each line of the list starting just after point."
  (interactive "*")
  (join-line)
  (save-excursion
    (backward-up-list)
    (indent-sexp)))

(defun my-join-line-and-indent-sexp-or-backward-kill-word ()
  "If point is on the whitespaces at the beginning of a line,
then join this line to previous and indent each line of the upper list.
Otherwise, kill characters backward until encountering the end of a word."

  (interactive "*")
  (if (save-excursion (and (skip-chars-backward " \t") (bolp)))
      (my-join-line-and-indent-sexp)
    (backward-kill-word 1)))

(define-key lisp-mode-map [(control return)] 'my-reindent-then-newline-and-indent-and-indent-sexp)
(define-key lisp-mode-map [(control backspace)] 'my-join-line-and-indent-sexp-or-backward-kill-word)
;;(tempo-define-template "lisp-print-map" '("(map (lambda (x) ) " p ")"))
;;(define-key lisp-mode-map "\C-zim" 'tempo-template-lisp-print-map)
(define-key emacs-lisp-mode-map [(control return)] 'my-reindent-then-newline-and-indent-and-indent-sexp)
(define-key emacs-lisp-mode-map [(control backspace)] 'my-join-line-and-indent-sexp-or-backward-kill-word)
;(define-key emacs-lisp-mode-map [(control meta tab)] 'lisp-complete-symbol)
(define-key emacs-lisp-mode-map "\C-ze\t" 'lisp-complete-symbol)
(define-key emacs-lisp-mode-map "\C-xF"  'find-function)
(define-key emacs-lisp-mode-map "\C-x4F" 'find-function-other-window)
(define-key emacs-lisp-mode-map "\C-x5F" 'find-function-other-frame)
(define-key emacs-lisp-mode-map "\C-xK"  'find-function-on-key)
(define-key emacs-lisp-mode-map "\C-xV"  'find-variable)
(define-key emacs-lisp-mode-map "\C-x4V" 'find-variable-other-window)
(define-key emacs-lisp-mode-map "\C-x5V" 'find-variable-other-frame)
;;(tempo-define-template "emacs-lisp-print-message" '("(message \"%s\" " p ")"))
;;(define-key emacs-lisp-mode-map "\C-zim" 'tempo-template-emacs-lisp-print-message)
;;(tempo-define-template "emacs-lisp-print-defun" '("(defun " p " ()\n  (interactive)\n\n)\n"))
;;(define-key emacs-lisp-mode-map "\C-zid" 'tempo-template-emacs-lisp-print-defun)
;;(tempo-define-template "lisp-print-map" '("(map (lambda (x) ) " p ")"))
;;(define-key lisp-interaction-mode-map "\C-zim" 'tempo-template-emacs-lisp-print-message)
(define-key lisp-interaction-mode-map [(control return)] 'my-reindent-then-newline-and-indent-and-indent-sexp)
(define-key lisp-interaction-mode-map [(control backspace)] 'my-join-line-and-indent-sexp-or-backward-kill-word)
;(define-key lisp-interaction-mode-map [(control meta tab)] 'lisp-complete-symbol)

;; gimmick: replace lambda with the greek Lambda symbol
;; (font-lock-add-keywords
;;  nil `(("\\<lambda\\>"
;;         (0 (progn (compose-region (match-beginning 0) (match-end 0)
;;                                   ,(make-char 'greek-iso8859-7 107))
;;                   nil)))))

(eval-after-load "scheme"
  '(progn
     (define-key scheme-mode-map [(control return)] 'my-reindent-then-newline-and-indent-and-indent-sexp)
     (define-key scheme-mode-map [(control backspace)] 'my-join-line-and-indent-sexp-or-backward-kill-word)))

;;}}}
;;{{{ Mode: Man

(eval-after-load "man"
  '(progn
     ;; Mozilla-like navigation:

     (define-key Man-mode-map [(meta right)] 'man-follow)
     (define-key Man-mode-map [(meta left)] 'quit-window)
     ;; Lynx-like navigation:
     (define-key Man-mode-map [(meta up)]
       (lambda ()
	 (interactive)
	 (my-prev-link-or-scroll-page-backward
	  (save-excursion
	    (ignore-errors (Man-previous-section 1))
	    (point)))))
     (define-key Man-mode-map [(meta down)]
       (lambda ()
	 (interactive)
	 (my-next-link-or-scroll-page-forward
	  (save-excursion

	    (ignore-errors (Man-next-section 1))
	    (point)))))
     (define-key Man-mode-map [f2] 'toggle-truncate-lines)
     ;; (define-key view-mode-map [tab] 'other-window) ; used for next-ref
     ;; more/less scrolling style
     (define-key Man-mode-map [return] 'View-scroll-line-forward)))



;;}}}
;;{{{ Mode: Org

;; http://orgmode.org/worg/org-tutorials/orgtutorial_dto.php
;; http://thread.gmane.org/gmane.emacs.orgmode/4832
;; http://www.newartisans.com/2007/08/using-org-mode-as-a-day-planner.html

(eval-after-load "org"
  '(progn (setq org-directory (file-truename (concat dotfiles-dir "org/"))
		org-default-notes-file (concat org-directory "notes.org")
		org-agenda-files (list (concat org-directory "agenda.org") org-default-notes-file)

		;; Only load these org modules:
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

		;; Add a space before the elipsis
		org-ellipsis " ..."

		;; Store notes at beginning of file
		org-reverse-note-order t

		;; Follow a link with just return
		org-return-follows-link t

		;; No need to add a DONE log entry, as our #+TODO: line makes a
		;; log entry anyway. For the same reason, don't add a closed-
		;; string either.
		org-log-done 'nil
		;; org-closed-string ""

		;; don't use S-xxx as this is used for CUA mode etc
		org-replace-disputed-keys t
		org-support-shift-select 'always

		;; Assumes you have "#+STARTUP: customtime" in your *.org file
		;; or you can alternative set "org-display-custom-times t"
		org-time-stamp-custom-formats '("<%d.%m.%Y %a>" . "<%d.%m.%Y %a %H:%M>")
		;; org-display-custom-times t

		;; This seems like a good basic set of keywords to start out with:
		;; org-todo-keywords '((type "TODO" "NEXT" "WAITING" "DONE"))
		;; org-todo-keywords '((sequence "TODO" | "DONE")
		;;                     (sequence "REPORT" "BUG" "KNOWNCAUSE" | "FIXED")
		;;                     (sequence | "CANCELLED")))

		)
	  (add-hook 'org-mode-hook 'auto-fill-mode)
	  (define-key org-mode-map "\C-t" 'org-shifttab)
     ))


(eval-after-load "org-agenda"
  '(progn (setq	;; Include diary entries
		org-agenda-include-diary t

		;; Opening/closing .org mode
		org-agenda-restore-windows-after-quit t
		org-agenda-window-setup 'current-window

		;; Skip done items
		org-agenda-skip-deadline-if-done t
		org-agenda-skip-scheduled-if-done t

		;; Let agenda starts on the current day
		org-agenda-start-on-weekday nil

		;; For C-c a #
		org-stuck-projects
		'("+LEVEL=2-CATEGORY=\"Notes\""
		  ;; TODO-keyword identifying non-stuck projects:
		  ("TODO" "DONE" "CANCELLED")
		  ;; Tags identifying non-stuck projects:
		  nil
		  ;; Arbitrary reg-exp identifying non-stuck projects:
		  "")

		;; Some special view to select from after C-c a
		;; (key desc type match settings files)
		org-agenda-custom-commands
		'(("f" "Finished" todo "DONE|CANCELLED" nil)
		  ("w" "Waiting" todo "WAIT|FORWARD" nil)
		  ("3" "next 3 weeks" agenda "" ((org-agenda-ndays 21)))
		  ("u" "unscheduled" alltodo ""
		   ((org-agenda-skip-function
		     (lambda ()
		       (org-agenda-skip-entry-if 'scheduled
						 'deadline
						 'regexp "<[^>\n]+>"
						 ))))))

		)
     ))

(eval-after-load "org-list"
  '(progn (setq ;; Only use "1.", "2." for ordered lists, not "1)", "2)" etc
		org-plain-list-ordered-item-terminator ?.)
	  ))

(autoload 'org-mode "org" "Org mode" t)
(autoload 'org-diary "org" "Diary entries from Org mode")
(autoload 'org-agenda "org-agenda" "Multi-file agenda from Org mode" t)
(autoload 'org-store-link "org" "Store a link to the current location" t)
;; (autoload 'orgtbl-mode "org" "Org tables as a minor mode" t)
;; (autoload 'turn-on-orgtbl "org" "Org tables as a minor mode")

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

;; Key-Bindings
(define-key global-map "\C-ca" 'org-agenda)
;; ORIGINAL: undefined
(define-key global-map "\C-cl" 'org-store-link)
;; ORIGINAL: undefined



;;}}}
;;{{{ Mode: Remember

(eval-after-load "org-remember"
  '(progn (setq org-remember-templates
		;; List elements:
		;;   single name
		;;   character
		;;   template
		;;     %?  After completing the template, position cursor here.
		;;     %t  time stamp, date only (%u but inactive date)
		;;   optional file
		;;   optional headline: under which headline to file the new entry
		;;   optional context
		'(("TODO"
		   ?t
		   "* TODO %?\n  - State \"TODO\"       %u"
		   "agenda.org"
		   "Tasks")
		  )

		)
	  (org-remember-insinuate)
	  ))

(eval-after-load "remember"
  '(progn (setq remember-data-file (concat org-directory "agenda.org")
	   )
	  ))

(autoload 'org-remember "org-remember" "Remember something" t)

(define-key global-map "\C-cr" 'org-remember)
;; ORIGINAL: undefined



;;}}}
;;{{{ Mode: Perl

;; Use cperl mode instead of perl mode
(fset 'perl-mode 'cperl-mode)

;; (add-to-list
;;TODO  'auto-insert-alist
;;  '(cperl-mode
;;    nil
;;    "#!/usr/bin/perl -w" \n
;;    "# -*- Perl -*-" \n
;;    ;; "# \$Id\$" \n
;;
;;    ;; "# \$RCSfile\$\$Revision\$\$Date\$" \n
;;    "# \$Revision\$" \n
;;    \n
;;    "while (<>) {" \n
;;    > "chomp;" \n
;;    > _ \n
;;    > "print \"$_\\n\";\n"
;;    "}\n"))

;; (eval-after-load "cperl-mode"
;;   '(progn
;;      ;; (define-auto-insert 'cperl-mode (lambda () (tempo-template-perl-skeleton)))
;;      (define-key cperl-mode-map "\C-ziw" 'tempo-template-perl-while-skeleton)
;;      (define-key cperl-mode-map "\C-zip" 'tempo-template-perl-print-skeleton)
;;      (define-key cperl-mode-map "\C-zis" 'tempo-template-perl-s-skeleton))
;;
;; (tempo-define-template "perl-skeleton" '("#!/usr/bin/perl -w\n# -*- Perl -*-\n# \$Revision\$\n\nwhile (<>) {\n  chomp;\n  " p "\n}\n"))
;; (tempo-define-template "perl-s-skeleton" '("s/" p "//;"))
;; (tempo-define-template "perl-print-skeleton" '("print \"$_" p "\\n\";"))
;; (tempo-define-template "perl-while-skeleton" '("while (<>) {\n  chomp;\n  " p "\n}\n"))



;;}}}
;;{{{ Mode: Python

(defun my-tab-setup ()
  (interactive)
  (setq indent-tabs-mode t)
  (setq tab-width 4))
(smart-tabs-advice python-indent-line-1 python-indent)
(add-hook 'python-mode-hook 'my-tab-setup)

;;}}}
;;{{{ Mode: Shell

(smart-tabs-advice shell-basic-indent-line sh-basic-offset)
(add-hook 'shell-mode-hook 'my-tab-setup)

;;}}}
;;{{{ Mode: Term

(eval-after-load "term"
  '(progn
     (add-hook 'term-mode-hook
	       (lambda ()
		 (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")
		 ;; (make-local-variable 'transient-mark-mode)
		 (auto-fill-mode -1)))))



;;}}}
;;{{{ Mode: Text

;; always do auto-fill in text mode
(add-hook 'text-mode-hook
	  '(lambda ()
	     (auto-fill-mode 1)))



;;}}}
;;{{{ Mode: WordStar

(autoload 'wsmm-mode "wsmm" "WordStar minor mode" t)



;;}}}
;;{{{ Package: aspell

; http://www.emacswiki.org/emacs-jp/InteractiveSpell
(setq ispell-program-name "aspell"
      ispell-list-command "list"
      ;; This makes aspell faster, but it will make it's suggestion worse
      ispell-extra-args '("--sug-mode=ultra")
      flyspell-issue-message-flag nil)

(defun flyspell-de ()
  "Calls Flyspell with german dictionary"
  (interactive)
  (ispell-change-dictionary "de-neu")
  (flyspell-mode 1)
  (flyspell-buffer))

(defun flyspell-en ()
  "Calls Flyspell with english dictionary"
  (interactive)
  (ispell-change-dictionary "en")
  (flyspell-mode 1)
  (flyspell-buffer))


;;}}}
;;{{{ Package: auto-install


(eval-after-load "install-elisp"
  '(setq install-elisp-repository-directory (concat dotfiles-dir "elisp/")))

(eval-after-load "url-cache"
  '(setq url-cache-directory (concat dotfiles-dir "tmp/cache/")))

(eval-after-load "auto-install"
  '(progn (setq auto-install-directory (concat dotfiles-dir "elisp/"))
	  ))

;; HINT: auto-install.el comes from EmacsWiki, you can update it
;; with (auto-install-from-emacswiki)

;; (require 'auto-install nil 'nomsg)
;; (auto-install-update-emacswiki-package-name t)
(autoload 'auto-install-from-emacswiki "auto-install" nil t)
(autoload 'auto-install-from-url "auto-install" nil t)



;;}}}
;;{{{ Package: bookmark

(eval-after-load "bookmark"
  '(progn
     (setq ;; Store bookmarks inside .emacs.d
           bookmark-default-file (concat dotfiles-dir "org/bookmarks.org")
	   ;; Save file after every bookmark altertation
	   bookmark-save-flag 1
	   )))



;;}}}
;;{{{ Package: browse-url

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome")



;;}}}
;;{{{ Package: calendar

(setq diary-file  (concat dotfiles-dir "org/diary")
      calendar-latitude 50.17
      calendar-longitude 8.46
      calendar-location-name "Wöllstadt"
      calendar-christian-all-holidays-flag nil
      calendar-date-style 'european
      holiday-bahai-holidays nil
      holiday-hebrew-holidays nil
      holiday-islamic-holidays nil
      holiday-oriental-holidays nil
      holiday-solar-holidays nil
      calendar-holidays '((holiday-fixed 1 1 "Neujahr")
                          (holiday-easter-etc 0 "Ostern")
                          (holiday-easter-etc 1 "Ostermontag")
                          (holiday-easter-etc -2 "Karfreitag")
                          (holiday-easter-etc -46 "Aschermittwoch")
                          (holiday-fixed 12 24 "Heiligabend")
                          (holiday-fixed 12 25 "1. Weihnachtstag")
                          (holiday-fixed 12 26 "2. Weihnachtstag"))
					;(if (fboundp (quote atan)) (-equinoxes-solstices)))
      calendar-week-start-day 1
      calendar-mark-holidays-flag 1
      calendar-view-holidays-initially-flag t
      holidays-general-holidays '(holiday-fixed 1 1 "Neujahr")
      )

;; Fix foolish calendar-mode scrolling.
;; http://steve.yegge.googlepages.com/my-dot-emacs-file
(add-hook 'calendar-load-hook
	  '(lambda ()
	     (define-key calendar-mode-map ">" 'scroll-calendar-left)
	     (define-key calendar-mode-map "<" 'scroll-calendar-right)
	     (define-key calendar-mode-map "\C-x>" 'scroll-calendar-left)
	     (define-key calendar-mode-map "\C-x<" 'scroll-calendar-right)))

(add-hook 'calendar-initial-window-hook 'diary-mark-entries)



;;}}}
;;{{{ Package: cscope

;; (eval-after-load "xcscope"
(eval-after-load "xcscope"
  '(progn (setq ;; This indexer ignores .obj, .git, .svn and single-letter directories
	   cscope-indexing-script (concat dotfiles-dir "bin/cscope-indexer")
	   ;; It seems that it asks anyway ...
	   cscope-no-mouse-prompts t)
	  ))

(autoload 'cscope-find-this-symbol "xcsope" nil t)
(autoload 'cscope-pop-mark "xcsope" nil t)
(autoload 'cscope-next-symbol "xcsope" nil t)
(autoload 'cscope-prev-symbol "xcsope" nil t)

(define-key esc-map "." 'cscope-find-this-symbol)
;; ORIGINAL: find-tag (etags.el)

(define-key esc-map "*" 'cscope-pop-mark)
;; ORIGINAL: pop-tag-mark (etags.el)

(define-key esc-map "," 'cscope-next-symbol)
;; ORIGINAL: tags-loop-continue (etags.el)

(define-key esc-map ";" 'cscope-prev-symbol)
;; ORIGINAL: comment-dwim



;;}}}
;;{{{ Disabled Package: desktop

;; http://www.emacswiki.org/emacs/DeskTop

;; (setq desktop-base-file-name (concat dotfiles-dir "tmp/desktop.data")
;;       desktop-base-lock-name (concat dotfiles-dir "tmp/desktop.lock")
;;       desktop-save t
;;       desktop-load-locked-desktop t
;;       desktop-buffers-not-to-save
;;       (concat "\\("
;; 	      "^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^TAGS"
;; 	      "\\|\\.emacs.*\\|\\.diary\\|\\.newsrc-dribble\\|\\.bbdb"
;; 	      "\\)$"))
;; (desktop-save-mode 1)

;; (add-to-list 'desktop-modes-not-to-save 'dired-mode)
;; (add-to-list 'desktop-modes-not-to-save 'org-mode)
;; (add-to-list 'desktop-modes-not-to-save 'Info-mode)
;; (add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
;; (add-to-list 'desktop-modes-not-to-save 'fundamental-mode)
;; ;; No need to save them, as the history will be handled by
;; ;; recentf and recentf-initialize-file-name-history, but
;; ;; only if file-name-history is empty ...
;; (delq 'file-name-history desktop-globals-to-save)
;; (add-to-list 'desktop-globals-to-save 'compile-command)


;;}}}
;;{{{ Package: eshell

(setq eshell-cmpl-cycle-completions nil
      eshell-save-history-on-exit t
      eshell-cmpl-dir-ignore "\\`\\(\\.\\.?\\|CVS\\|\\.svn\\|\\.git\\)/\\'")

(eval-after-load 'esh-opt
  '(progn
     (require 'em-prompt)
     (require 'em-term)
     (require 'em-cmpl)
     (setenv "PAGER" "cat")
     (set-face-attribute 'eshell-prompt nil :foreground "turquoise1")
     (add-hook 'eshell-mode-hook ;; for some reason this needs to be a hook
	       '(lambda () (define-key eshell-mode-map "\C-a" 'eshell-bol)))
     (add-to-list 'eshell-visual-commands "ssh")
     (add-to-list 'eshell-visual-commands "tail")
     (add-to-list 'eshell-command-completions-alist
     		  '("gunzip" "gz\\'"))
     (add-to-list 'eshell-command-completions-alist
     		  '("tar" "\\(\\.tar|\\.tgz\\|\\.tar\\.gz\\)\\'"))
     (add-to-list 'eshell-output-filter-functions 'eshell-handle-ansi-color)))


;; The eshell directory holds alias definitions and history
;; information.  It is much like a .bashrc file for those who are
;; familiar with bash.  This set the value of eshell-directory-name to
;; point to the eshell directory in this directory.  The alias file
;; is pre-populated with some generally applicable aliases.

;; (setq eshell-directory-name (expand-file-name "./" (expand-file-name "eshell" dotfiles-dir)))

(global-set-key (kbd "C-x m") 'eshell)
;; ORIGINAL: undefined
(global-set-key (kbd "C-x M") (lambda () (interactive) (eshell t)))
;; ORIGINAL: compose-mail


;;}}}
;;{{{ Package: eproject

(require 'eproject nil t)
(setq eproject-completing-read-function (quote eproject--ido-completing-read))

;;(require 'eprojects)
(load (concat dotfiles-dir "eprojects.el") 'noerror 'nomessage)


;; Snippets from eproject-extra.el:

(defun eproject-grep (regexp)
  "Search all files in the current project for REGEXP."
  (interactive "sRegexp grep: ")
  (let* ((root (eproject-root))
         (default-directory root)
         (files (eproject-list-project-files-relative root)))
    (grep-compute-defaults)
    (lgrep regexp (combine-and-quote-strings files) root)))

(defvar eproject-todo-expressions
  '("TODO" "XXX" "FIXME")
  "A list of tags for `eproject-todo' to search for when generating the project's TODO list.")

(defun eproject-todo ()
  "Display a project TODO list.

Customize `eproject-todo-expressions' to control what this function looks for."
  (interactive)
  ;; TODO: display output in a buffer called *<project>-TODO* instead of *grep*.
  (eproject-grep (regexp-opt eproject-todo-expressions)))



;;}}}
;;{{{ Package: fill

;; Each list element as new paragraph
;; http://www.emacswiki.org/cgi-bin/wiki/FillParagraph
(setq paragraph-start    " *\\([*+-]\\|\\([0-9]+\\|[a-zA-Z]\\)[.)]\\|$\\)"
      paragraph-separate "$")

;; Do not break line after single character when filling
(defun fill-single-char-nobreak-p ()
  "Don't break line after a single character."
  (save-excursion
    (skip-chars-backward " \t")
    (backward-char 2)
    (looking-at "[[:space:]][a-zA-Z]")))

(add-to-list 'fill-nobreak-predicate 'fill-single-char-nobreak-p)


;;}}}
;;{{{ Package: ibuffer

(require 'ibuffer)

(setq ibuffer-display-summary nil
      ;;ibuffer-use-header-line t
      ;;ibuffer-default-sorting-mode 'major-mode
      ;;
      ;; Don't ask when killing a buffer
      ibuffer-expert t
      ;;
      ibuffer-show-empty-filter-groups nil
      ibuffer-old-time 4
      ;; And now my filters:
      ibuffer-saved-filter-groups
      '(("default"
	 ("dired" (mode . dired-mode))
	 ("erc" (mode . erc-mode))
	 ("Agenda" (or
		    (name . "^\\*Calendar\\*$")
		    (name . "^diary$")
		    (name . "^\\*Org.*")
		    (mode . muse-mode)))
	 ("Mail" (or
		  (name . "^contacts$")
		  (name . "^\\*BBDB\\*$")
		  (name . "^Folder$")
		  (name . "^Summary$")
		  (name . "^\\.draft/")))
	 ("magit" (name . "^\\*magit"))
	 ("emacs" (name . "^\\*"))
	 )))

;; reverse group order
(defadvice ibuffer-generate-filter-groups (after reverse-ibuffer-groups ()
						   activate)
  (setq ad-return-value (nreverse ad-return-value)))

(add-hook 'ibuffer-mode-hook
	  (lambda ()
	    (ibuffer-auto-mode 1)
	    (ibuffer-switch-to-saved-filter-groups "default")))

(defun my-ibuffer ()
  "Open ibuffer with cursor pointed to most recent buffer name"
  (interactive)
  (let ((recent-buffer-name (buffer-name)))
    (ibuffer)
    (ibuffer-jump-to-buffer recent-buffer-name)))

(global-set-key "\C-x\C-b" 'my-ibuffer)
;; ORIGINAL: list-buffers

;; http://curiousprogrammer.wordpress.com/2009/04/02/ibuffer/
(defun ibuffer-ediff-marked-buffers ()
  (interactive)
  (let* ((marked-buffers (ibuffer-get-marked-buffers))
         (len (length marked-buffers)))
    (unless (= 2 len)
      (error (format "%s buffer%s been marked (needs to be 2)"
                     len (if (= len 1) " has" "s have"))))
    (ediff-buffers (car marked-buffers) (cadr marked-buffers))))
(define-key ibuffer-mode-map "e" 'ibuffer-ediff-marked-buffers)
;; ORIGINAL: ibuffer-visit-buffer


;;}}}
;;{{{ Package: ido

(require 'ido)
(eval-after-load "ido"
  '(setq ido-save-directory-list-file (concat dotfiles-dir "tmp/ido.last")
	 ;;ido-work-directory-list '()
	 ido-everywhere t			; use for many file dialogs
	 ido-max-work-file-list      50		; remember many
	 ido-enable-flex-matching t		; be flexible
	 ido-max-prospects 4			; don't spam my minibuffer
	 ido-confirm-unique-completion t	; wait for RET, even with unique completion
	 ;;ido-enable-dot-prefix t		; need "." to select hidden files
	 ido-enable-tramp-completion nil
	 ido-ignore-buffers
	 '("\\`"
	   "^\*Mess"
	   "^\*Help*"
	   "^\*Back"
	   ".*Completion"
	   "^\*Ido")
	 ido-ignore-directories
	 '("\\`CVS/"
	   "\\.svn/"
	   "\\.git/"
	   "\\`\\.\\./"
	   "\\`\\./")
	 ))

(ido-mode 'both)


;;}}}
;;{{{ Package: magit

;; Magit is now loaded via package.el (elpa)
(eval-after-load "magit"
  '(progn
     (setq magit-save-some-buffers nil
	   magit-omit-untracked-dir-content t)))

(global-set-key "\M-g\M-m" 'magit-status)
;; ORIGINAL: undefined

(global-set-key "\M-gm" 'magit-status)
;; ORIGINAL: undefined



;;}}}
;;{{{ Package: maxima

(defun my-maxima-inferior-setup ()
  (setq yas/dont-activate t))

(eval-after-load "imaxima"
  '(progn (setq imaxima-fnt-size "Large"
		imaxima-latex-preamble"\\usepackage{concrete}"
		imaxima-use-maxima-mode-flag t)
	  (add-hook 'inferior-maxima-mode-hook 'my-maxima-inferior-setup)
	  ))

(autoload 'imaxima "imaxima" nil t)


;; This function opens an imaxima buffer in the background. When I use
;; C-c C-c in the maxima-mode, the already started imaxima buffer will
;; then be re-used by maxima-display-buffer. That way I have the image
;; capable imaxima instead of the text-only maxima buffer.
(defun my-maxima-setup()
  (let ((oldbuf (current-buffer)))
    (require 'imaxima)
    (imaxima)
    (switch-to-buffer oldbuf))
  )

(eval-after-load "maxima"
  '(progn (define-key inferior-maxima-mode-map "\t" 'inferior-maxima-complete)
	  (setq maxima-use-full-color-in-process-buffer t)
	  (add-hook 'maxima-mode-hook 'my-maxima-setup)
	  ))

(autoload 'maxima-mode "maxima" nil t)

(add-to-list 'auto-mode-alist '("\\.mac$" . maxima-mode))



;;}}}
;;{{{ Package: md-mode

(autoload 'md-mode "md-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.md$" . md-mode))

;;}}}
;;{{{ Package: mediawiki

;; http://www.emacswiki.org/emacs/mediawiki.el
;; (auto-install-from-url "http://launchpadlibrarian.net/59170085/mediawiki.el")
;; "http://bazaar.launchpad.net/~hexmode/mediawiki-el/trunk/download/head%3A/mediawiki.el-20100227051241-nk34zzd7aq6b02gm-1/mediawiki.el")
(eval-after-load "mediawiki"
  '(progn (setq mediawiki-site-default "Mediawiki"
		mediawiki-pop-buffer-hook '(delete-other-windows)
		mediawiki-draft-data-file (concat dotfiles-dir "tmp/draft.wiki")
		)
	  (add-to-list 'mediawiki-site-alist
		       (list "DARC"
		       	     "http://wiki.darc.de/"
		       	     "DH3HS"
		       	     "dtgabzd0"
		       	     "Harzburg"))
	  (add-to-list 'mediawiki-site-alist
		       (list "Mediawiki"
			     "http://www.mediawiki.org/w/"
			     "HolgerSchurig"
			     "dtgabzm"
			     "Sandbox"))
     ))

;; (require 'mediawiki)
;; (mediawiki-site "Mediawiki")
;; (mediawiki-site "DARC")
;; (mediawiki-open "Sandbox")
(autoload 'mediawiki-open "mediawiki" nil t)



;;}}}
;;{{{ Package: pabbrev

(autoload 'pabbrev-mode "pabbrev" nil t)
(autoload 'global-pabbrev-mode "pabbrev" nil t)

;; HINT: pabbrev.el comes from EmacsWiki, you can update it
;; with (auto-install-from-emacswiki)

;; (require 'pabbrev)
;; (global-pabbrev-mode t)
;; (setq pabbrev-read-only-error nil)



;;}}}
;;{{{ Package: package           (Emacs package manager)

(eval-after-load "package"
  '(package-initialize))

(require 'package nil 'noerror)



;;}}}
;;{{{ Package: paredit

;; Paredit is now loaded via package.el (elpa)
(autoload 'paredit-mode "paredit" nil t)

;; Automatically turn on in elist mode
;; (add-hook 'emacs-lisp-mode-hook 'paredit-mode)



;;}}}
;;{{{ Package: rcirc

(eval-after-load "rcirc"
  '(progn
     (setq rcirc-prompt "%t> "
	   ;; Turn on logging everything to a special buffer, for debugging.
	   ;;rcirc-debug-flag
	   rcirc-fill-prefix "      "
	   ;; Use max. frame width
	   rcirc-fill-column 'frame-width
	   ;; Colorize inside text
	   rcirc-keywords '("schurig")
	   ;; colorize important :-) nicks
	   rcirc-bright-nicks '("schurig")
	   ;; Omit JOIN/PART/QUIT/NICK (from rcirc-omit-responses)
	   rcirc-omit-mode t
	   ;; Automatically connect:
	   rcirc-server-alist
	   '(("irc.freenode.net"
	      :channels ("#emacs"))
	     ;; ("irc.datacomm.ch"
	     ;;  :channels ("#drsrm"))
	     ;; ("irc.perl.org"
	     ;;  :channels ("#perlde"))
	     )
	   ;;rcirc-decode-coding-system 'undecided
	   ;;rcirc-coding-system-alist '(("#nihongo" undecided . iso-2022-jp))
	   rcirc-authinfo
	   '(("freenode" nickserv "schurig" "dtgabzi")
	     ;;("freenode" chanserv "bob" "#bobland" "passwd99")
	     ;;("bitlbee" bitlbee "robert" "sekrit")
	     ))
     ))



;;}}}
;;{{{ Package: recentf

;; Save recent files
(setq recentf-save-file (concat dotfiles-dir "tmp/recentf.el")
      recentf-exclude '("bbdb$"
			"svn-commit.tmp$"
			".git/COMMIT_EDITMSG$"
			".git/TAG_EDITMSG")
      recentf-max-saved-items 1000
      recentf-auto-cleanup 300
      recentf-max-menu-items 20)

(recentf-mode 1)



;;}}}
;;{{{ Package: server

;; Automatically start server, even when run interactively
(require 'server)
(unless (server-running-p)
  (server-start))

;; Make sure the frame pops up as a graphical frame
(setq server-window '(lambda (buf)
		       (switch-to-buffer buf)
		       (raise-frame))
      server-temp-file-regexp "^/tmp/Re\\|/draft\\|/.git/COMMIT_EDITMSG\\|/.git/TAG_EDITMSG$")

;; Kill buffers when done (M-x #)
(add-hook 'server-done-hook (lambda nil (kill-buffer nil)))




;;}}}
;;{{{ Package: tramp

(setq tramp-persistency-file-name (concat dotfiles-dir "tmp/tramp")
      tramp-default-method "ssh"
      ;; Relax prompt checking
      tramp-shell-prompt-pattern "^#$>\n]*[#$%>] *")



;;}}}
;;{{{ Package: unbound

;; http://www.emacswiki.org/emacs/download/unbound.el
;;
;; HINT: unbound.el comes from EmacsWiki, you can update it
;; with (auto-install-from-emacswiki)
(autoload 'describe-unbound-keys "unbound"
  "Display a list of unbound keystrokes of complexity no greater than max." t)



;;}}}
;;{{{ Package: uniquify

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets
      uniquify-separator "/"
      uniquify-after-kill-buffer-p t
      uniquify-ignore-buffers-re "^\\*")



;;}}}
;;{{{ Package: visual-basic

(autoload 'visual-basic-mode "visual-basic-mode" nil t)

(add-to-list 'auto-mode-alist '("\\.vba$" . visual-basic-mode))



;;}}}
;;{{{ Disabled Package: skeleton

;; Skeleton pairs
;; (global-set-key (kbd "(") 'skeleton-pair-insert-maybe)
;; (global-set-key (kbd "[") 'skeleton-pair-insert-maybe)
;; (global-set-key (kbd "{") 'skeleton-pair-insert-maybe)
;; (global-set-key (kbd "\"") 'skeleton-pair-insert-maybe)

;; ;; If this function returns nil, then pairing is attempted
;; (defun my-skeleton-pair-filter-function ()
;;   (cond
;;    ;; if the just-entered skeleton char is a "
;;    ((eq last-command-char ?\")
;;     (or (looking-at   (regexp-quote (string last-command-char)))
;; 	(looking-back (regexp-quote (string last-command-char)))
;; 	(looking-back "[[:graph:]]")
;; 	))
;;    ;; For other skeleton chars
;;    (t
;;     (looking-at (regexp-quote (string last-command-char))))))

;; (setq skeleton-pair t
;;       skeleton-pair-filter-function 'my-skeleton-pair-filter-function)



;;}}}
;;{{{ Key bindings

(global-set-key "\C-xE"		'apply-macro-to-region-lines)
;; ORIGINAL: kmacro-and-and-call-macro (on \C-Xe)

(global-set-key "\C-x\\"	'align-regexp)
;; ORIGINAL: undefined

(global-set-key "\C-c\C-f"	'ff-find-other-file)
;; ORIGINAL: undefined

(global-set-key "\C-cc"		'comment-region)
;; ORIGINAL: undefined

(global-set-key "\C-cu"		'uncomment-region)
;; ORIGINAL: undefined

(global-set-key (kbd "C-;")     'comment-dwim)
;; ORIGINAL: undefined


;; Don't iconify
(when window-system
  (global-unset-key "\C-z"))

;; Enable some normally disabled functions
;; (put 'capitalize-region 'disabled nil)
;; (put 'dired-find-alternate-file 'disabled nil)
;; (put 'downcase-region 'disabled nil)
;; (put 'erase-buffer 'disabled nil)
;; (put 'eval-expression 'disabled nil)
;; (put 'narrow-to-region 'disabled nil)
;; (put 'scroll-left 'disabled nil)
;; (put 'upcase-region 'disabled nil)

;; Enable all disabled commands
(setq disabled-command-function nil)

;; Don't bother entering search and replace args if the buffer is read-only
(defadvice query-replace-read-args (before barf-if-buffer-read-only activate)
  "Signal a `buffer-read-only' error if the current buffer is read-only."
  (barf-if-buffer-read-only))



;;}}}
