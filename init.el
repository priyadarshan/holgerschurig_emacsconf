;; -*- mode: emacs-lisp; mode: fold -*-

;; SEE: http://github.com/mina86/dot-files/raw/master/dot-emacs
;; SEE: http://www.emacswiki.org/emacs/download/.emacs-thierry.el
;; SEE: http://www.jurta.org/en/emacs/dotemacs
;; SEE: http://www.djcbsoftware.nl/dot-emacs.html



;;{{{ Debugging emacs

;; Provide a useful error trace if loading this monster fails
(setq debug-on-error t)

(defconst ms-windows (equal window-system 'w32))



;;}}}
;;{{{ Load path

(dolist (i '(
	     "~/.emacs.d/cedet/"
	     "~/.emacs.d/cedet/common/"
	     "~/.emacs.d/ecb/"
	     "~/.emacs.d/icicles/"
	     "~/.emacs.d/magit/"
	     "~/.emacs.d/"
	   ))
  (when (not (member i load-path))
    (add-to-list 'load-path (expand-file-name i))))



;;}}}
;;{{{ Auto byte-compile

(require 'bytecomp)

(defvar auto-byte-compile-files-list
  (let ((ghd (or (and (boundp 'gnus-home-directory) gnus-home-directory)
		 (expand-file-name "~/.gnus"))))
    (cond
     ((string-match "\\.elc$" (or user-init-file "/home/mina86/.emacs.d/init.el"))
      (list (substring user-init-file 0 -4)
	    (substring user-init-file 0 -1)
	    (concat ghd "/.gnus")
	    (concat ghd "/.gnus.el")))
     (t
      (list user-init-file
	    (concat ghd "/.gnus")
	    (concat ghd "/.gnus.el")))))
  "List of files to auto compile")

(defun auto-byte-compile-file (&optional file match regexp)
  "file can be
- nil in which case value returned by `buffer-file-name' will be used
  unless it returns nil in which case no action will be taken;
- a string which is equivalent to passing list with that string as the
  only element;
- a list of strings representing file names; or
- anything else which is equivalent to passing
  `auto-byte-compile-files-list'.

Entries equal to \".\", \"..\" or ending with \"/.\" or \"/..\"
are ignored.  Directories starting with a dot will be ignored.
If element is a directory it will be processed recursively but if
regexp is nil only files ending with \".el\" will be processed.

match can be
- nil which is equivalent to passing `auto-byte-compile-files-list';
- a string which is equivalent to passing list with that string as the
  only element;
- a list in which case file have to be in that list to be processed; or
- anything else in which case file will be processed regardless of name.

If any element of match is a string ending with a slash ('/') it
is treated as directory name (no checking is done if it is really
a directory or even if it exists) and file is said to match such
entry if it begins with it thus all files in given directory will
match.

If called interacivelly without prefix arg will behave as with
match equal t.  With prefix arg will behave as with match equal
nil.

regexp must be nil which is equivalent with passing a list
containing only empty string or a list of regular expressions
which file have to match to be processed.

So the default is to auto-compile the current file iff it exists
in `auto-byte-compile-files-list'.

Non-string elements in list will be ignored.

Auto-compilation means that file will be byte-compiled iff the
compiled version does not exits or is older then the file
itself."
  (interactive (list (read-file-name "Auto byte compile file:" nil nil t)
		     (not current-prefix-arg)))

  (if (not (or file (setq file (buffer-file-name))))
      0
    (setq file  (cond ((stringp file)  (list file))
		      ((listp   file)  file)
		      (t               auto-byte-compile-files-list))
	  match (mapcar (function (lambda (i) (expand-file-name i)))
			(cond ((not match)     auto-byte-compile-files-list)
			      ((stringp match) (list match))
			      ((listp match)   match)
			      (t               nil))))

    (let (f (n 0))
      (while (setq f (car file))
	(setq file (cdr file) f (expand-file-name f))
	(cond
	 ((string-match f "\\(?:^\\|/\\)\\.\\.?$"))
	 ((file-directory-p f)
	  (unless (string-match f "\\(?:^\\|/\\)\\.")
	    (if regexp
		(setq file (append (directory-files f t nil t) file))
	      (setq n (+ n (auto-byte-compile-file (directory-files f t nil t)
						   (or match t) '("\\.el$")))))))
	 ((and (file-newer-than-file-p f (byte-compile-dest-file f))
	       (or (not match)
		   (catch 'found
		     (dolist (m match)
		       (if (string= m (if (string-match "/$" m)
					  (substring f 0 (length m)) f))
			   (throw 'found t)))))
	       (or (not regexp)
		   (catch 'found (dolist (r regexp)
				   (if (string-match r f) (throw 'found t)))))
	       (byte-compile-file f)
	       (setq n (1+ n))))))
      n)))

(defun auto-byte-compile-buffer (&optional match buffer)
  "Auto compiles file in given buffer (if buffer is nil current
buffer is used) providing that major mode of the buffer is
lisp-mode or emacs-lisp-mode.  match has the same meaning as in
`auto-byte-compile-file'.

If called interacivelly will behave as with match equal t and
buffer equal nil unless prefix argument was given in which case
match will equal nil."
  (interactive (list (not current-prefix-arg) nil))
  (and (buffer-file-name buffer)
       (memq (if buffer (save-current-buffer (set-buffer buffer)
					     major-mode) major-mode)
	     '(lisp-mode emacs-lisp-mode))
       (auto-byte-compile-file (buffer-file-name buffer) match)))


(add-hook 'kill-buffer-hook 'auto-byte-compile-buffer)
(add-hook 'kill-emacs-hook (function (lambda () (auto-byte-compile-file t))))



;;}}}
;;{{{ Functions: Deleting

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
(setq scroll-conservatively 1)
(setq scroll-preserve-screen-position t)



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
(global-set-key (kbd "\C-j") 'newline)



;;}}}
;;{{{ Functions: Searching

;; Scrolling while searching
(setq isearch-allow-scroll t)

;; Save Isearch stuff
(setq isearch-resume-in-command-history t)

;; (add-hook 'isearch-mode-end-hook
;;           (lambda ()
;;             ;; On typing C-RET
;;             (when (eq last-input-event 'C-return)
;;               ;; Set the point at the beginning of the search string
;;               (if (and isearch-forward isearch-other-end)
;; 		  (goto-char isearch-other-end))
;;               ;; Don't push the search string into the search ring
;;               (if isearch-regexp
;;                   (setq regexp-search-ring (cdr regexp-search-ring))
;;                 (setq search-ring (cdr search-ring))))))

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


(defun isearch-occur ()
  "Invoke `occur' from within isearch."
  (interactive)
  (let ((case-fold-search isearch-case-fold-search))
    (occur (if isearch-regexp isearch-string (regexp-quote isearch-string))))
  (pop-to-buffer "*Occur*"))
;; WAS: isearch-other-control-char
(define-key isearch-mode-map (kbd "C-o") 'isearch-occur)

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


;; (global-set-key "\C-x k" 'kill--buffer-and-window)

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



;;}}}
;;{{{ Functions: Buffers

(defun my-zoom-next-buffer ()
  "Search for buffers that have are visiting a file. If one is found,
then current buffer is buried, and the first buffer in the list will
then be visited. That way, you can cycle throught all open files.

While doing this, all other windows are shrinked, so that only one
big window will be displayed."
  (interactive)

  (delete-other-windows)

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
(global-set-key "\M-n" 'my-zoom-next-buffer)

;; Insert buffer at current position
(global-set-key "\C-xI" 'insert-buffer)

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

(defun my-compile (&optional recompile touch)
  "If touch is non-nil marks buffer as modified.  Saves
buffer (`save-buffer') and executes `recompile' if recompile is
non-nill or `compile' otherwise."
  (interactive "P")
  (delete-other-windows)
  (when touch
    (set-buffer-modified-p t))
  (save-buffer)
  (if (or (eq major-mode 'lisp-mode) (eq major-mode 'emacs-lisp-mode))
      (progn
	(my--kill-buffer-and-window (get-buffer-create "*Compile-Log*"))
	(auto-byte-compile-file nil t))
    (progn
      (my--kill-buffer-and-window (get-buffer-create "*compilation*"))
      (if recompile (recompile) (call-interactively 'compile)))))
(global-set-key [(f7)] 'my-compile)
(global-set-key [(shift f7)]
		(function (lambda (a) (interactive "P") (my-compile a t))))



;; Helper for compilation. Close the compilation window if there was
;; no error at all.
(defun compilation-exit-autoclose (status code msg)
  ;; If M-x compile exists with a 0
  (when (and (eq status 'exit) (zerop code))
    ;; then bury the *compilation* buffer, so that C-x b doesn't go there
    (bury-buffer "*compilation*")
    ;; and delete the *compilation* window
    (delete-window (get-buffer-window (get-buffer "*compilation*"))))
  ;; Always return the anticipated result of compilation-exit-message-function
  (cons msg code))
(setq compilation-exit-message-function 'compilation-exit-autoclose
      compilation-ask-about-save nil
      compilation-scroll-output t)

(global-set-key [(f8)] 'next-error)
(global-set-key [(shift f8)] 'previous-error)



;;}}}
;;{{{ Mouse

;; paste at text-cursor, not at mouse-cursor
(setq mouse-yank-at-point t)

;; Show the text pointer in text areas
;;(setq void-text-area-pointer nil)

(eval-after-load "avoid"
  '(progn
     ;; Move the mouse to the lower-right corner instead of default upper-right
     ;; (defun mouse-avoidance-banish-destination ()
     ;;   (cons (- (frame-width) 1) (- (frame-height) 1)))
     (setq mouse-avoidance-timer-delay 0.1)
     (mouse-avoidance-mode 'banish)))
(unless ms-windows
  (when (display-mouse-p) (require 'avoid nil t)))



;;}}}
;;{{{ X-Windows cut'n'paste

;; Use clipboard of X
;; (setq x-select-enable-clipboard t
;;       interprogram-paste-function 'x-cut-buffer-or-selection-value)



;;}}}
;;{{{ Entering/exiting Emacs

;; get rid of yes-or-no questions - y or n is enough
(fset 'yes-or-no-p 'y-or-n-p)

;; This inhibits the initial startup echo area message.
;;(setq inhibit-startup-echo-area-message "schurig")

;; Do without annoying startup msg.
(setq inhibit-startup-message t)

;; Empty scratch message
;(setq initial-scratch-message nil)

;; Include current buffer name in the title bar
(setq frame-title-format '(buffer-file-name "%f" ("%b")))

;; Don't ask for killing emacs
;;(setq confirm-kill-emacs t)

;; Set up default editing mode.
(setq default-major-mode 'indented-text-mode)

;; Custom file
(setq custom-file "~/.emacs.d/custom.el")
(if (file-exists-p custom-file) (load-file custom-file))
(if (boundp 'auto-byte-compile-files-list)
    (setq auto-byte-compile-files-list
	  (cons custom-file auto-byte-compile-files-list)))

;; Save recent files
;(require 'recentf)
(setq recentf-save-file "~/.emacs.d/recentf")
(and (fboundp 'recentf-mode) (recentf-mode 1))


;;}}}
;;{{{ Entering text

;; use decimal for `C-q'
(setq read-quoted-char-radix 10)



;;}}}
;;{{{ File opening/saving

;; find file at point
(require 'ffap)

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

;; Preserve hard links to the file you´re editing (this is
;; especially important if you edit system files)
(setq backup-by-copying-when-linked t)

;; Just never create backup files at all
(setq make-backup-files nil)

;; Emacs is a text editor, make sure your text files end in a newline
(setq require-final-newline t)

;; Disable auto-save (#init.el# file-names)
(setq auto-save-default nil)

;; Don't open Qt's *.pro files as IDLWAVE files
(add-to-list 'auto-mode-alist '("\\.pro$" . fundamental-mode))

;; Open *.h files normally in c++ mode
(add-to-list 'auto-mode-alist '("\\.h$" . c++-mode))


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



;;}}}
;;{{{ Display: Frame display

;; ~/.Xresources
;; Emacs*geometry: 90x60+0+0
;; Emacs.Font: terminus 11
;; Emacs.verticalScrollBars: off
;; Emacs.toolBar: off
;; Emacs*Background: #000000
;; Emacs*Foreground: #ffffff
;;
;; xrdb -merge .Xresources
;; appres Emacs

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
;; 			    (width . 90)
;; 			    (height . 67)
;; 			    (left-fringe . 0)
;; 			    (right-fringe . 0)
;; 			    (menu-bar-lines . 1)
;; 			    (tool-bar-lines . 1)
;; 			    (left . 0))

(if ms-windows
    (setq initial-frame-alist
	  '(
	    (background-color . "black")
	    (foreground-color . "white")
	    (cursor-color . "white")
	    (horizontal-scroll-bars . nil)
	    (vertical-scroll-bars . nil)
	    (tool-bar-lines . 0)
	    (top . 0)
	    (left . 0)
	    (left-fringe . 0)
	    (right-fringe . 0)))
  (setq initial-frame-alist
	`(;;(font . ,(if my-win32
	  ;;	     (if have-win32-sixbyten-font
	  ;;		 "-raster-sixbyten-normal-r-normal-normal-10-75-96-96-c-60-iso10646-1"
	  ;;	       "-outline-Lucida Console-normal-r-normal-normal-11-82-96-96-c-*-iso8859-1")
	  ;;	   ;; X's 6x10 font ...  Why not use "6x10" here?
	  ;;	   "-Misc-Fixed-Medium-R-Normal--10-100-75-75-C-60-ISO8859-1"))
	  (background-color . "black")
	  (foreground-color . "white")
	  (horizontal-scroll-bars . nil)
	  (vertical-scroll-bars . nil)
	  (tool-bar-lines . 0)
	  (top . 0)
	  (left . 0)
	  ;;(height . ,(if (or (not my-win32)
	  ;;		   have-win32-sixbyten-font)
	  ;;	       (my-frame-percent-to-char-height 97)
	  ;;	     70))
	  (heigh . 67)
	  (width . 90)
	  (left-fringe . 0)
	  (right-fringe . 0)
	  (mouse-color . "green")
	  )))

;; default-frame-alist is defined in terms of initial-frame-alist.  Don't
;; use copy-sequence here -- it doesn't copy the list elements, just the
;; list's cons cells.  Use copy-alist instead.
(setq default-frame-alist (copy-alist initial-frame-alist))


;;}}}
;;{{{ Display: Faces

;; ;; http://www.jurta.org/en/emacs/dotemacs
;;
;; (defun my-colors-dark (&optional frame)
;;   "Set colors suitable for working in the darkness without electricity."
;;
;;   (interactive)
;;   (if frame
;;       (select-frame frame)
;;     (setq frame (selected-frame)))
;;   (set-background-color "black")
;;   (set-foreground-color "DarkGrey")
;;   (when (facep 'region)
;;     (set-face-background 'region "DimGray" frame))
;;   (when (facep 'fringe)
;;     (set-face-background 'fringe (face-background 'default) frame)
;;     (set-face-foreground 'fringe (face-foreground 'default) frame)))
;; (my-colors-dark)
;;
;; ;; Colorize newly created frames
;; (add-hook 'after-make-frame-functions 'my-colors-dark)

;; http://www.emacswiki.org/cgi-bin/wiki/EightyColumnRule
(defface my--todo-face
  '((t :foreground "red"
       :weight bold))
  "Font for showing TODO words."
  :group 'basic-faces)

(defface my--fixme-face
  '((t :background "red"
       :foreground "white"
       :weight bold))
  "Font for showing FIXME and XXX words."
  :group 'basic-faces)

(defface my--hint-face
  '((t :foreground "green"
       :weight bold))
  "Font for showing HINT words."
  :group 'basic-faces)

(defun my--hint-facify ()
   (unless (or (eq 'diff-mode major-mode) (eq 'script-mode major-mode))
     (font-lock-add-keywords nil '(
         ;;("\t+" 0 'my--tab-face t)
	 ("\\<\\(TODO\\(\\?|:\\)?\\)\\>" 1 'my--todo-face t)
	 ("\\<\\(FIXME:\\|XXX\\)\\>" 1 'my--fixme-face t)
	 ("\\<\\(HINT:\\)\\>" 1 'my--hint-face t)
	 ))))

(add-hook 'font-lock-mode-hook 'my--hint-facify)



;;}}}
;;{{{ Display: Font lock

;; Activate font-lock-mode (syntax coloring)
(setq global-font-lock-mode t)
(setq font-lock-verbose nil)

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
;;{{{ Display: whitespace, 80columns

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
      )
  (progn
    ;; Visial bell only in text mode
    (setq visible-bell t)
    ))

;; Let parenthesis behave
(show-paren-mode 1)
(setq show-paren-delay 0
      blink-matching-parent nil)

;; Display page delimiter ^L as a horizontal line
(or standard-display-table (setq standard-display-table (make-display-table)))
(aset standard-display-table ?\f (vconcat (make-vector 72 ?-) "^L"))

;; Let emacs react way faster
(setq echo-keystrokes 0.1
      idle-update-delay 0.35)



;;}}}
;;{{{ Menue

;; get rid of the Games in the Tools menu
(define-key menu-bar-tools-menu [games] nil)

;; Default was 80000
(setq undo-limit 30000)


;;}}}
;;{{{ Printing

(setq lpr-command "kprinter")


;;}}}
;;{{{ Misc

;; A sentence doesn't end with two spaces (in german)
(setq sentence-end-double-space nil)

;; Delete previous identical history entries
(setq history-delete-duplicates t)

;; Allow german umlaut characters
(unless ms-windows
  (set-language-environment "German"))



;;}}}
;;{{{ Completion

;; ignore case when reading a file name completion
(setq read-file-name-completion-ignore-case t)

;; do not consider case significant in completion (GNU Emacs default)
(setq completion-ignore-case t)

;; Doesn't work with ido
;; (partial-completion-mode 1)
;; (setq completion-auto-help 'lazy)




;;}}}
;;{{{ Web-Browsing

(setq browse-url-browser-function 'browse-url-kde)



;;}}}
;;{{{ Mode: C, C++

;; Tabs mode
(setq indent-tabs-mode t)
(setq c-basic-offset 8)
(setq tab-width 8)

;; Silence byte-compiler
(require 'cc-mode)

(defun c-newline-and-perhaps-comment (&optional soft)
  "Insert a newline and continue commenting if inside a C style comment.
This function usually inserts a newline character.  However it has
several other actions;

If point is within a one line comment /* ... */ then point is moved to the
next line of code .

If point is directly after /* then a multi-line comment block is written
and point placed on the first line.

If point is within a multi-line comment, then a newline starting with a
'*' is added at the correct indentation.

If point is after an empty line, any remaining white space is removed.

The inserted newline is marked hard if `use-hard-newlines' is true,
unless optional argument SOFT is non-nil."
  (interactive)
  (let ((auto-fill-function nil))
    (save-match-data
      (cond (;;
	     ;; Inside a one line comment?
	     ;;
	     (and (save-excursion
		    (end-of-line)
		    (let ((eol-pos (point)))
		      (beginning-of-line)
		      (re-search-forward "/\\*.*\\*/" eol-pos t)))
		  (>= (point) (match-beginning 0))
		  (<= (point) (match-end 0)))
	     ;;
	     ;; Then goto next line.
	     ;;
	     (end-of-line)
	     (if soft (insert ?\n) (newline))
	     (indent-according-to-mode))
	    (;;
	     ;; Inside a block comment?
	     ;;
	     (save-excursion
	       (and (re-search-backward "/\\*\\|\\*/" 0 t)
		    (string= "/*" (buffer-substring (match-beginning 0)
						    (match-end 0)))))
	     ;;
	     ;; Check if we just wrote "/*", if so build a comment block.
	     ;;
	     (if (save-excursion
		   (end-of-line)
		   (re-search-backward "/\\*\\([^ \t]*\\)\\(.*\\)"
				       (save-excursion (beginning-of-line)
						       (point)) t))
		 (let ((col (save-excursion (goto-char (match-beginning 0))
					    (current-column)))
		       (start (buffer-substring (match-beginning 1)
						(match-end 1)))
		       (text (buffer-substring (match-beginning 2)
					       (match-end 2))))
		   (if (/= (length text) 0)
		       (delete-region (match-beginning 2) (match-end 2))
		     (setq text " "))
		   (if soft (insert ?\n) (newline))
		   (indent-to-column col)
		   (insert " *" text)
		   (if soft (insert ?\n) (newline))
		   (indent-to-column col)
		   (if (/= (length start) 1)
		       (insert " "))
		   ;;
		   ;; Handle JavaDoc convention correctly. (ie. /** ... */)
		   ;;
		   (if (string-equal start "*")
		       (insert " ")
		     (insert start))
		   (insert "*/")
		   (forward-line -1)
		   (end-of-line))
	       ;;
	       ;; Otherwise continue the comment block.
	       ;;
	       (if soft (insert ?\n) (newline))
	       (indent-according-to-mode)
	       (insert "*")
	       (indent-relative)))
	    (;;
	     ;; After an empty line?
	     ;;
	     (save-excursion
	       (beginning-of-line)
	       (looking-at "[	]+$"))
	     (delete-region
	      (match-beginning 0)
	      (match-end 0))
	     (if soft (insert ?\n) (newline))
	     (indent-according-to-mode))
	    (;;
	     ;; Otherwise just do a normal newline.
	     ;;
	     (message "normal newline")
	     (if soft (insert ?\n) (newline))
	     (indent-according-to-mode))
	    ))))

(defun my-c-mode-common-setup ()
  (define-key c-mode-map (kbd "RET") 'c-newline-and-perhaps-comment)
  (turn-on-auto-fill)
  (c-toggle-auto-newline 1)
  (setq fill-column 76
	;; Let RET break and continue a comment
	comment-line-break-function 'c-newline-and-perhaps-comment
	;; C doesn't start functions with a ( in the first column
	open-paren-in-column-0-is-defun-start nil
	;; Insert TABs if needed
	;;c-tab-always-indent nil
	;; Tell cc-mode not to check for old-style (K&R) function
	;; declarations. This speeds up indenting a lot (I hear).
	c-recognize-knr-p nil
	;; Jump to errors, please
	compilation-auto-jump-to-first-error t
	;; Turn of elect
	;;c-electric-flag nil
	c-electric-pound-behavior '(alignleft)
	;; No abbrevs
	abbrev-mode nil
	;; Default style
	c-default-style '((java-mode . "java")
			  (awk-mode . "awk")
			  (other . "linux"))
	)
  )
(add-hook 'c-mode-common-hook 'my-c-mode-common-setup)

;; Linux style for linux :-)
;; (defun my-c-mode-hook ()
;;   (c-set-style
;;    (if (and (buffer-file-name)
;; 	    (string-match "/usr/src/linux" (buffer-file-name)))
;;        "linux"
;;      "free-group-style")))
;; (add-hook 'c-mode-hook 'my-c-mode-hook)


;; (c-set-offset XXX)
;; (c-add-style XXX)

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
(setq srecode-map-save-file "~/.emacs.d/srecode-map")
(setq semanticdb-default-save-directory "~/.emacs.d/semanticdb")

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

;; provide some dired goodies and dired-jump at C-x C-j
(require 'dired-x)

;; HINT: next expression is very useful for M-( in Dired mode:
;; (string-match "perl" (shell-command-to-string (concat "file " name)))



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

;; (add-to-list 'Info-default-directory-list "~/.emacs.d/info")

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
;; (defadvice info (before info activate)
;;   (select-frame (make-frame))
;;   )


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
(define-key emacs-lisp-mode-map [(control meta tab)] 'lisp-complete-symbol)
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
(define-key lisp-interaction-mode-map [(control meta tab)] 'lisp-complete-symbol)

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

;; SEE: http://www.emacswiki.org/emacs/org-config-thierry.el
;; http://www.newartisans.com/2007/08/using-org-mode-as-a-day-planner.html

(require 'org-install)

(setq org-directory "~/.emacs.d"
      org-agenda-files (list "~/.emacs.d/agenda.org"
			     )
      ;; Opening/closing .org mode
      org-agenda-restore-windows-after-quit t
      org-agenda-window-setup 'current-window

      ;; Skip done items
      org-agenda-skip-deadline-if-done t
      org-agenda-skip-scheduled-if-done t

      ;; Let agenda starts on the current day
      org-agenda-start-on-weekday nil

      ;; Store notes at beginning of file
      org-reverse-note-order t

      ;; Don't show TODO items at first
      ;;(org-fast-tag-selection-single-key 'expert)

      ;; For C-c a #
      org-stuck-projects
      '("+LEVEL=2/-DONE"
	;; TODO-keyword identifying non-stuck projects:
	("TODO" "CANCELLED")
	;; Tags identifying non-stuck projects:
	nil
	;; Arbitrary reg-exp identifying non-stuck projects:
	"")

      ;; Some special view to select from after C-c a
      org-agenda-custom-commands
      '(("f" "Finished" todo "DONE|CANCELLED" nil)
	("w" "Waiting" todo "WAIT|FORWARD" nil)
	("3" "3 week agenda" agenda "" ((org-agenda-ndays 21)))
	;; ("A" "A-Tasks" agenda ""
	;;  ((org-agenda-skip-function
	;;    (lambda nil
	;;      (org-agenda-skip-entry-if 'notregexp "\\=.*\\[#A\\]")))
	;;   (org-agenda-ndays 1)
	;;   (org-agenda-overriding-header "Today's Priority #A tasks: ")))
	("u" "unscheduled" alltodo ""
	 ((org-agenda-skip-function
	   (lambda ()
	     (org-agenda-skip-entry-if 'scheduled
				       'deadline
				       'regexp "<[^>\n]+>"
				       )))
	  (org-agenda-overriding-header "Unscheduled TODO entries: "))))

      ;; Add a space before the elipsis
      org-ellipsis " ..."

      ;; No need to add a DONE log entry, as our #+TODO: line makes a
      ;; log entry anyway. For the same reason, don't add a closed-
      ;; string either.
      org-log-done 'nil
      org-closed-string ""

      ;; Only use "1.", "2." for ordered lists, not "1)", "2)" etc
      org-plain-list-ordered-item-terminator ?.

      ;; Don't use S-xxx as this is used for CUA mode etc
      org-replace-disputed-keys t

      ;; Assumes you have "#+STARTUP: customtime" in your *.org file
      org-time-stamp-custom-formats '("<%a %d.%m.%Y>" . "<%a %d.%m.%Y %H:%M>")
      )

;; Turn on auto-fill
(add-hook 'org-mode-hook 'auto-fill-mode)

;; Use Org mode for files ending with .org
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

;; Key-Bindings
(define-key global-map "\C-ca" 'org-agenda)



;;}}}
;;{{{ Mode: Remember

(require 'remember)

;;(org-remember-insinuate)
(setq org-default-notes-file (concat org-directory "/agenda.org")
      org-remember-store-without-prompt t
      ;; Store remember notes without prompting
      org-remember-store-without-prompt t
      org-remember-templates
      ;; SingleName character template         optionalFile optionalHeadline
      '(("TODO"     ?t       "* TODO %?\n  - State \"TODO\"       %u" nil    "Tasks")
	("NOTES"    ?n       "* %?\n  - State \"TODO\"       %u"      nil    "Notes")
	)
      remember-annotation-functions 'org-remember-annotation
      remember-handler-functions 'org-remember-handler
      )
(add-hook 'remember-mode-hook 'org-remember-apply-template)
(define-key global-map "\C-cr" 'org-remember)



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
;;{{{ Mode: Term

(eval-after-load "term"
  '(progn
     (add-hook 'term-mode-hook
	       (lambda ()
		 (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")
		 ;; (make-local-variable 'transient-mark-mode)
		 (auto-fill-mode -1)
		 (setq tab-width 8)))))



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
;;{{{ Package: bookmark

(eval-after-load "bookmark"
  '(progn
     (setq ;; Store bookmarks inside .emacs.d
           bookmark-default-file "~/.emacs.d/bookmarks.org"
	   ;; Save file after every bookmark altertation
	   bookmark-save-flag 1
	   )))



;;}}}
;;{{{ Package: calendar

(setq diary-file  "~/.emacs.d/diary"
      calendar-latitude 50.17
      calendar-longitude 8.46
      calendar-location-name "Wöllstadt"
      calendar-christian-all-holidays-flag nil
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



;;}}}
;;{{{ Package: erc

(autoload 'erc-open "erc" "IRC client." t)

(eval-after-load "erc"
  '(progn
     (require 'erc-match)
     ;; This will add an ERC item to the Tools menu
     (require 'erc-menu)
     (setq erc-server "irc.freenode.net"
	   erc-port 6667
	   ;;erc-email-userid "userid"
	   erc-keywords '("schurig")
	   ;; Freenode.net doesn't need a password
	   erc-prompt-for-password nil
	   ;; But Nickserv does
	   erc-prompt-for-nickserv-password nil
	   erc-nickserv-passwords  '((freenode (("schurig" . "dtgabzi"))))
	   ;;erc-autojoin-channels-alist '((".*freenode.net" "#emacs"))
	   erc-button-url-regexp
	   "\\([-a-zA-Z0-9_=!?#$@~`%&*+\\/:;,]+\\.\\)+[-a-zA-Z0-9_=!?#$@~`%&*+\\/:;,]*[-a-zA-Z0-9\\/]"
	   ;; Kill channel after /part
	   erc-kill-buffer-on-part t
	   ;; Kill buffers for server messages after quitting the server
	   erc-kill-server-buffer-on-quit t
	   )
     (require 'erc-services)
     (erc-services-mode 1)
     (add-hook 'erc-mode-hook
	       '(lambda ()
					;(require 'erc-pcomplete)
		  (pcomplete-erc-setup)
		  (erc-completion-mode 1)))
     ;;(load-library "erc-highlight-nicknames")
     ;;(add-to-list 'erc-modules 'highlight-nicknames)
     ;;(erc-update-modules)
     ))

(autoload 'erc-compute-server "erc" "Return an IRC server name." t)
(autoload 'erc-compute-port  "erc" "Return a port for an IRC server." t)
(autoload 'erc-compute-nick "erc" "Return user's IRC nick." t)
(autoload 'erc-compute-full-name "erc" "Return user's full name." t)


(defun irc ()
  "Calls ERC with freenode.net"
  (interactive)
  (erc-open (erc-compute-server)
	    (erc-compute-port)
	    (erc-compute-nick)
	    (erc-compute-full-name)
	    t ""))



;;}}}
;;{{{ Package: ibuffer

(require 'ibuffer)

(global-set-key "\C-x\C-b" 'ibuffer)

(setq ibuffer-default-sorting-mode 'major-mode
      ibuffer-always-show-last-buffer t
      ibuffer-view-ibuffer nil
      )



;;}}}
;;{{{ Package: ido

;; (require 'ido)

;; (ido-mode 'buffer)
;; (setq
;;  ido-save-directory-list-file  "~/.emacs.d/ido.last"
;;  ido-work-directory-list '()
;;  ido-everywhere t			; use for many file dialogs
;;  ido-case-fold t			; be case-insensitive
;;  ido-enable-last-directory-history t	; remember last used dirs
;;  ido-max-work-directory-list 30		; should be enough
;;  ido-max-work-file-list      50		; remember many
;;  ido-use-filename-at-point t		; don't use filename at point (annoying)
;;  ido-use-url-at-point nil		;  don't use url at point (annoying)
;;  ido-enable-flex-matching t		; be flexible
;;  ido-max-prospects 5			; don't spam my minibuffer
;;  ido-confirm-unique-completion t	; wait for RET, even with unique completion
;;  ido-enable-dot-prefix t		; need "." to select hidden files
;;  ido-ignore-buffers
;;  '("\\`"
;;   "^\*Mess"
;;   "^\*Help*"
;;   "^\*Back"
;;   ".*Completion"
;;   "^\*Ido")
;;  ido-ignore-directories
;;  '("\\`CVS/"
;;    "\\.svn/"
;;    "\\.git/"
;;    "\\`\\.\\./"
;;    "\\`\\./")
;; )


;; ;; ;; wget -O smex.el http://github.com/nonsequitur/smex/blob/master/smex.el?raw=true
;; ;; (if (require 'smex nil 'noerror)
;; ;;     (progn
;; ;;       (global-set-key (kbd "M-X") 'smex)
;; ;;       (setq smex-save-file "~/.emacs.d/smex.last")
;; ;;       (smex-initialize))
;; ;;   (message "No smex found :-("))


;; ;; Use ido for almost everything
;; ;; (defadvice completing-read
;; ;;   (around foo activate)
;; ;;   (if (boundp 'ido-cur-list)
;; ;;       ad-do-it
;; ;;     (setq ad-return-value
;; ;; 	  (ido-completing-read
;; ;; 	   prompt
;; ;; 	   (all-completions "" collection predicate)
;; ;; 	   nil require-match initial-input hist def))))

;; ;; use ido even for M-x
;; (global-set-key
;;  "\M-x"
;;  (lambda ()
;;    (interactive)
;;    (call-interactively
;;     (intern
;;      (ido-completing-read
;;       "M-x "
;;       (all-completions "" obarray 'commandp))))))



;;}}}
;;{{{ Package: magit

;; git clone git://gitorious.org/magit/mainline.git magit
;; http://zagadka.vm.bytemark.co.uk/magit/magit.html

(autoload 'magit-status "magit" nil t)

(eval-after-load "magit"
  '(progn
     (setq magit-save-some-buffers nil)))

(global-set-key "\M-g\M-m" 'magit-status)
(global-set-key "\M-gm" 'magit-status)



;;}}}
;;{{{ Package: minibuffer

;; C-c clears minibuffer
(define-key minibuffer-local-map "\C-c" (lambda () (interactive) (delete-minibuffer-contents)))

(setq
 ;; Don't insert current directory into minubuffer
 insert-default-directory nil
 ;; enable recursive minibuffer
 enable-recursive-minibuffers t
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
(setq savehist-file "~/.emacs.d/history"
      history-length 1000)
(savehist-mode 1)



;;}}}
;;{{{ Package: pabbrev

(autoload 'pabbrev-mode "pabbrev" nil t)
(autoload 'global-pabbrev-mode "pabbrev" nil t)

;; (require 'pabbrev)
;; (global-pabbrev-mode t)
;; (setq pabbrev-read-only-error nil)



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
;;{{{ Disabled Package: icomplete

;; (require 'icomplete)
;; (require 'icomplete+ nil 'noerror)
;; (icomplete-mode 1)
;; (setq icomplete-compute-delay 0)



;;}}}
;;{{{ Disabled Package: Icicles

;; To install icicles
;;
;;  mkdir icicles
;;  wget http://www.emacswiki.org/emacs/download/fuzzy-match.el
;;  cd icicles
;;  wget http://www.emacswiki.org/emacs/download/icicles-install.el
;;
;; (setq icicle-download-dir "~/.emacs.d/icicles")
;; (load "icicles-install")
;; (icicle-download-wizard)


;; (eval-after-load "icicles/icicles"
;;   '(progn
;;   (message "HS: init icicles")
;;   (add-to-list 'load-path "~/.emacs.d/icicles/")
;;   ))


;; (require 'icicles)
;; (require 'fuzzy-match)

;; (icicle-mode 1)



;;}}}
;;{{{ Uniquify

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets
      uniquify-separator "/"
      uniquify-after-kill-buffer-p t
      uniquify-ignore-buffers-re "^\\*")



;;}}}
;;{{{ Server

;; Automatically start server, even when run interactively
(require 'server)
(unless (server-running-p)
  (server-start))

;; Make sure the frame pops up as a graphical frame
(setq server-window '(lambda (buf)
		       (switch-to-buffer buf)
		       (raise-frame)
		       (add-hook 'server-done-hook 'delete-frame t t)))



;;}}}
;;{{{ Key bindings

(global-set-key "\C-xE"		'apply-macro-to-region-lines)
(global-set-key "\C-x\\"	'align-regexp)

(global-set-key "\C-c\C-f"	'ff-find-other-file)

(global-set-key "\C-cc"		'comment-region)
(global-set-key "\C-cu"		'uncomment-region)

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

;; Don´t bother entering search and replace args if the buffer is read-only
(defadvice query-replace-read-args (before barf-if-buffer-read-only activate)
  "Signal a `buffer-read-only' error if the current buffer is read-only."
  (barf-if-buffer-read-only))



;;}}}
;;{{{ TODO stuff

;; TODO: copyright-update?


;; TODO: desktop/session-save?

;; http://hoolihan.net/blog-tim/?p=155

;; (defvar user-temporary-file-directory
;;   (concat temporary-file-directory user-login-name "/"))
;; (make-directory user-temporary-file-directory t)
;; (setq backup-by-copying t)
;; (setq backup-directory-alist
;;       `(("." . ,user-temporary-file-directory)
;; 	(,tramp-file-name-regexp nil)))
;; (setq auto-save-list-file-prefix
;;       (concat user-temporary-file-directory ".auto-saves-"))
;; (setq auto-save-file-name-transforms
;;       `((".*" ,user-temporary-file-directory t)))



;;}}}
