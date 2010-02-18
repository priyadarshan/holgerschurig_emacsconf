;; -*- mode: emacs-lisp; mode: fold -*-


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

(dolist (i '(
	     "~/.emacs.d/elisp/"
	     "~/.emacs.d/magit/"
	     "~/.emacs.d/elpa/"
	     "~/.emacs.d/"
	     ))
  (when (not (member i load-path))
    (add-to-list 'load-path (expand-file-name i))))



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
;; ORIGINAL: deleteline



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



;;}}}
;;{{{ Functions: Yank and Delete

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

;; ORIGNAL: yank
(global-set-key "\C-y" 'my-yank)



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


(defun my-explode-window ()
  "If there is only one window displayed, act like C-x2. If there
are two windows displayed, act like C-x1:"
  (interactive)
  (if (one-window-p t)
      (progn
	(split-window-vertically)
	(other-window 1)
	(my-zoom-next-buffer)
	(other-window -1))
    (delete-other-windows)
  ))

(defun my-next-window ()
  "If there is only one window displayed, switch to the next
  buffer. Otherwise simply toggle the window."
  (interactive)
  (if (one-window-p t)
      (my-zoom-next-buffer)
    (other-window 1)))

(global-set-key [(f5)] 'my-explode-window)
;; ORIGINAL: undefined

(global-set-key [(f6)] 'my-next-window)
;; ORIGINAL: undefined

;; TODO: Shift-F6: prev window?



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
;; ORIGINAL: undefined


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
  ;; disabled for now
  (if (or (eq major-mode 'lisp-mode) (eq major-mode 'emacs-lisp-mode))
      (progn
	(ignore-errors (my--kill-buffer-and-window (get-buffer-create "*Compile-Log*")))
	; Now try to compile this file
	(my--bcc-compile-source-file (buffer-file-name))
	)
    (progn
      (my--kill-buffer-and-window (get-buffer-create "*compilation*"))
	(compile compile-command))))

(global-set-key [(f7)] 'my-compile)
;; ORIGINAL: undefined


(defun set-compile-command (&optional cmd)
  "Helper for to set compile-command"
  (interactive "scmd: ")
  (setq compile-command cmd))



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
;; ORIGINAL: undefined

(global-set-key [(shift f8)] 'previous-error)
;; ORIGINAL: undefined



;;}}}
;;{{{ Load private data

(load "~/.emacs.d/private.el" 'noerror 'nomessage)



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

;; Save recent files
(setq recentf-save-file "~/.emacs.d/tmp/recentf.el"
      recentf-exclude '("bbdb$"
			"svn-commit.tmp$"
			".git/COMMIT_EDITMSG$"
			".git/TAG_EDITMSG")
      recentf-max-saved-items 1000)
(recentf-mode 1)

;; Don't run vc-git & friends, we have magit
(defun vc-find-file-hook ()
  "Dummy, overriding the one in vc-hooks.el"
  (setq vc-mode nil))



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

(setq
 ;; Preserve hard links to the file you´re editing (this is
 ;; especially important if you edit system files)
 backup-by-copying-when-linked t
 ;; Just never create backup files at all
 ;;make-backup-files nil
 backup-directory-alist '(("." . "~/.emacs.d/tmp/bak/"))
 )

;; Emacs is a text editor, make sure your text files end in a newline
(setq require-final-newline t)

;; Disable auto-save (#init.el# file-names)
(setq auto-save-default nil)

;; Don't open Qt's *.pro files as IDLWAVE files
(add-to-list 'auto-mode-alist '("\\.pro$" . fundamental-mode))

;; Open *.h files normally in c++ mode
(add-to-list 'auto-mode-alist '("\\.h$" . c++-mode))


;; Auto-saving into some global directory

;; (defvar autosave-dir
;;   (file-name-as-directory "~/.emacs.d/tmp/autosave")
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

(setq auto-save-list-file-prefix "~/.emacs.d/tmp/auto-save-list/saves-")

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
      )
  (progn
    ;; Visial bell only in text mode
    (setq visible-bell t)
    ))

;; Let parenthesis behave
(show-paren-mode 1)
(setq show-paren-delay 0
      blink-matching-parent nil)
(set-face-background 'show-paren-match-face "#e0e0e0")


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
(setq savehist-file "~/.emacs.d/tmp/history.el"
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


(defun my-c-mode-common-setup ()
  (define-key c-mode-map (kbd "RET") 'newline)
  (turn-on-auto-fill)
  (c-toggle-auto-newline 1)
  (modify-syntax-entry ?_ "w")
  (setq fill-column 76
	;; Let RET break and continue a comment
	;; C doesn't start functions with a ( in the first column
	open-paren-in-column-0-is-defun-start nil
	;; Insert TABs if needed
	;;c-tab-always-indent nil
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
(setq srecode-map-save-file "~/.emacs.d/tmp/srecode-map"
      semanticdb-default-save-directory "~/.emacs.d/tmp/semanticdb")

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

;; use 'e' to edit filenames
(eval-after-load "dired"
  '(progn
     (define-key dired-mode-map "e" 'wdired-change-to-wdired-mode)))


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

;; SEE: http://www.emacswiki.org/emacs/org-config-thierry.el
;; http://www.newartisans.com/2007/08/using-org-mode-as-a-day-planner.html

(require 'org-install)

(setq org-directory "~/.emacs.d/org"
      org-agenda-files (list "~/.emacs.d/org/agenda.org"
			     )

      ;; Include diary entries
      org-agenda-include-diary t

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
;; ORIGINAL: undefined



;;}}}
;;{{{ Mode: Remember

(require 'remember)

;;(org-remember-insinuate)
(setq org-default-notes-file (concat org-directory "/agenda.org")
      remember-data-file (concat org-directory "/notes.org")
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
;;{{{ Package: aspell

(setq ispell-program-name "aspell"
      ispell-extra-args '("--sug-mode=ultra"))



;;}}}
;;{{{ Package: auto-install

;; See also Package: install-elist

(eval-after-load "install-elisp"
  '(setq install-elisp-repository-directory "~/.emacs.d/elisp/"))

(eval-after-load "auto-install"
  '(setq auto-install-directory "~/.emacs.d/tmp/auto-install/"))
(require 'auto-install nil 'nomsg)



;;}}}
;;{{{ Package: bookmark

(eval-after-load "bookmark"
  '(progn
     (setq ;; Store bookmarks inside .emacs.d
           bookmark-default-file "~/.emacs.d/org/bookmarks.org"
	   ;; Save file after every bookmark altertation
	   bookmark-save-flag 1
	   )))



;;}}}
;;{{{ Package: calendar

(setq diary-file  "~/.emacs.d/org/diary"
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
	   ;; Don't show any of this
	   erc-hide-list '("JOIN" "PART" "QUIT" "NICK")
	   ;; Tracking
	   erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
				     "324" "329" "332" "333" "353" "477")
	   ;; Set the prompt to the channel name
	   erc-prompt (lambda ()
			(if (and (boundp 'erc-default-recipients) (erc-default-target))
			    (erc-propertize (concat (erc-default-target) ">")
					    'read-only t 'rear-nonsticky t 'front-nonsticky t)
			  (erc-propertize "ERC>"
					  'read-only t 'rear-nonsticky t 'front-nonsticky t)))
	   )
     
     ;; Respond once if mentioned while away
     (defvar erc-responded-once nil)
     (defvar erc-away-reason nil)
     (defun erc-respond-once-if-away (match-type nickuserhost msg)
       (if (erc-away-time)
           (if (eq match-type 'current-nick)
               (unless erc-responded-once
                 (erc-send-action (erc-default-target) (concat "is away: " erc-away-reason))
                 (setq erc-responded-once t)))))
     (add-hook 'erc-text-matched-hook 'erc-respond-once-if-away)

     (require 'erc-services)
     (erc-services-mode 1)
     (erc-track-mode t)
     (add-hook 'erc-mode-hook
	       '(lambda ()
		  ;;(require 'erc-pcomplete)
		  (pcomplete-erc-setup)
		  (erc-completion-mode 1)))
     
     ;; (require 'notify)
     ;; (defun my--notify-erc (match-type nickuserhost message)
     ;;   "Notify when a message is received."
     ;;   (notify (format "%s in %s"
     ;;                   ;; Username of sender
     ;;                   (car (split-string nickuserhost "!"))
     ;;                   ;; Channel
     ;;                   (or (erc-default-target) "#unknown"))
     ;;           ;; Remove duplicate spaces
     ;;           (replace-regexp-in-string " +" " " message)
     ;;           :icon "emacs-snapshot"
     ;;           :timeout -1))
     ;; (add-hook 'erc-text-matched-hook 'my--notify-erc)
     
     ;; (require 'erc-join)
     ;; (erc-autojoin-mode 1)
     ;; (setq erc-autojoin-channels-alist
     ;;       '(("freenode.net" "#drupal-vcs" "#drupal")))
     
     ;;(load-library "erc-highlight-nicknames")
     ;;(add-to-list 'erc-modules 'highlight-nicknames)
     ;;(erc-update-modules)

     (defadvice erc-process-away (after erc-away-reason-clear (proc away-p) activate)
       "Clear things"
       (unless away-p
         (setq erc-responded-once nil
               erc-away-reason nil)))

     (defadvice erc-cmd-AWAY (after erc-store-reason (line) activate)
       "store line"
       (when (string-match "^\\s-*\\(.*\\)$" line)
         (let ((reason (match-string 1 line)))
           (setq erc-away-reason reason))))

     (add-hook 'erc-mode-hook '(lambda () (visual-line-mode 1)))
     
     ))

(autoload 'erc-compute-server "erc" "Return an IRC server name." t)
(autoload 'erc-compute-port  "erc" "Return a port for an IRC server." t)
(autoload 'erc-compute-nick "erc" "Return user's IRC nick." t)
(autoload 'erc-compute-full-name "erc" "Return user's full name." t)

;; (defun erc-generate-log-file-name-date-and-name (buffer target nick server port)
;;   "Generates a log-file name with the date and other info.
;; This results in a file name of the form \"2009-06-03-#channel@server:port.txt\".
;; This function is a possible value for `erc-generate-log-file-name-function'."
;;   (let ((file (concat
;;                (format-time-string "%Y-%m-%d")
;;                "-" target
;;                "@" server ".txt")))
;;     ;; we need a make-safe-file-name function.
;;     (convert-standard-filename file)))

;; (setq erc-generate-log-file-name-function 'erc-generate-log-file-name-date-and-name)


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

(setq ibuffer-display-summary nil
      ;;ibuffer-use-header-line t
      ;;ibuffer-default-sorting-mode 'major-mode
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
	    (ibuffer-switch-to-saved-filter-groups "default")))

;; Turn off header
(defadvice ibuffer-update-title-and-summary (after kill-2-lines)
  (save-excursion
    (set-buffer "*Ibuffer*")
    (toggle-read-only 0)
    (goto-char 1)
    (search-forward "-\n" nil t)
    (delete-region 1 (point))
    (let ((window-min-height 1))
      ;; save a little screen estate
      (shrink-window-if-larger-than-buffer))
    (toggle-read-only)))
(ad-activate 'ibuffer-update-title-and-summary)

(defun my-ibuffer ()
  "Open ibuffer with cursour pointed to most recent buffer name"
  (interactive)
  (let ((recent-buffer-name (buffer-name)))
    (ibuffer)
    (ibuffer-jump-to-buffer recent-buffer-name)))

(global-set-key "\C-x\C-b" 'my-ibuffer)
;; ORIGINAL: list-buffers



;;}}}
;;{{{ Package: magit

;; git clone git://gitorious.org/magit/mainline.git magit
;; http://zagadka.vm.bytemark.co.uk/magit/magit.html

(autoload 'magit-status "magit/magit" nil t)

(eval-after-load "magit"
  '(progn
     (setq magit-save-some-buffers nil)))

(global-set-key "\M-g\M-m" 'magit-status)
;; ORIGIN: undefined

(global-set-key "\M-gm" 'magit-status)
;; ORIGIN: undefined



;;}}}
;;{{{ Package: pabbrev

(autoload 'pabbrev-mode "pabbrev" nil t)
(autoload 'global-pabbrev-mode "pabbrev" nil t)

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

(autoload 'paredit-mode "paredit" nil t)

;; Automatically turn on in elist mode
;; (add-hook 'emacs-lisp-mode-hook 'paredit-mode)



;;}}}
;;{{{ Package: quilt

(load "quilt" 'noerror 'nomessage)



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

(setq tramp-persistency-file-name "~/.emacs.d/tmp/tramp")



;;}}}
;;{{{ Package: unbount

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
;;{{{ Disabled Package: wanderlust

;; ;; http://emacs-fu.blogspot.com/2009/06/e-mail-with-wanderlust.html
;; ;; http://www.emacswiki.org/emacs/hgw-init-wl.el


;; ;;{{{ Package: wanderlust - autoload

;; (autoload 'wl-other-frame "wl" "Wanderlust on new frame." t)
;; (autoload 'wl-draft "wl-draft" "Write draft with Wanderlust." t)


;; ;;}}}
;; ;;{{{ Package: wanderlust - SMTP

;; (setq wl-smtp-connection-type 'starttls
;;       wl-smtp-posting-port 587
;;       wl-smtp-authenticate-type "plain"
;;       wl-smtp-posting-user "holgerschurig@googlemail.com"
;;       wl-smtp-posting-server "smtp.gmail.com"
;;       wl-local-domain "gmail.com")

;; ;; You should also run elmo-passwd-alist-save, otherwise the passwords
;; ;; vanish at (wl-exit) time and won't be loaded again.
;; (eval-after-load "elmo-util"
;;   '(add-to-list 'elmo-passwd-alist '("SMTP:holgerschurig@googlemail.com/PLAIN@smtp.gmail.com" . "ZHRnYWJ6ZzA=")))


;; ;;}}}
;; ;;{{{ Package: wanderlust - IMAP

;; (setq elmo-imap4-default-server "lin03"
;;       elmo-imap4-default-user "schurig"
;;       elmo-imap4-default-authenticate-type 'cram-md5
;;       elmo-imap4-default-port '143
;;       elmo-imap4-use-modified-utf7 t)

;; (eval-after-load "elmo-util"
;;   '(add-to-list 'elmo-passwd-alist '("IMAP:schurig/cram-md5@lin03:143" . "c2NodXJpZ3B3")))
;; (eval-after-load "elmo-util"
;;   '(add-to-list 'elmo-passwd-alist '("IMAP:schurig/digest-md5@lin01:143" . "ZzdrZnAy")))


;; ;;}}}
;; ;;{{{ Package: wanderlist - POP3

;; (eval-after-load "elmo-util"
;;   '(add-to-list 'elmo-passwd-alist '("POP3:p4004p2/user@mail.mn-solutions.de:110" . "c2Vkb2thNg==")))


;; ;;}}}
;; ;;{{{ Package: wanderlust - Paths

;; (setq elmo-maildir-folder-path "~/Mail"
;;       wl-folders-file "~/Mail/.folders"
;;       elmo-msgdb-directory "~/.emacs.d/tmp/elmo-msgdb"
;;       wl-address-file "~/.emacs.d/wl-addresses"
;;       wl-alias-file "~/.emacs.d/wl-im-aliases"
;;       wl-score-files-directory "~/.emacs.d/tmp/wl-scores/"
;;       wl-temporary-file-directory "~/.emacs.d/tmp/wl/"
;;       ;; Disabble all ugly icons
;;       wl-icon-directory nil
;;       )


;; ;;}}}
;; ;;{{{ Package: wanderlust - Folder view

;; (setq wl-folder-desktop-name "Mails"
;;       wl-draft-folder ".draft"
;;       wl-trash-folder ".[Google Mail]/Papierkorb"
;;       ;; File-Carbon-
;;       wl-fcc ".[Google Mail].Gesendet"
;;       wl-fcc-force-as-read t
;;       ;; Folders with more than this number are highlighted
;;       wl-folder-many-unsync-threshold 1000
;;       ;; Ask before writing .folders
;;       wl-interactive-save-folders t
;;       ;; Don't in folder window when f
;;       wl-stay-folder-window nil
;;       wl-folder-window-width 35
;;       ;; Don't ask if we want to quit
;;       wl-interactive-exit nil
;;       )


;; ;;}}}
;; ;;{{{ Package: wanderlust - Summary view

;; (setq wl-auto-select-next 'unread
;;       ;; Threading
;;       wl-summary-default-view 'thread
;;       wl-thread-insert-opened t
;;       wl-summary-line-format "%T%P %D.%M,%h:%m %t%4(%c%)%20(%f%) %s"
;;       ;; Shorten the mode-line
;;       wl-summary-mode-line-format "WL: %f (%n/%u)"
;;       ;; Don't truncate summary window
;;       wl-summary-width nil
;;       ;; no special mouse decoration
;;       wl-use-highlight-mouse-line nil
;;       ;; In those folders we want to display the receiver, not the sender
;;       wl-summary-showto-folder-regexp ".[Google Mail].Gesendet"
;;       ;; no ugly bold graphics for the thread display
;;       wl-thread-child-str                "+"
;;       wl-thread-have-younger-brother-str "+"
;;       wl-thread-horizontal-str           "-"
;;       wl-thread-space-str                " "
;;       wl-thread-vertical-str             "|"
;;       wl-thread-youngest-child-str       "+"
;;       wl-folder-thread-indent-set-alist '((".*" . (2 "+" "+" "|" "-" " ")))
;;       )

;; ;; Functions for private key bindings
;; (defun my-wl-summary-enter-handler (&optional arg)
;;   "Enter the message after pressing ENTER in the summary view."
;;   (interactive)
;;   (wl-summary-enter-handler arg)
;;   (other-window 1))

;; ;; Key bindings

;; ;; ENTER         * my-wl-summary-enter-handler
;; ;; SPACE         * my-wl-summary-enter-handler
;; ;; q               wl-summary-exit

;; ;; C-w           * wl-summary-save-current-message
;; ;; C-y		wl-summary-yank-saved-message

;; ;; C-o		wl-summary-auto-refile
;; ;; C-t		wl-plugged-change

;; ;; Ideas:

;; ;; m r             mark read
;; ;; m u             mark unread


;; (eval-after-load "wl-summary"
;;   '(progn
;;      ;; (define-key wl-summary-mode-map "\r"   'my-wl-summary-enter-handler)
;;      ;; (define-key wl-summary-mode-map "\C-m" 'my-wl-summary-enter-handler)
;;      ;; ORIGINAL: wl-summary-read
;;      (define-key wl-summary-mode-map [(space)] 'my-wl-summary-enter-handler)
;;      (define-key wl-summary-mode-map [(return)] 'my-wl-summary-enter-handler)
;;      ;; C-w: save, C-y yank
;;      (define-key wl-summary-mode-map "\C-w" 'wl-summary-save-saved-message)

;;      ;; Normal marks
;;      (define-key wl-summary-mode-map "mu" 'wl-summary-mark-as-unread)
;;      (define-key wl-summary-mode-map "mi" 'wl-summary-mark-as-important)
;;      (define-key wl-summary-mode-map "md" 'wl-summary-delete)
;;      (define-key wl-summary-mode-map "mr" 'wl-summary-mark-as-read)
;;      (define-key wl-summary-mode-map "mx" 'wl-summary-unmark)
;;      ;; Thread marks
;;      (define-key wl-summary-mode-map "tu" 'wl-thread-mark-as-unread)
;;      (define-key wl-summary-mode-map "ti" 'wl-thread-mark-as-important)
;;      (define-key wl-summary-mode-map "td" 'wl-thread-delete)
;;      (define-key wl-summary-mode-map "tr" 'wl-thread-mark-as-read)
;;      (define-key wl-summary-mode-map "tx" 'wl-thread-unmark)
;;      ))



;; ;;}}}
;; ;;{{{ Package: wanderlust - Message view

;; (setq ;; Only display some message fields
;;       wl-message-ignored-field-list
;;       '("^.*:")
;;       wl-message-visible-field-list
;;       '("^\\(To\\|Cc\\):"
;; 	"^Subject:"
;; 	"^\\(From\\|Reply-To\\):"
;; 	;;"^Organization:"
;; 	"^\\(Posted\\|Date\\):"
;; 	)
;;       ;; Display header fields in this order:
;;       wl-message-sort-field-list
;;       '("^Subject"
;; 	"^From"
;; 	"^Date"
;; 	"^To"
;; 	"^Cc")
;;       ;; Disable inline display of HTML part.
;;       ;; Put before load `mime-setup'
;;       mime-setup-enable-inline-html nil
;;       ;; Don't split large message.
;;       mime-edit-split-message nil
;;       ;; If lines of message are larger than this value, treat it as `large'.
;;       mime-edit-message-default-max-lines 1000
;;  )

;; (defun my-wl-summary-next ()
;;   (interactive)
;;   (other-window -1)
;;   (wl-summary-next)
;;   (other-window 1))
;; (defun my-wl-summary-prev ()
;;   "Should be called while in the message window"
;;   (interactive)
;;   (other-window -1)
;;   (wl-summary-prev)
;;   (other-window 1))
;; (defun my-wl-summary-up ()
;;   (interactive)
;;   (other-window -1)
;;   (wl-summary-up)
;;   (other-window 1))
;; (defun my-wl-summary-down ()
;;   "Should be called while in the message window"
;;   (interactive)
;;   (other-window -1)
;;   (wl-summary-down)
;;   (other-window 1))

;; (defun my-mime-view-keybindings ()
;;   ;; As TAB moves forward, S-TAB should move backwards
;;   (local-set-key [(backtab)] 'mime-preview-move-to-previous)
;;   ;; this will also delete the buffer
;;   (local-set-key "q" 'kill-buffer-and-window)
;;   ;; n moves to next NEW message, but p moves to previos message
;;   (local-set-key "n" 'my-wl-summary-down)
;;   (local-set-key "p" 'my-wl-summary-prev)
;;   ;; next message, but P moves to previos NEW messages, which is
;;   ;; exists quite seldom
;;   (local-set-key "N" 'my-wl-summary-next)
;;   (local-set-key "P" 'my-wl-summary-up)
;;   (local-set-key [(space)] 'my-wl-summary-down)
;;   )
;; (add-hook 'mime-view-mode-hook 'my-mime-view-keybindings)

;; ;;}}}
;; ;;{{{ Package: wanderlust - Beginnings of PDF handling
;; ;; (eval-after-load "mime-view"
;; ;;   '(progn
;; ;;      (ctree-set-calist-strictly
;; ;;       'mime-acting-condition
;; ;;       '((mode . "play")
;; ;;         (type . application)(subtype . pdf)
;; ;;         (method . my-mime-save-content-find-file)))))
;; ;;}}}
;; ;;{{{ Package: wanderlust - Message composing

;; (setq wl-forward-subject-prefix "Fwd: "
;;       ;; We don't want this overlong user-agent
;;       mime-edit-insert-user-agent-field nil
;;       ;; http://www.gohome.org/wl/doc/wl_91.html
;;       wl-draft-config-alist
;;       '(;; Private messages -> private signature
;; 	("^From:.*gmail.com"
;; 	 ;;(bottom . "\nBye.\n") ;; inserted at the bottom of the body
;; 	 (bottom-file . "~/.emacs.d/wl/private.sig"))
;;         )
;;  )

;; (defun my-kill-user-agent ()
;;   (let ((loc (point)))
;;     (mail-position-on-field "User-Agent")
;;     (delete-region
;;      (save-excursion (move-beginning-of-line 1) (point))
;;      (save-excursion (move-end-of-line 1) (point)))
;;     (delete-char 1)
;;     (goto-char loc))
;;     )
;; (add-hook 'wl-mail-setup-hook 'my-kill-user-agent)
;; (add-hook 'wl-draft-reply-hook 'my-kill-user-agent)
;; (add-hook 'wl-draft-forward-hook 'my-kill-user-agent)


;; (defun my-clean-mime-reply ()
;;   "Clean-up the citation in replies, removing unnecessary entities."
;;   (interactive)
;;   ;; Find and strip the first tag, indicating the start of the
;;   ;; cited message
;;   (when (re-search-forward "^> \\[1" nil t)
;;     (beginning-of-line)
;;     (delete-lines 1)
;;     (while (or (looking-at "^> *$")
;;                (looking-at "^> \\[[1-9]"))
;;       (delete-lines 1))
;;     (when (re-search-forward "^> \\[[1-9][\\. ]" nil t)
;;       (beginning-of-line)
;;       (let ((pt (point)))
;;         (re-search-forward "^$")
;;         (delete-region pt (point)))))
;;   ;; Now find the tag that ends the first section, and strip off
;;   ;; everything from there to the end of the message (including any
;;   ;; other sections that got cited)
;;   (goto-char (point-max))
;;   (when (re-search-backward "^> +[^ ]" nil t)
;;     (beginning-of-line)
;;     (let ((pt (point)))
;;       (goto-char (point-max))
;;       (if (re-search-backward "^> *$" pt t)
;;           (progn
;;             (beginning-of-line)
;;             (while (looking-at "^> *$")
;;               (delete-lines 1)
;;               (forward-line -1))
;;             (forward-line 1)
;;             (delete-lines 1))
;;         (goto-char (point-max))
;;         (re-search-backward "^$")
;;         (delete-lines 1)))))

;; ;; Modify mail buffer at mail creation time, not at send time
;; (remove-hook 'wl-draft-send-hook 'wl-draft-config-exec)

;; (defun my-mail-setup ()
;;   "Set up appropriate modes for writing Email and clean-up
;; citation for replies."
;;   (interactive)
;;   ;; Fold over-lenght lines
;;   (turn-on-auto-fill)
;;   ;; Now call our hooks. For mailing lists with patches, this might
;;   ;; turn off auto-fill again.
;;   (wl-draft-config-exec)

;;   ;; TODO Clean up reply citation
;;   (save-excursion
;;     ;; Goto the beginning of the message body
;;     (mail-text)
;;     ;; If the message body starts with "Dear " assume it is a reply
;;     ;; and clean the citation
;;     (when (looking-at "^Dear ")
;;       (my-clean-mime-reply))))
;; (add-hook 'wl-mail-setup-hook 'my-mail-setup)


;; ;;}}}
;; ;;{{{ Package: wanderlust - E-Mail address database

;; ;; http://emacs-fu.blogspot.com/2009/08/managing-e-mail-addresses-with-bbdb.html

;; (eval-after-load "wl"
;;   '(progn
;;      ;; this also does "(require 'bbdb)"
;;      (load "/usr/share/emacs/site-lisp/wl/utils/bbdb-wl.el" 'noerror 'nomsg)))

;; (eval-after-load "bbdb-wl"
;;   '(progn
;;      (bbdb-wl-setup)
;;      (define-key wl-draft-mode-map (kbd "<C-tab>") 'bbdb-complete-name)))

;; (eval-after-load "bbdb"
;;   '(progn
;;      (setq bbdb-file "~/.emacs.d/tmp/contacts"
;; 	   ;; allow contacts file to be edited outside emacs
;; 	   bbdb-auto-revert-p t
;; 	   ;; 1 means save-without-asking
;;            bbdb-offer-save 1

;; 	   ;; Don't always show the BBDB window
;; 	   bbdb-use-pop-up nil
;; 	   ;; be disposable with SPC
;; 	   bbdb-electric-p t
;; 	   ;; allow cycling if user has several mail addresses
;; 	   bbdb-complete-name-allow-cycling t

;; 	   ;; De-americanize
;; 	   bbdb-north-american-phone-numbers-p nil
;; 	   bbdb-check-zip-codes-p nil

;; 	   ;; auto-create addresses from mail
;; 	   bbdb/mail-auto-create-p 'bbdb-ignore-some-messages-hook
;; 	   bbdb-always-add-addresses t

;; 	   ;; Don't ask about fake addresses. NOTE: there can be only one
;; 	   ;; entry per header (such as To, From)
;; 	   ;; http://flex.ee.uec.ac.jp/texi/bbdb/bbdb_11.html
;; 	   bbdb-ignore-some-messages-alist
;; 	   '(( "From" . "no.?reply\\|DAEMON\\|daemon\\|facebookmail\\|twitter"))
;; 	   )
;;      (bbdb-initialize)))


;; ;;}}}
;; ;;{{{ Package: wanderlust - Misc customization

;; (setq wl-from "Holger Schurig <holgerschurig@gmail.com>"
;;       ;;User's mail addresses
;;       wl-user-mail-address-list '("h.schurig@mn-solutions.de"
;; 				  "holgerschurig@gmail.com"
;; 				  "holgerschurig@gmail.com"
;; 				  "hs4233@mail.mn-solutions.de"
;; 				  )
;;       ;; Subscribed mailing list.
;;       ;; wl-subscribed-mailing-list '("linux-wireless@vger.kernel.org"
;;       ;; 				   "hostap@lists.shmoo.com"
;;       ;; 				   "ath5k-devel@lists.ath5k.org"
;;       ;; 				   "madwifi-devel@lists.sourceforge.net"
;;       ;; 				   "linux-arm-kernel@lists.arm.linux.org.uk"
;;       ;; 				   "haret@handhelds.org"
;;       ;; 				   "urjtag-development@lists.sourceforge.net"
;;       ;; 				   "MEMBERS@LIST.CELINUXFORUM.ORG"
;;       ;; 				   "cfe-dev@cs.uiuc.edu"
;;       ;; 				   )
;;       ;; No demo at startup
;;       wl-demo nil
;;       )

;; ;; Automatically save entered passwords before clearing them
;; (add-hook 'wl-exit-hook 'elmo-passwd-alist-save)


;; ;;}}}
;; ;;{{{ Package: wanderlust - Default compose-mail

;; (autoload 'wl-user-agent-compose "wl-draft" nil t)
;; (if (boundp 'mail-user-agent)
;;     (setq mail-user-agent 'wl-user-agent))
;; (if (fboundp 'define-mail-user-agent)
;;     (define-mail-user-agent
;;       'wl-user-agent
;;       'wl-user-agent-compose
;;       'wl-draft-send
;;       'wl-draft-kill
;;       'mail-send-hook))


;; ;;}}}
;; ;;{{{ Package: wanderlust - (Disabled) Mail checking

;; ;; (setq wl-biff-check-folder-list
;; ;;       '("&xxxxxxx+h.xxxxxx/user@mail.plus.net:110!direct"
;; ;;         "&xxxxxxx+enquiries/user@mail.plus.net:110!direct"
;; ;;         "%inbox:hxxxxxx0/clear@imap.gmail.com:993!"
;; ;;         "-gmane.emacs.cvs@news.gmane.org"
;; ;;         "-gmane.emacs.devel@news.gmane.org"
;; ;;         "-gmane.emacs.orgmode@news.gmane.org"
;; ;;         "-gmane.emacs.emms.user@news.gmane.org"
;; ;;         "-gmane.emacs.sources@news.gmane.org"
;; ;;         "-gmane.mail.wanderlust.general@news.gmane.org"
;; ;;         "-gmane.mail.wanderlust.general.japanese@news.gmane.org"
;; ;;         "-gmane.comp.window-managers.stumpwm.devel@news.gmane.org"
;; ;;         "-gmane.comp.mozilla.conkeror@news.gmane.org"
;; ;;         )
;; ;;       wl-biff-check-interval 180
;; ;;       wl-biff-use-idle-timer t)


;; ;;}}}
;;}}}
;;{{{ Disabled Package: bs

;; (require 'bs)

;; (global-set-key "\C-xb" 'bs-show)
;; ;; ORIGINAL: switch-to-buffer

;; (defun list-buffers-other-win ()
;;   "Opens list-buffers and put focus on it"
;;   (interactive)
;;   (bs-show "all"))

;; (global-set-key "\C-x\C-b" 'list-buffers-other-win)
;; ORIGINAL: list-buffers



;;}}}
;;{{{ Disabled Package: ido

;; (require 'ido)

(eval-after-load "ido"
  '(setq ido-save-directory-list-file "~/.emacs.d/tmp/ido.last"
	 ido-work-directory-list '()
	 ido-everywhere t			; use for many file dialogs
	 ido-case-fold t			; be case-insensitive
	 ido-enable-last-directory-history t	; remember last used dirs
	 ido-max-work-directory-list 30		; should be enough
	 ido-max-work-file-list      50		; remember many
	 ido-use-filename-at-point t		; don't use filename at point (annoying)
	 ido-use-url-at-point nil		;  don't use url at point (annoying)
	 ido-enable-flex-matching t		; be flexible
	 ido-max-prospects 5			; don't spam my minibuffer
	 ido-confirm-unique-completion t	; wait for RET, even with unique completion
	 ido-enable-dot-prefix t		; need "." to select hidden files
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

;; (ido-mode 'buffer)



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
;;{{{ Disabled package: smart-tab

;; This positions the cursor at weird positions when writin

;; (load "smart-tab/smart-tab" 'noerror 'nomessage)
;; (eval-after-load "smart-tab"
;;   '(global-smart-tab-mode 1))



;;}}}
;;{{{ Key bindings

(global-set-key "\C-xE"		'apply-macro-to-region-lines)
;; ORIGIN: kmacro-and-and-call-macro (on \C-Xe)

(global-set-key "\C-x\\"	'align-regexp)
;; ORIGIN: undefined

(global-set-key "\C-c\C-f"	'ff-find-other-file)
;; ORIGIN: undefined

(global-set-key "\C-cc"		'comment-region)
;; ORIGIN: undefined

(global-set-key "\C-cu"		'uncomment-region)
;; ORIGIN: undefined


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
