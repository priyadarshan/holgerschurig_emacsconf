;;; * Load paths
(defvar emacs-d (file-name-directory
		 (file-chase-links load-file-name))
  "My emacs dotfiles directory, ~/.emacs.d on Linux")

(setq package-user-dir
      (expand-file-name "elpa" emacs-d))
(package-initialize)

(let ((emacs-git (expand-file-name "git/" emacs-d)))
  (mapc (lambda (x)
          (add-to-list 'load-path (expand-file-name x emacs-git)))
        (delete ".." (directory-files emacs-git))))

;; When using org-mode from GIT:
;;(add-to-list 'load-path (expand-file-name "git/org-mode/lisp/" emacs-d))

(add-to-list 'load-path (expand-file-name "elisp/" emacs-d))


;;; * Customize

;; http://lists.gnu.org/archive/html/emacs-devel/2015-04/msg01261.html
(defmacro csetq (variable value)
  `(funcall (or (get ',variable 'custom-set) 'set-default) ',variable ,value))


;;** decorations
(csetq tool-bar-mode nil)
(csetq menu-bar-mode nil)
(csetq scroll-bar-mode nil)
(csetq inhibit-startup-screen t)
(csetq initial-scratch-message "")


;;: * Debugging
(csetq message-log-max 10000)





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; * package and use-package
;;
;;  See http://github.com/jwiegley/use-package/
;;   or http://www.lunaryorn.com/2015/01/06/my-emacs-configuration-with-use-package.html
;;
;; (use-package package-name
;;      [:keyword [option]]...)
;;
;; :init          Code to run before PACKAGE-NAME has been loaded.
;; :config        Code to run after PACKAGE-NAME has been loaded.  Note that if
;;                loading is deferred for any reason, this code does not execute
;;                until the lazy load has occurred.
;; :preface       Code to be run before everything except `:disabled'; this can
;;                be used to define functions for use in `:if', or that should be
;;                seen by the byte-compiler.
;;
;; :mode          Form to be added to `auto-mode-alist'.
;; :interpreter   Form to be added to `interpreter-mode-alist'.
;;
;; :commands      Define autoloads for commands that will be defined by the
;;                package.  This is useful if the package is being lazily loaded,
;;                and you wish to conditionally call functions in your `:init'
;;                block that are defined in the package.
;;
;; :bind          Bind keys, and define autoloads for the bound commands.
;; :bind*         Bind keys, and define autoloads for the bound commands,
;;                *overriding all minor mode bindings*.
;; :bind-keymap   Bind a key prefix to an auto-loaded keymap defined in the
;;                package.  This is like `:bind', but for keymaps.
;; :bind-keymap*  Like `:bind-keymap', but overrides all minor mode bindings
;; :defer         Defer loading of a package -- this is implied when using
;;                `:commands', `:bind', `:bind*', `:mode' or `:interpreter'.
;;                This can be an integer, to force loading after N seconds of
;;                idle time, if the package has not already been loaded.
;; :demand        Prevent deferred loading in all cases.
;;
;; :if EXPR       Initialize and load only if EXPR evaluates to a non-nil value.
;; :disabled      The package is ignored completely if this keyword is present.
;; :defines       Declare certain variables to silence the byte-compiler.
;; :functions     Declare certain functions to silence the byte-compiler.
;; :load-path     Add to the `load-path' before attempting to load the package.
;; :diminish      Support for diminish.el (if installed).
;; :ensure        Loads the package using package.el if necessary.
;; :pin           Pin the package to an archive.
;;
;; Please don't load outdated byte code
(setq load-prefer-newer t)

;; ELPA might use Emacs-W3 to get files, and this in turn sets cookies.
;; Move the cookie file out into the =tmp/= directory.
(setq url-configuration-directory (concat emacs-d "tmp/"))

(require 'package)
(setq package-enable-at-startup nil)

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))

; When using melpa-stable instead of melpa, magit won't run:
;(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

;; Automatically install `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; * Entering/exiting Emacs

(setq gc-cons-threshold 10000000

      ;; Do without annoying startup msg.
      inhibit-startup-message t

      ;; This inhibits the initial startup echo area message.
      inhibit-startup-echo-area-message "schurig"

      ;; Don't ask when running revert-buffer
      revert-without-query (quote (""))

      ;; Empty scratch message
      initial-scratch-message nil

      ;; Include current buffer name in the title bar
      frame-title-format '(buffer-file-name "%f" ("%b"))

      ;; Set up default editing mode.
      major-mode 'indented-text-mode

      ;; Custom file, part one
      custom-file (concat emacs-d "custom.el")

      ;; Delete previous identical history entries
      history-delete-duplicates t
      )


;;; ** Load customization file

(if (file-exists-p custom-file) (load-file custom-file))


;;; ** Simpler yes or no prompt

;;  Get rid of yes-or-no questions - y or n is enough
(fset 'yes-or-no-p 'y-or-n-p)





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; * Private data

(setq user-full-name "Holger Schurig"
      user-mail-address "holgerschurig@gmail.com")
(load (concat emacs-d "private.el") 'noerror 'nomessage)






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; * Display

;;; ** Fixed frame display

(if (equal window-system 'w32)
    ;; Windows settings
    (setq initial-frame-alist
	  '(
	    ;;(background-color . "black")
	    ;;(foreground-color . "LightGray")
	    (horizontal-scroll-bars . nil)
	    (vertical-scroll-bars . right)
	    (tool-bar-lines . 0)
	    (left-fringe . 1)
	    (right-fringe . 0)))
  ;; Linux settings
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

;;; ** Set default frame attributes
;;  default-frame-alist is defined in terms of initial-frame-alist.  Don't
;;  use copy-sequence here -- it doesn't copy the list elements, just the
;;  list's cons cells.  Use copy-alist instead.

(setq default-frame-alist (copy-alist initial-frame-alist))

;;; ** Don't wait for window manager when font changes
;;  Avoid Emacs hanging for a while changing default font
;;
(modify-frame-parameters nil '((wait-for-wm . nil)))

;;; ** Theme
(use-package afternoon-theme
  :config
  (load-theme 'afternoon t)
)

;;; ** Font-Lock some keywords
(defface my--todo-face
  '((t :foreground "red"
       :weight bold))
  "Font for showing TODO words."
  :group 'basic-faces)

;; Highlight each of TODO FIXME XXX DISABLED \todo
(defun my--hint-facify ()
   (unless (or (eq 'diff-mode major-mode) (eq 'script-mode major-mode))
     (font-lock-add-keywords nil '(
	 ("\\(\\<\\(\\(FIXME\\|TODO\\|XXX\\|DISABLED\\|\\:disabled\\):?\\>\\)\\|\\\\todo\\)" 1 'my--todo-face t)
	 ))))

(add-hook 'font-lock-mode-hook 'my--hint-facify)

;;
;;; ** Line truncation
(setq ;; don't display continuation lines
      truncate-lines t
      ;; respect truncate-lines:
      truncate-partial-width-windows nil)

;;; ** Show trailing whitespace
;;  This makes typing errors more obvious.
;;
(add-hook 'find-file-hook
	  '(lambda ()
	     ;; This hack allows the diff-mode hook to set the
	     ;; variable to -1. The find-file-hook fires after the
	     ;; diff-mode hook, so we get the -1 and are able to turn
	     ;; off the display of trailing whitespaces.
	     (if (eq show-trailing-whitespace -1)
		 (setq show-trailing-whitespace nil)
	       (setq show-trailing-whitespace t))))

;;
;;; ** Handle some buffers specials (no toolbar, extra frame)
;;  Display various non-editing buffers in their own frames and show those
;;  special buffer frames without a tool bar
;;
(add-to-list 'special-display-buffer-names "*Backtrace*")
(add-to-list 'special-display-frame-alist '(tool-bar-lines . 0))

;;; ** Misc settings for text vs. windowing systems
(if window-system
    ;; X11, Windows, etc
    (progn
      ;; Windowing systems are fast enought
      (column-number-mode t)
      ;; Turn off blinking
      (blink-cursor-mode -1)
      )
  ;; Text mode
  (progn
    ;; No "very" visible cursor
    (setq visible-cursor nil)
    ))

;;; ** No audible bell
;;  No audible bell, beeps are annoying
(setq visible-bell t)

;;
;;; ** Let emacs react faster to keystrokes
(setq echo-keystrokes 0.1
      idle-update-delay 0.35)

;;; ** Menu
;;
;;  Get rid of the Games in the Tools menu.
;;
;; (define-key menu-bar-tools-menu [games] nil)

;;; ** Powerline (modeline setup)
;;
;;  http://emacs.stackexchange.com/questions/281/how-do-i-get-a-fancier-mode-line-that-uses-solid-colors-and-triangles
;;
;;  We could either use http://www.emacswiki.org/emacs/PowerLine
;;
(use-package powerline
  :init
  (powerline-default-theme)
  ;; I used to use, but because of some "left" string this didn't work anymore
  ;; (powerline-center-theme)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; * Cursor
;;; ** Nicer scroll handling

;;; ** multiple key binding
;;  First we define code that allows us to bind multiple functions to
;;  repeated commands. Taken from
;;  [[http://www.emacswiki.org/cgi-bin/wiki/DoubleKeyBinding]]:
;;
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

;;
;;; ** Home / End
;;  Based on this, we define new home/end functions:
;;
;;  (defvar my--previous-position 0)
;;
;;  (defun my-home ()
;;    "Depending on how many times it was called moves the point to:
;;
;;  - begin of indentation
;;  - beginning of line
;;  - begin of function
;;  - beginning of buffer
;;  - back to where it was"
;;    (interactive)
;;    (seq-times-do nil (setq my--previous-position (point))
;;      (back-to-indentation)
;;      (beginning-of-line)
;;      (beginning-of-defun)
;;      (goto-char (point-min))
;;      (goto-char my--previous-position)))
;;  (bind-key "C-a" 'my-home)
;;  (bind-key "<home>" 'my-home)
;;
;;  And the same for end:
;;
(defun my-end ()
  "Depending on how many times it was called moves the point to:

- end of line
- end of function
- end of buffer
- back to where it was"
  (interactive)
  (seq-times-do nil (setq my--previous-position (point))
    (end-of-line)
    (forward-paragraph)
    (end-of-defun)
    (goto-char (point-max))
    (goto-char my--previous-position)))
(bind-key "C-e" 'my-end)
(bind-key "<end>" 'my-end)

;;
;;; ** Recenter
;;  This is built-in into Emacs 23, but doesn't work as nice, e.g. the
;;  bottom position is almost identical to the middle position.
;;
(defun my-recenter ()
  "Depending on how many times it was called moves the point to:

- center of screen
- near start of screen
- near end of center
- back to where it was"
  (interactive)
  (let ((i 0) (old (window-start)))
    (while (and (<= (setq i (1+ i)) 6) (equal (window-start) old))
      (seq-times-do nil (setq my--previous-position (window-start))
	(recenter)
	(recenter 4)
	(recenter -1)
	(set-window-start (selected-window) my--previous-position)))))
(bind-key "C-l" 'my-recenter)

;;
;;; ** Nicer goto-line
;;
;;  Doesn't modify minibuffer-history, but use it's own little history
;;  list.
;;
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

(bind-key "M-g g"   'my-goto-line)
(bind-key "M-g M-g" 'my-goto-line)

;;; ** Package: expand-region
;;
;;  Home page: https://github.com/magnars/expand-region.el
;;
;;  C-+ Expand region increases the selected region by semantic units.
;;
;;  You can then either continue to press C-+ to expand even further, or
;;  use + and - after the first expand to expand further / shrink again.
;;
(use-package expand-region
  :defer t
  :bind ("C-+" . er/expand-region)
  )





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; * Yank and Delete
;;; ** Delete word or yank
;;  The following may be of interest to people who (a) are happy with
;;  "C-w" and friends for killing and yanking, (b) use
;;  "transient-mark-mode", (c) also like the traditional Unix tty
;;  behaviour that "C-w" deletes a word backwards. It tweaks "C-w" so
;;  that, if the mark is inactive, it deletes a word backwards instead of
;;  killing the region. Without that tweak, the C-w would create an error
;;  text without an active region.
;;
;;  http://www.emacswiki.org/emacs/DefaultKillingAndYanking#toc2
;;
(defadvice kill-region (before unix-werase activate compile)
  "When called interactively with no active region, delete a single word
    backwards instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (save-excursion (backward-word 1) (point)) (point)))))

;;; ** Selection deletion
;;  Use delete-selection mode:
;;
(delete-selection-mode t)

;;
;;; ** Deletion in readonly buffer
;;  Be silent when killing text from read only buffer:
;;
(setq kill-read-only-ok t)

;;
;;; ** Join lines at killing
;;  If at end of line, join with following; otherwise kill line.
;;  Deletes whitespace at join.
;;
(defun kill-and-join-forward (&optional arg)
  "If at end of line, join with following; otherwise kill line.
Deletes whitespace at join."
  (interactive "P")
  (if (and (eolp) (not (bolp)))
      (delete-indentation t)
    (kill-line arg)))

(bind-key "C-k" 'kill-and-join-forward)


;;
;;; ** Dynamic char deletion
;;
;;  The following is from Boojum's post in
;;  [[http://www.reddit.com/r/emacs/comments/b1r8a/remacs_tell_us_about_the_obscure_but_useful/]].
;;
;;  I don't want to kill the comment, just the prefix to it. So that
;;
;;  // The quick brown fox[]
;;  // jumps over the lazy dog.
;;
;;  becomes
;;
;;  // The quick brown fox[] jumps over the lazy dog.
;;
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
(bind-key "<deletechar>" 'delete-char-dynamic)
(bind-key "<delete>" 'delete-char-dynamic)
(bind-key "C-d" 'delete-char-dynamic)





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; * Completion
;;
(setq ;; ignore case when reading a file name completion
      read-file-name-completion-ignore-case t
      ;; do not consider case significant in completion (GNU Emacs default)
      completion-ignore-case t
      ;; lets TAB do completion as well
      tab-always-indent 'complete
      completions-format 'vertical)





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; * Windows handling
;;; ** delete-window
;;  If only one window in frame, `delete-frame'.
;;
;;  From http://www.emacswiki.org/emacs/frame-cmds.el
;;
(defadvice delete-window (around delete-window (&optional window) activate)
  (interactive)
  (save-current-buffer
    (setq window (or window (selected-window)))
    (select-window window)
    (if (one-window-p t)
	(delete-frame)
      ad-do-it (selected-window))))

;;; ** new kill-buffer-and-window
;;  Replacement for interactive `kill-buffer'. We cannot redefine
;;  `kill-buffer', because other elisp code relies on it's exact
;;  behavior.
;;
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

(bind-key "C-x k" 'my--kill-buffer-and-window)

;;
;;; ** Window sizing
(bind-key "<M-down>" 'enlarge-window)

(bind-key "<M-up>" 'shrink-window)


;;
;;; ** Window zooming (F5)
;;
;;  If there is only one window displayed, act like C-x 2. If there are
;;  two windows displayed, act like C-x 1
;;
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

(bind-key "<f5>" 'my-explode-window)






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; * Buffers
;;; ** Insert buffer
;;
;;  |-------+---------------|
;;  | C-x i | insert file   |
;;  |-------+---------------|
;;  | C-x I | insert buffer |
;;  |-------+---------------|
;;
;; Insert buffer at current position
(bind-key "C-x I" 'insert-buffer)

;;; ** Protect buffers
;;
;;  https://raw.githubusercontent.com/lewang/le_emacs_libs/master/keep-buffers.el
;;
;;  By default, "*scratch*" is protected and erased when killed, "*Messages*"
;;  is never killed or erased.  You can customize easily using elisp:
;;
;;  ;; protect all buffers starting with "*scratch"
;;  (push '("\\`*scratch" . erase) keep-buffers-protected-list)
;;
(eval-when-compile
  (require 'cl))

(define-minor-mode keep-buffers-mode
  "when active, killing protected buffers results in burying them instead.
Some may also be erased, which is undo-able."
  :init-value nil
  :global t
  :group 'keep-buffers
  :lighter ""
  :version "1.4"
  (if keep-buffers-mode
      ;; Setup the hook
      (add-hook 'kill-buffer-query-functions 'keep-buffers-query)
    (remove-hook 'kill-buffer-query-functions 'keep-buffers-query)))

(defcustom keep-buffers-protected-alist
  '(("\\`\\*scratch\\*\\'" . erase)
    ("\\`\\*Messages\\*\\'" . nil))
  "an alist '((\"regex1\" . 'erase) (\"regex2\" . nil))

CAR of each cons cell is the buffer matching regexp.  If CDR is
not nil then the matching buffer is erased then buried.

If the CDR is nil, then the buffer is only buried."
  :type '(alist)
  :group 'keep-buffers
  )

(defun keep-buffers-query ()
  "The query function that disable deletion of buffers we protect."
  (let ((crit (dolist (crit keep-buffers-protected-alist)
                (when (string-match (car crit) (buffer-name))
                  (return crit)))))
    (if crit
        (progn
          (when (cdr crit)
            (erase-buffer))
          (bury-buffer)
          nil)
      t)))

(keep-buffers-mode 1)

;;; ** Easier kill buffers with processes
;;  Don't asks you if you want to kill a buffer with a live process
;;  attached to it:
;;
;;  http://www.masteringemacs.org/articles/2010/11/14/disabling-prompts-emacs/
;;
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
	     kill-buffer-query-functions))

;;; ** Package: cycbuf
(use-package cycbuf
  :defer t
  :bind (("<f6>"   . cycbuf-switch-to-next-buffer)
	 ("S-<f6>" . cycbuf-switch-to-previous-buffer))
  :init
  (progn
    (setq cycbuf-dont-show-regexp
        '("^ "
	  "^\\*"
	  ))
    (setq ;; sort by recency
          cycbuf-buffer-sort-function 'cycbuf-sort-by-recency
	  ;; Format of header
	  cycbuf-attributes-list
	  '(("M"          2                      left  cycbuf-get-modified-string)
	    ("Buffer"     cycbuf-get-name-length left  cycbuf-get-name)
	    (""           2                      left  " ")
	    ("Directory"  cycbuf-get-file-length left  cycbuf-get-file-name)
	    (""           2                      left  "  ")
	    ("Mode"      12                      left  cycbuf-get-mode-name)
	    )))
)





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; * File opening/saving
;;; ** Basic settings
(setq ;; Never show GTK file open dialog
      use-file-dialog nil
      ;; don't add newlines to end of buffer when scrolling, but show them
      next-line-add-newlines nil
      ;; Preserve hard links to the file you´re editing (this is
      ;; especially important if you edit system files)
      backup-by-copying-when-linked t
      ;; Just never create backup files at all
      ;;make-backup-files nil
      backup-directory-alist (list (cons "." (concat emacs-d "tmp/bak/")))
      ;; Make sure your text files end in a newline
      require-final-newline t
      ;; Disable auto-save (#init.el# file-names)
      auto-save-default nil)

(setq auto-save-list-file-prefix (concat emacs-d "tmp/auto-save-list/saves-"))

;;
;;; ** Automatically load .Xresources after changes
;;
;;  Sample ~/.Xresources:
;;
;;  Emacs.geometry: 120x55
;;  Emacs.Font:	terminus 11
;;  Emacs.verticalScrollBars: right
;;  Emacs.toolBar: off
;;  Emacs*Background: #000000
;;  Emacs*Foreground: #7f7f7f
;;
(defun merge-x-resources ()
  (let ((file (file-name-nondirectory (buffer-file-name))))
    (when (or (string= file ".Xdefaults")
	      (string= file ".Xresources"))
      (start-process "xrdb" nil "xrdb" "-merge" (buffer-file-name))
      (message (format "Merged %s into X resource database" file)))))
(add-hook 'after-save-hook 'merge-x-resources)

;;; ** Autorevert
;;  (global-auto-revert-mode 1)
;;; ** Decompress compressed files
;;  Auto decompress compressed files.
;;
(auto-compression-mode t)

;;; ** Quickly save (F2)
(bind-key "<f2>" 'save-buffer)

;;; ** Unique buffer names
;;
;;  Make two buffers with the same file name open distinguishable.
;;  https://www.gnu.org/software/emacs/manual/html_node/emacs/Uniquify.html
;;
;;  (require 'uniquify)
;;  (setq uniquify-buffer-name-style 'forward)
;;; ** Package: recentf
;;
;; Save recent files
(setq recentf-save-file (concat emacs-d "tmp/recentf.el")
      recentf-exclude '("bbdb$"
			"svn-commit.tmp$"
			".git/COMMIT_EDITMSG$"
			".git/TAG_EDITMSG")
      recentf-max-saved-items 1000
      recentf-auto-cleanup 300
      recentf-max-menu-items 20)

(recentf-mode 1)

;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; * Minibuffer
(setq ;; Don't insert current directory into minubuffer
      insert-default-directory nil

      ;; enable recursive minibuffer, they're a nuisance
      enable-recursive-minibuffers nil

      ;; minibuffer window expands vertically as necessary to hold the text that
      ;; you put in the minibuffer
      resize-mini-windows t
      )

;;
;;  Allow to type space chars in minibuffer input (for `timeclock-in', for
;;  example).
;;
(define-key minibuffer-local-completion-map " " nil)
(define-key minibuffer-local-must-match-map " " nil)





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; * Searching
;;; ** Package: isearch
(setq ;; Scrolling while searching
      isearch-allow-scroll t

      ;; Save Isearch stuff
      isearch-resume-in-command-history t)

(bind-key "C-y" 'isearch-yank-kill isearch-mode-map)

;;
;;; ** Command; my-grep
;;  Prompts you for an expression, defaulting to the symbol that your
;;  cursor is on, and greps for that in the current directory and all
;;  subdirectories:
;;
(defun my-grep ()
  "grep the whole directory for something defaults to term at cursor position"
  (interactive)
  (let ((default (thing-at-point 'symbol)))
    (let ((needle (or (read-string (concat "grep for '" default "': ")) default)))
      (setq needle (if (equal needle "") default needle))
      (grep (concat "egrep -s -i -n -r " needle " *")))))

(bind-key "M-s g" 'my-grep)


;;
;;; ** Invoke isearch from occur
(defun isearch-occur ()
  "Invoke `occur' from within isearch."
  (interactive)
  (let ((case-fold-search isearch-case-fold-search))
    (occur (if isearch-regexp isearch-string (regexp-quote isearch-string))))
  (pop-to-buffer "*Occur*"))

(bind-key "C-o" 'isearch-occur isearch-mode-map)





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; * Help
;;; ** Go to back to previous help buffer
;;  Make 'b' (back) go to the previous position in emacs help.
;;
;;  [[http://www.emacswiki.org/cgi-bin/wiki/EmacsNiftyTricks]]
;;
(add-hook 'help-mode-hook
	  '(lambda ()
	     (bind-key "b" 'help-go-back help-mode-map)))

;;; ** F1 key searches in help or opens man page
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

(bind-key "<f1>" 'my-help)

;;
;;; ** Apropos
;;  Check all variables and non-interactive functions as well
(setq apropos-do-all t)

;; Help should search more than just commands
(bind-key "C-h a" 'apropos)

;;; ** Show unbound keys
;;
;;  If you call it and enter 5 as complexity, you'll see something like
;;
;;  34 unbound keys with complexity at most 5:
;;  C-x y
;;  C-x x
;;  C-x w
;;  ...
;;
(use-package unbound
  :defer t
  :commands describe-unbound-keys
)

;;; ** Show keytable
;;
;;  Beside (describe-personal-keybindings) you might want to have a keytable:
;;
(use-package keytable
  :defer t
  :commands my-keytable
  ;; :init
  ;; (define-key global-map [menu-bar tools keytable] '("Keytable" . my-keytable))
)





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; * Miscelleanous
;;; ** Mouse
;;  Paste at text-cursor, not at mouse-cursor:
;;
(setq mouse-yank-at-point t)

;;
;;  Let the mouse-wheel move the cursor in a sane manner.
;;  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
;;  (setq mouse-wheel-progressive-speed nil)
;;  (setq mouse-wheel-follow-mouse +1)
;;
;;  When it is /typing time/, do not show the mouse cursor. Be at ease, it will
;;  return once you move it again.
;;
;;  (setq make-pointer-invisible +1)
;;
;;; ** Localisation
;;  A sentence doesn't end with two spaces:
;;
(setq sentence-end-double-space nil)

;;; ** Decimal entry of quoted characters
;;  Use decimal for `C-q', not octal. Hey, who's using octal nowaydays?
;;
(setq read-quoted-char-radix 10)

;;; ** Swap RET and C-j
(bind-key "RET" 'newline-and-indent)

(bind-key "C-j" 'newline)

;;; ** dos2unix
(defun dos2unix()
  "convert dos (^M) end of line to unix end of line"
  (interactive)
  (goto-char(point-min))
  (while (search-forward "\r" nil t) (replace-match "")))

;;; ** 822date
;;  Inserts something like "Fri,  1 Dec 2006 15:41:36 +0100"
;;
(defun 822date ()
  "Insert date at point format the RFC822 way."
  (interactive)
  (insert (format-time-string "%a, %e %b %Y %H:%M:%S %z")))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; * Package: ace-jump-buffer

(use-package ace-jump-buffer
  :defer t
  :bind ("C-c C-j" . ace-jump-buffer)
  )


;; * Package: avy (alternative to ace-jump-mode)

(use-package avy
  :defer t
  :bind ("C-." . avy-goto-word-1)
  :config (setq avy-keys (append (number-sequence ?a ?z)
				 (number-sequence ?0 ?9))
                avy-style 'at-full
		avy-all-windows nil
		avy-highlight-first t)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;






;;; ** Package: org (must be before helm)

(use-package org
  :init
  (progn
    (setq org-replace-disputed-keys t    ; allow Shift-Cursor to mark stuff
	  org-default-notes-file (expand-file-name "notes.org" emacs-d)
	  ;; Time stamp handling
	  org-display-custom-times t
	  org-time-stamp-formats '("<%Y-%m-%d>" . "<%Y-%m-%d %H:%M>")
	  org-time-stamp-custom-formats '("<%Y-%m-%d>")
          ))
  :config
  (progn
    (setq org-src-window-setup 'current-window
          org-src-fontify-natively t     ; inside src block use the colors like the major mode of the src type
          org-src-tab-acts-natively t    ; inside a src block let tab act like it was in major mode of the src type
          org-src-preserve-indentation t ; don't add two indentation spaces into src blocks
	  )
    ;; normally I'd need C-c ' to exit, but this enables the same exit
    ;; method I have in when doing a commit in magit.
    (bind-key "C-c C-c" 'org-edit-src-exit org-src-mode-map)
    )
)

(eval-after-load 'ox-html
  '(progn
     (setq org-html-postamble-format '(("en" "<p class=\"author\">Author: %a</p><p class=\"creator\">Created with %c</p>"))
           org-html-postamble t)
 ))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; * Package: helm
;;
;;  Very good intro: http://tuhdo.github.io/helm-intro.html
;;
(use-package helm
  :defer nil
  :ensure helm
  :diminish helm-mode
  :bind (
	 ("C-h a"   . helm-apropos)
	 ("C-x C-f" . helm-find-files)
	 ("M-s o"   . helm-occur)
	 ("M-s i"   . helm-imenu)
	 ("M-s m"   . helm-imenu)
	 ("M-x"     . helm-M-x)
	 ("M-y"     . helm-show-kill-ring)
         ("C-x C-b"   . helm-mini)
	 )
  :init
  (progn
    (require 'helm-config nil t)
    (helm-mode t)
    )
  :config
  (progn
    ;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
    ;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
    ;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
    ;; (from http://tuhdo.github.io/helm-intro.html)
    (bind-key "C-c h" 'helm-command-prefix)
    (global-unset-key (kbd "C-x c"))

    (when (executable-find "curl")
      (setq helm-google-suggest-use-curl-p t))

    ;; allow "find man at point" for C-c h m (helm-man-woman)
    (add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)

    (setq helm-candidate-number-limit 100
          helm-quick-update t
          helm-M-x-requires-pattern nil
          helm-ff-skip-boring-files t

	  ;; open helm buffer inside current window, not occupy whole other window
	  helm-split-window-in-side-p t
	  ;; move to end or beginning of source when reaching top or bottom of source.
	  helm-move-to-line-cycle-in-source t
	  ;; search for library in `require' and `declare-function' sexp.
	  helm-ff-search-library-in-sexp t
	  ;; scroll 8 lines other window using M-<next>/M-<prior>
	  helm-scroll-amount 8

	  ;; test this out
	  helm-ff-file-name-history-use-recentf t

	  ;; define browser
	  helm-browse-url-chromium-program "x-www-browser"
	  helm-google-suggest-default-browser-function 'helm-browse-url-chromium
	  helm-home-url "http://www.google.de"
	  )
    (helm-autoresize-mode t)

    ;; ignore Emacs save files
    (add-to-list 'helm-boring-file-regexp-list "\\.#")

    ;; this is kind of a goto, you can visit all marks
    (bind-key "g"   'helm-all-mark-rings helm-command-map)
    );; end progn
)





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; * Package: helm-descbinds
(use-package helm-descbinds
  :defer t
  :bind ("C-h b" . helm-descbinds))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; * Package: savehist (save mini-buffer history)
(use-package savehist
  :init
   (setq savehist-file (concat emacs-d "tmp/history.el")
      history-length 1000)
  :config
  (savehist-mode 1)
)

;;
;;
;;
;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; * Programming
;;; ** Tab handling
;;  Tabify only initial whitespace
;;
(setq tabify-regexp "^\t* [ \t]+")

;;
;;  Deleting past a tab normally changes tab into spaces. Don't do that,
;;  kill the tab instead.
;;
(setq backward-delete-char-untabify-method nil)

;;; ** Disable vc backends
;;  We only use git, not other version controls:
;;
(setq vc-handled-backends nil)

;;
;;; ** Let parenthesis behave
(show-paren-mode 1)
(setq show-paren-delay 0
      blink-matching-parent nil)

;;
;;; ** qmake project files
;;  Don't open Qt's *.pro files as IDLWAVE files.
;;
;;  TODO: look for a real qmake-mode
(add-to-list 'auto-mode-alist '("\\.pro$" . fundamental-mode))

;;; ** Commenting
(bind-key "C-c c" 'comment-dwim)

;;; ** Compilation
(defun my-compile ()
  (interactive)
  (delete-other-windows)
  (save-buffer)
  (if (fboundp 'eproject-root)
      (let ((default-directory (eproject-root)))
	(compile compile-command))
    (compile compile-command)))

(bind-key "<f7>" 'my-compile)


(defun set-compile-command (&optional cmd)
  "Helper for to set compile-command"
  (interactive "scmd: ")
  (setq compile-command cmd))

;;
;;; *** Auto close compile log if there are no errors
;;
;;  [[http://www.emacswiki.org/emacs/ModeCompile]]
;;
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

;;
;;; *** Error navigation
;;

(bind-key "<f8>" 'next-error)

(bind-key "S-<f8>" 'previous-error)

;;; ** Mark files with shebang executable
(add-hook 'after-save-hook
	  'executable-make-buffer-file-executable-if-script-p)

;;; ** Mode: C, C++

;; Open *.h files normally in c++ mode
(add-to-list 'auto-mode-alist '("\\.h$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.inl\\'" . c++-mode))

;; from linux/Documentation/CodingStyle, used in coding style "linux-tabs-only"
(defun c-lineup-arglist-tabs-only (ignored)
  "Line up argument lists by tabs, not spaces"
  (let* ((anchor (c-langelem-pos c-syntactic-element))
         (column (c-langelem-2nd-pos c-syntactic-element))
         (offset (- (1+ column) anchor))
         (steps (floor offset c-basic-offset)))
    (* (max steps 1)
       c-basic-offset)))

(defun my-c-electric-brace-open (arg)
  "This just inserts the spaces, a newline, and indents into the
newline to the correct position"
  (interactive "*P")
  (self-insert-command (prefix-numeric-value arg))
  (newline-and-indent)
  )


;; somehow a the first visited file stays in "gnu" style when I set the c-default-style
;; just in the common hook
(defun my-c-initialization-setup ()
  ;; Default style
  (c-add-style "linux-tabs-only"
	       '("linux" (c-offsets-alist (arglist-cont-nonempty
					   c-lineup-gcc-asm-reg
					   c-lineup-arglist-tabs-only))))
  (setq c-default-style '((java-mode . "javax")
			  (awk-mode . "awk")
			  (other . "linux")))
   )
(add-hook 'c-initialization-hook 'my-c-initialization-setup)


;; Thinks that will only ever apply to .C files
(defun my-c-mode-setup ()
  (when (and buffer-file-name
                 (string-match "linux" buffer-file-name))
    (progn (c-set-style "linux-tabs-only")
	   (setq tab-width 8
		 c-basic-offset 8))))
(add-hook 'c-mode-hook 'my-c-mode-setup)


;; Thinks that will apply to .C and .CPP files
(defun my-c-mode-common-setup ()
  (define-key c-mode-map "(" 'self-insert-command)
  (define-key c-mode-map ")" 'self-insert-command)
  (define-key c-mode-map "{" 'my-c-electric-brace-open)
  (turn-off-auto-fill)
  (c-toggle-auto-newline 1)
  ;; This makes things like super_function_for_you a word
  (modify-syntax-entry ?_ "w")
  (setq fill-column 78
	;; indent by 4 (almost) everywhere
	tab-width 4
	c-basic-offset 4
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
	;; Keep electric mode on for now
	;; c-electric-flag nil
	;; But if it's on, let a "#" go to the left, for #if/#else/#endif
	c-electric-pound-behavior '(alignleft)
	;; No abbrevs
	abbrev-mode nil
	)
  )
(add-hook 'c-mode-common-hook 'my-c-mode-common-setup)


;;; ** Mode: ELisp

(defun my--elisp-setup ()
  ;; Setup imenu
  ;;(setq imenu-prev-index-position-function nil)
  (add-to-list 'imenu-generic-expression '("Section" "^;;; \\(.*\\)" 1) t)

  ;; automatically give help about function syntax
  (eldoc-mode t)
  ;; "-" is almost always part of a function- or variable-name
  (modify-syntax-entry ?- "w"))

(add-hook 'emacs-lisp-mode-hook 'my--elisp-setup)


;;; ** Mode: Markdown
(use-package markdown-mode
  :mode (("\\.md\\'"       . markdown-mode)
	 ("\\.markdown\\'" . markdown-mode))
  )

;;; ** Mode: Python

(defun my-python-setup ()
  (interactive)
  (setq indent-tabs-mode t
	python-indent-offset 4
	tab-width 4
	;; this fixes the weird indentation when entering a colon
	;; from http://emacs.stackexchange.com/questions/3322/python-auto-indent-problem
	electric-indent-chars (delq ?: electric-indent-chars)
	))
(add-hook 'python-mode-hook 'my-python-setup)


;;; ** Mode: Rust
(use-package rust-mode
  :mode (("\\.rs\\'"       . rust-mode))
  )

;;; ** Mode: Shell

(defun my-shell-tab-setup ()
  (interactive)
  (setq indent-tabs-mode t
	tab-width 4
	tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84))
  (define-key text-mode-map "\C-i" 'self-insert-command)
  )

(add-hook 'shell-mode-hook 'my-shell-tab-setup)
(add-hook 'sh-mode-hook 'my-shell-tab-setup)


;;; ** Mode: web-mode
;;
;;  Home page: http://web-mode.org/
;;
(defun my-web-mode-hook ()
  ;; (whitespace-turn-off)
  ;; (rainbow-turn-off)
  ;; (visual-line-mode)
  ;; (local-set-key (kbd "RET") 'newline-and-indent)
  (setq web-mode-markup-indent-offset 2
	web-mode-css-indent-offset 2
	web-mode-code-indent-offset 2
	web-mode-indent-style 2
	web-mode-style-padding 1
	web-mode-script-padding 1
	web-mode-block-padding 0)
  ;;(gcr/untabify-buffer-hook))
)
(use-package web-mode
  :defer t
  :mode (("\\.html\\'" . web-mode)
	 ("\\.css\\'" . web-mode)
	 ("\\.json\\'" . web-mode)
	 ("\\.js\\'" . web-mode)
	 )
  :init
  (add-hook 'web-mode-hook 'my-web-mode-hook)
  :config
  (progn
    (setq web-mode-enable-block-partial-invalidation t
	  web-mode-engines-alist '(("ctemplate" . "\\.html$"))
	  )
    )
)

;;; *** Default browser
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "x-www-browser")

;;; ** Package: column-marker
;;
(defun my--column-marker-at-80 ()
  (interactive)
  (column-marker-2 80)
     ;(set-face-foreground 'magit-diff-add "green4")
  ;; column-marker-1
  ;; xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
)

(use-package column-marker
  :defer t
  :commands (column-marker-1 column-marker-2)
  :init
  (add-hook 'c-mode-hook 'my--column-marker-at-80)
  )






;;; ** Package: magit

;;
;;  =magit-rigid-key-bindings= must be set before magit is loaded. It will
;;  remove the new key bindings that use pop-up buffers.
;;
(setq magit-rigid-key-bindings t)

(use-package magit
  :defer t
  :diminish magit-auto-revert-mode  ;; disable "MRev" in the status line
  :init
  ;; disable warning about magit-auto-revert-mode
  (setq magit-last-seen-setup-instructions "1.4.0")
  :config
  (progn
    (setq magit-save-some-buffers 'dontask
          magit-commit-all-when-nothing-staged nil
          magit-stage-all-confirm nil
          magit-unstage-all-confirm nil
          magit-status-buffer-switch-function 'switch-to-buffer
          magit-refresh-file-buffer-hook '(revert-buffer)
          magit-diff-use-overlays t
          magit-completing-read-function 'completing-read
          )
     ;(set-face-foreground 'magit-diff-add "green4")
     ;(set-face-foreground 'magit-diff-del "red3")
     )
  :bind ("C-c m" . magit-status)
  :commands (magit-get-top-dir)
)

;;(autoload 'magit-get-top-dir "magit" nil t)

(defun magit-refresh-status ()
  (magit-git-exit-code "update-index" "--refresh")
  (magit-create-buffer-sections
    (magit-with-section 'status nil
      (run-hooks 'magit-status-insert-sections-hook)))
  (run-hooks 'magit-refresh-status-hook))
