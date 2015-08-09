;;;_ * Hide/show subtries with "C-c C-c C-s" or "C-c C-c h"


;;;_ * Load paths
(defvar emacs-d (file-name-directory
		 (file-chase-links load-file-name))
  "My emacs dotfiles directory, ~/.emacs.d on Linux")

(setq package-user-dir
      (expand-file-name "elpa" emacs-d))
(package-initialize)

;; Please don't load outdated byte code
(setq load-prefer-newer t)

(let ((emacs-git (expand-file-name "git/" emacs-d)))
  (mapc (lambda (x)
          (add-to-list 'load-path (expand-file-name x emacs-git)))
        (delete ".." (directory-files emacs-git))))

;; When using org-mode from GIT:
;;(add-to-list 'load-path (expand-file-name "git/org-mode/lisp/" emacs-d))

(add-to-list 'load-path (expand-file-name "elisp/" emacs-d))

(require 'auto-compile)
(auto-compile-on-load-mode 1)
(auto-compile-on-save-mode 1)
;;;_ * Customize
;; http://lists.gnu.org/archive/html/emacs-devel/2015-04/msg01261.html
;; http://oremacs.com/2015/01/17/setting-up-ediff/
;;
;; This macro I've put together myself after searching though the code
;; base and not finding something similar; custom-set-variables comes
;; close to what I want, or maybe custom-initialize-changed. Basically
;; all I want is a setq that is aware of the custom-set property of a
;; variable. If you know such a macro, please let me know.
(defmacro csetq (variable value)
  `(funcall (or (get ',variable 'custom-set)
		'set-default)
	    ',variable ,value))
;;;_  . Window Decorations
(csetq tool-bar-mode nil)
;(csetq menu-bar-mode nil)
(csetq scroll-bar-mode nil)
(csetq inhibit-startup-screen t)
;;;_  . Window manager
;; Avoid Emacs hanging for a while after changing default font
(modify-frame-parameters nil '((wait-for-wm . nil)))
;;;_  . Theme
(require 'afternoon-theme)
;; put something like this into ~/.Xresources
;; Emacs.geometry: 120x55
;; Emacs.Font:     Terminus 11
;;;_  . Blend fringe
;; http://emacs.stackexchange.com/a/5343/115
(defun my-blend-fringe ()
  (interactive)
  "Set the fringe foreground and background color to that of the theme."
  (set-face-attribute 'fringe nil
                      :foreground (if (string= (face-foreground 'default) "unspecified-fg")
                                      "#f7f7f7" (face-foreground 'default))
                      :background (if (string= (face-background 'default) "unspecified-bg")
									  "#282828" (face-background 'default))))
(my-blend-fringe)
;;;_  . Entering/exiting Emacs
; Do without annoying startup msg.
(csetq inhibit-startup-message t)
; This inhibits the initial startup echo area message.
(eval '(csetq inhibit-startup-echo-area-message "schurig"))
; Empty scratch message
(csetq initial-scratch-message nil)
; Include current buffer name in the title bar
(csetq frame-title-format '(buffer-file-name "%f" ("%b")))
; TODO Set up default editing mode.
; (csetq major-mode 'indented-text-mode)
; Custom file, part one
(csetq custom-file (concat emacs-d "custom.el"))
; Delete previous identical history entries
(csetq history-delete-duplicates t)
;;;_  . Emacs internals
(csetq gc-cons-threshold (* 10 1024 1024))
(csetq message-log-max 10000)
;; Use new byte codes from Emacs 24.4
(setq byte-compile--use-old-handlers nil)
(csetq ad-redefinition-action 'accept)
;;;_  . Default browser
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "x-www-browser")
;;;_  . Simpler yes or no prompt
;  Get rid of yes-or-no questions - y or n is enough
(fset 'yes-or-no-p 'y-or-n-p)
;;;_  . Private data
(csetq user-full-name "Holger Schurig")
(csetq user-mail-address "holgerschurig@gmail.com")
(load (concat emacs-d "private.el") 'noerror 'nomessage)
;;;_  . Load customization file
(if (file-exists-p custom-file) (load-file custom-file))
;;;_  . Mouse
;; Paste at text-cursor, not at mouse-cursor:
(csetq mouse-yank-at-point t)
;;;_  . Localisation
;; A sentence doesn't end with two spaces:
(csetq sentence-end-double-space nil)
;;;_  . Customization buffer
;; keep lisp names in the custom buffers, don't capitalize
(csetq custom-unlispify-tag-names nil)
;;;_ * Package infrastructure
;;;_  . package
;; ELPA might use Emacs-W3 to get files, and this in turn sets cookies.
;; Move the cookie file out into the =tmp/= directory.
(csetq url-configuration-directory (concat emacs-d "tmp/"))
(require 'package)
(csetq package-enable-at-startup nil)
(csetq package-archives
	   '(("melpa-stable" . "http://stable.melpa.org/packages/")
		 ("melpa"        . "http://melpa.org/packages/")
		 ("gnu"          . "http://elpa.gnu.org/packages/")
		 ;; for org-plus-contrib
		 ;; ("org"          . "http://orgmode.org/elpa/")
		 ))

;; This is my over version. The original version doesn't read
;; all archives when called with no-fetch.
(defun list-packages (&optional no-fetch)
  "Display a list of packages.
This first fetches the updated list of packages before
displaying, unless a prefix argument NO-FETCH is specified.
The list is displayed in a buffer named `*Packages*'."
  (interactive "P")
  (require 'finder-inf nil t)
  ;; Initialize the package system if necessary.
  (unless package--initialized
    (package-initialize t))
  (let (old-archives new-packages)
    ;; Read the locally-cached archive-contents.
    (package-read-all-archive-contents)
    (setq old-archives package-archive-contents)
    ;; Fetch the remote list of packages.
    (unless no-fetch
      (package-refresh-contents))
    ;; Find which packages are new.
    (dolist (elt package-archive-contents)
      (unless (assq (car elt) old-archives)
	(push (car elt) new-packages)))

    ;; Generate the Package Menu.
    (let ((buf (get-buffer-create "*Packages*")))
      (with-current-buffer buf
	(package-menu-mode)
	(set (make-local-variable 'package-menu--new-package-list)
	     new-packages)
	(package-menu--generate nil t))
      ;; The package menu buffer has keybindings.  If the user types
      ;; `M-x list-packages', that suggests it should become current.
      (switch-to-buffer buf))

    (let ((upgrades (package-menu--find-upgrades)))
      (if upgrades
	  (message "%d package%s can be upgraded; type `%s' to mark %s for upgrading."
		   (length upgrades)
		   (if (= (length upgrades) 1) "" "s")
		   (substitute-command-keys "\\[package-menu-mark-upgrades]")
		   (if (= (length upgrades) 1) "it" "them"))))))
;;;_  . use-package
;; See http://github.com/jwiegley/use-package/
;; or http://www.lunaryorn.com/2015/01/06/my-emacs-configuration-with-use-package.html
;;
;; (use-package package-name
;;      [:keyword [option]]...)
;;
;; :init          Code to run when `use-package' form evals.
;; :config        Runs if and when package loads.
;; :mode          Form to be added to `auto-mode-alist'.
;; :interpreter   Form to be added to `interpreter-mode-alist'.
;; :commands      Define autoloads for given commands.
;; :bind          Perform key bindings, and define autoload for bound
;;                commands.
;; :pre-load      Code to run when `use-package' form evals and before
;;                anything else. Unlike :init this form runs before the
;;                package is required or autoloads added.
;; :defer         Defer loading of package -- automatic
;;                if :commands, :bind, :mode or :interpreter are used.
;; :demand        Prevent deferred loading in all cases.
;; :if            Conditional loading.
;; :disabled      Ignore everything.
;; :defines       Define vars to silence byte-compiler.
;; :load-path     Add to `load-path' before loading.
;; :diminish      Support for diminish package (if it's installed).
;; :idle          adds a form to run on an idle timer
;; :idle-priority schedules the :idle form to run with the given
;;                priority (lower priorities run first). Default priority
;;                is 5; forms with the same priority are run in the order in
;;                which they are evaluated.
;; :ensure        loads package using package.el if necessary.

;; Automatically install `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
;;;_ * Editing
;;;_  . Transpose
;; http://endlessparentheses.com/transposing-keybinds-in-emacs.html

(bind-key "\C-t" #'transpose-lines)
(bind-key "\C-t" #'transpose-chars  ctl-x-map)
;;;_  . Undo
;; This lets you use C-z or C-x u (undo-tree-visualize) to visually walk
;; through the changes you've made, undo back to a certain point (or
;; redo), and go down different branches.
(use-package undo-tree
  :defer t
  :ensure t
  :diminish undo-tree-mode
  :commands (undo-tree-visualize)
  :bind ("C-z" . undo-tree-visualize)
  :idle
  (progn
    (global-undo-tree-mode)
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-visualizer-diff t)))
;;;_ * Display
;;;_  . Font locking
;; Highlight each of TODO FIXME XXX DISABLED \todo :disabled
(defface my--todo-face
  '((t :foreground "red"
       :weight bold))
  "Font for showing TODO words."
  :group 'basic-faces)
(defun my--hint-facify ()
   (unless (or (eq 'diff-mode major-mode) (eq 'script-mode major-mode))
     (font-lock-add-keywords nil '(
	 ("\\(\\<\\(\\(FIXME\\|TODO\\|XXX\\|DISABLED\\):?\\>\\)\\|\\\\todo\\|:disabled:?\\)" 1 'my--todo-face t)
	 ))))
(add-hook 'find-file-hook 'my--hint-facify)

;; highlight special comment lines
(defface my--elisp-section-face
  '((t :foreground "dark blue"
       :background "dark blue"))
  "Font for showing elisp sections."
  :group 'basic-faces)

(font-lock-add-keywords 'emacs-lisp-mode '(
	 ("^\\(;;;?_\\) " 1 'my--elisp-section-face t)
	 ("^\\(;;;;;;;;+\\)" 1 'my--elisp-section-face t)
	 ))
;;;_  . Line truncation
;; don't display continuation lines
(setq-default truncate-lines t)
;; Do `M-x toggle-truncate-lines` to toggle truncation mode.
;; `truncate-partial-width-windows' has to be nil for `toggle-truncate-lines'
;; to work even in split windows
(csetq truncate-partial-width-windows nil)
;;;_  . Show trailing whitespace
(add-hook 'find-file-hook
	  '(lambda ()
	     ;; This hack allows the diff-mode hook to set the
	     ;; variable to -1. The find-file-hook fires after the
	     ;; diff-mode hook, so we get the -1 and are able to turn
	     ;; off the display of trailing whitespaces.
	     (if (eq show-trailing-whitespace -1)
		 (csetq show-trailing-whitespace nil)
	       (csetq show-trailing-whitespace t))))
;;;_  . Buffers without toolbar, extra frame etc
(add-to-list 'special-display-buffer-names "*Backtrace*")
(add-to-list 'special-display-frame-alist '(tool-bar-lines . 0))
;;;_  . Misc settings for text vs. windowing systems
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
    (csetq visible-cursor nil)))
;;;_  . No audible bell
(csetq visible-bell t)
;;;_  . Let emacs react faster to keystrokes
(csetq echo-keystrokes 0.1)
(csetq idle-update-delay 0.35)
;;;_  . Mode line setup
;; Show line and column numbers in the mode-line
(line-number-mode 1)
(column-number-mode 1)
;;;_  . Smart mode line
;; https://github.com/Malabarba/smart-mode-line
(use-package smart-mode-line
  :config
  (csetq sml/line-number-format    "%4l")
  (csetq sml/name-width 40) ; buffer name width in the mode-line
  (csetq sml/mode-width 'full) ; minor mode lighters area width
  (csetq sml/shorten-modes nil)
  (csetq sml/no-confirm-load-theme t)
  (csetq sml/replacer-regexp-list '())
  (if (display-graphic-p)
      (progn
	(require 'smart-mode-line-powerline-theme)
	(csetq sml/theme 'powerline)
	)
    (csetq sml/theme 'light))
  (sml/setup)
  (when (display-graphic-p)
    (set-face-attribute 'sml/prefix nil :foreground "white")
    (set-face-attribute 'sml/folder nil :foreground "white")
    (set-face-attribute 'sml/filename nil :foreground "white")
    (set-face-attribute 'sml/position-percentage nil :foreground "#660000")
    (set-face-attribute 'sml/modes nil :foreground "gray50"))
  )
;;;_ * Cursor movement
;; First we define code that allows us to bind multiple functions to
;; repeated commands. Taken from
;; [[http://www.emacswiki.org/cgi-bin/wiki/DoubleKeyBinding]]:
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
;;;_  . Home / End
(defvar my--previous-position)
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
;;;_  . Recenter
(csetq recenter-positions '(middle 4 -4))
;;;_  . Nicer goto-line
;; Doesn't modify minibuffer-history, but use it's own little history
;; list.
(defvar my-goto-line-history '())
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
;;;_  . expand-region
;; Home page: https://github.com/magnars/expand-region.el
;;
;; C-+ Expand region increases the selected region by semantic units.
;;
;; You can then either continue to press C-+ to expand even further, or
;; use + and - after the first expand to expand further / shrink again.
(use-package expand-region
  :defer t
  :bind ("C-+" . er/expand-region)
  :config
  (csetq expand-region-reset-fast-key    "<ESC><ESC>"))
;;;_  . avy (alternative to ace-jump-mode)
(use-package avy
  :defer t
  :bind ("C-#" . avy-goto-word-1)
  :config (progn
	    (csetq avy-keys (append (number-sequence ?a ?z)
				    (number-sequence ?0 ?9)))
	    (csetq avy-style 'at-full)
	    (csetq avy-all-windows nil)
	    (csetq avy-highlight-first t)))
;;;_ * Yank and Delete
;;;_  . Delete word or yank
;; The following may be of interest to people who (a) are happy with
;; "C-w" and friends for killing and yanking, (b) use
;; "transient-mark-mode", (c) also like the traditional Unix tty
;; behaviour that "C-w" deletes a word backwards. It tweaks "C-w" so
;; that, if the mark is inactive, it deletes a word backwards instead
;; of killing the region. Without that tweak, the C-w would create an
;; error text without an active region.
;; http://www.emacswiki.org/emacs/DefaultKillingAndYanking#toc2
(defadvice kill-region (before unix-werase activate compile)
  "When called interactively with no active region, delete a single word
    backwards instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (save-excursion (backward-word 1) (point)) (point)))))
;;;_  . Selection deletion
;; Use delete-selection mode:
(delete-selection-mode t)
;;;_  . Deletion in readonly buffer
;; Be silent when killing text from read only buffer:
(csetq kill-read-only-ok t)
;;;_  . Join lines at killing
;; If at end of line, join with following; otherwise kill line.
;; Deletes whitespace at join.
(defun kill-and-join-forward (&optional arg)
  "If at end of line, join with following; otherwise kill line.
Deletes whitespace at join."
  (interactive "P")
  (if (and (eolp) (not (bolp)))
      (delete-indentation t)
    (kill-line arg)))
(bind-key "C-k" 'kill-and-join-forward)
;;;_  . Dynamic char deletion
;; The following is from Boojum's post in
;; [[http://www.reddit.com/r/emacs/comments/b1r8a/remacs_tell_us_about_the_obscure_but_useful/]].
;;
;; I don't want to kill the comment, just the prefix to it. So that
;;
;; // The quick brown fox[]
;; // jumps over the lazy dog.
;;
;; becomes
;;
;; // The quick brown fox[] jumps over the lazy dog.
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
;;;_ * Completion
;; ignore case when reading a file name completion
(csetq read-file-name-completion-ignore-case t)
;; do not consider case significant in completion (GNU Emacs default)
(setq completion-ignore-case t)
;; lets TAB do completion as well
(csetq tab-always-indent 'complete)
(csetq completions-format 'vertical)
;;;_ * Windows handling
;;;_  . delete-window
;; If only one window in frame, `delete-frame'.
;; From http://www.emacswiki.org/emacs/frame-cmds.el
(defadvice delete-window (around delete-window (&optional window) activate)
  (interactive)
  (save-current-buffer
    (setq window (or window (selected-window)))
    (select-window window)
    (if (one-window-p t)
	(delete-frame)
      ad-do-it (selected-window))))
;;;_  . new kill-buffer-and-window
;; Replacement for interactive `kill-buffer'. We cannot redefine
;; `kill-buffer', because other elisp code relies on it's exact
;; behavior.
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
  (kill-buffer buffer))
(bind-key "C-x k" 'my--kill-buffer-and-window)
;;;_  . Window sizing
(bind-key "<M-down>" 'enlarge-window)
(bind-key "<M-up>" 'shrink-window)
;;;_  . Window zooming (F5)
;; If there is only one window displayed, act like C-x 2. If there are
;; two windows displayed, act like C-x 1
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
      (switch-to-buffer firstbuf))))
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
    (delete-other-windows)))
(bind-key "<f5>" 'my-explode-window)

;; ** Winner mode
(use-package winner
  :init
  (winner-mode))
;;;_ * Buffers
;;;_  . Insert buffer
;; |-------+---------------|
;; | C-x i | insert file   |
;; |-------+---------------|
;; | C-x I | insert buffer |
;; |-------+---------------|
;; Insert buffer at current position
(bind-key "C-x I" 'insert-buffer)
;;;_  . Protect buffers
;; https://raw.githubusercontent.com/lewang/le_emacs_libs/master/keep-buffers.el
(eval-when-compile (require 'cl))
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
  :group 'keep-buffers)
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
;;;_  . Easier kill buffers with processes
;; Don't asks you if you want to kill a buffer with a live process
;; attached to it:
;; http://www.masteringemacs.org/articles/2010/11/14/disabling-prompts-emacs/
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
	     kill-buffer-query-functions))
;;;_  . Cycle buffers with iflipb
;; http://www.emacswiki.org/emacs/iflipb
(use-package iflipb
  :defer t
  :commands (iflipb-next-buffer iflipb-previous-buffer)
  :bind (("<f6>"   . my-iflipb-next-buffer)
  	 ("S-<f6>" . my-iflipb-previous-buffer))
  :config
  (csetq iflipb-wrap-around t)

  (setq my-iflipb-auto-off-timeout-sec 4.5)
  (setq my-iflipb-auto-off-timer-canceler-internal nil)
  (setq my-iflipb-ing-internal nil)
  (defun my-iflipb-auto-off ()
    (message nil)
    (setq my-iflipb-auto-off-timer-canceler-internal nil
	  my-iflipb-ing-internal nil))
  (defun my-iflipb-next-buffer (arg)
    (interactive "P")
    (iflipb-next-buffer arg)
    (if my-iflipb-auto-off-timer-canceler-internal
	(cancel-timer my-iflipb-auto-off-timer-canceler-internal))
    (run-with-idle-timer my-iflipb-auto-off-timeout-sec 0 'my-iflipb-auto-off)
    (setq my-iflipb-ing-internal t))
  (defun my-iflipb-previous-buffer ()
    (interactive)
    (iflipb-previous-buffer)
    (if my-iflipb-auto-off-timer-canceler-internal
	(cancel-timer my-iflipb-auto-off-timer-canceler-internal))
    (run-with-idle-timer my-iflipb-auto-off-timeout-sec 0 'my-iflipb-auto-off)
    (setq my-iflipb-ing-internal t))
  (defun iflipb-first-iflipb-buffer-switch-command ()
    "Determines whether this is the first invocation of
  iflipb-next-buffer or iflipb-previous-buffer this round."
    (not (and (or (eq last-command 'my-iflipb-next-buffer)
		  (eq last-command 'my-iflipb-previous-buffer))
	      my-iflipb-ing-internal)))
)
;;;_  . ace-jump-buffer
(use-package ace-jump-buffer
  :defer t
  :bind ("C-c C-j" . ace-jump-buffer)
  )
;;;_ * File opening/saving
;;;_  . Basic settings
;; Never show GTK file open dialog
(csetq use-file-dialog nil)
;; don't add newlines to end of buffer when scrolling, but show them
(csetq next-line-add-newlines nil)
;; Preserve hard links to the file you�re editing (this is
;; especially important if you edit system files)
(csetq backup-by-copying-when-linked t)
;; Just never create backup files at all
;;make-backup-files nil
(csetq backup-directory-alist (list (cons "." (concat emacs-d "tmp/bak/"))))
;; Make sure your text files end in a newline
(csetq require-final-newline t)
;; Disable auto-save (#init.el# file-names)
(csetq auto-save-default nil)
(csetq auto-save-list-file-prefix (concat emacs-d "tmp/auto-save-list/saves-"))
;; Kill means kill, not asking. Was:
(setq kill-buffer-query-functions nil)
;;;_  . Automatically load .Xresources after changes
;; Sample ~/.Xresources:
;;
;; Emacs.geometry: 120x55
;; Emacs.Font:	terminus 11
(defun merge-x-resources ()
  (let ((file (file-name-nondirectory (buffer-file-name))))
    (when (or (string= file ".Xdefaults")
	      (string= file ".Xresources"))
      (start-process "xrdb" nil "xrdb" "-merge" (buffer-file-name))
      (message (format "Merged %s into X resource database" file)))))
(add-hook 'after-save-hook 'merge-x-resources)
;;;_  . Autorevert
(global-auto-revert-mode 1)
;; Don't ask when running revert-buffer when reverting files in this
;; list of regular expressions:
(csetq revert-without-query '(""))
;;;_  . Decompress compressed files
(auto-compression-mode t)
;;;_  . Quickly save (F2)
(bind-key "<f2>" 'save-buffer)
;;;_  . Unique buffer names
(use-package uniquify
  :config (csetq uniquify-buffer-name-style 'forward))
;;;_  . recentf
(csetq recentf-save-file (concat emacs-d "tmp/recentf.el"))
(csetq recentf-exclude '("bbdb$"
			 "svn-commit.tmp$"
			 ".png$"
			 "COMMIT_EDITMSG" "COMMIT_EDITMSG" "TAG_EDITMSG"))
(csetq recentf-max-saved-items 1000)
(csetq recentf-auto-cleanup 300)
(csetq recentf-max-menu-items 20)

(recentf-mode 1)
;;;_ * Minibuffer
;; Don't insert current directory into minubuffer
(csetq insert-default-directory nil)
;; Minibuffer window expands vertically as necessary to hold the text
;; that you put in the minibuffer
(csetq resize-mini-windows t) ;; was grow-only
;; Read quoted chars with radix 16
(csetq read-quoted-char-radix 16)
;; Allow to type space chars in minibuffer input (for `timeclock-in',
;; for example).
(define-key minibuffer-local-completion-map " " nil)
(define-key minibuffer-local-must-match-map " " nil)
;;;_  . save mini-buffer history
(use-package savehist
  :init
   (setq savehist-file (concat emacs-d "tmp/history.el")
      history-length 1000)
  :config
  (savehist-mode 1))
;;;_ * Searching
;;;_  . isearch (incremental search)
;; Scrolling while searching
(csetq isearch-allow-scroll t)
(bind-key "C-y" 'isearch-yank-kill isearch-mode-map)
;;;_  . Command; my-grep
;; Prompts you for an expression, defaulting to the symbol that your
;; cursor is on, and greps for that in the current directory and all
;; subdirectories:
(defun my-grep ()
  "grep the whole directory for something defaults to term at cursor position"
  (interactive)
  (let ((default (thing-at-point 'symbol)))
    (let ((needle (or (read-string (concat "grep for '" default "': ")) default)))
      (setq needle (if (equal needle "") default needle))
      (grep (concat "egrep -s -i -n -r " needle " *")))))
(bind-key "M-s g" 'my-grep)
;;;_ * Help
;;;_  . Go to back to previous help buffer
;; Make 'b' (back) go to the previous position in emacs help.
;; [[http://www.emacswiki.org/cgi-bin/wiki/EmacsNiftyTricks]]
(add-hook 'help-mode-hook
	  '(lambda ()
	     (bind-key "b" 'help-go-back help-mode-map)))
;;;_  . F1 key searches in help or opens man page
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
;;;_  . Apropos
(bind-key "C-h a" 'apropos)
;;;_  . Guide key
;; It's hard to remember keyboard shortcuts. The guide-key package
;; pops up help after a short delay.
(use-package guide-key
  :defer t
  :diminish guide-key-mode
  :idle
  (csetq guide-key/guide-key-sequence
	'("C-c" "C-h" "C-x" "M-g" "M-s"))
  (csetq guide-key/recursive-key-sequence-flag t)
  (guide-key-mode 1))
;;;_  . DISABLED Which key
;; This is mostly better than guide-key, but only mostly. It doesn't
;; automatically switch into a multi-column mode for C-x, for example.
(use-package which-key
  :disabled t
  :config
  (which-key-mode)
  (which-key-setup-side-window-right-bottom)
  )
;;;_ * Miscelleanous
;;;_  . Swap RET and C-j
(bind-key "RET" 'newline-and-indent)
(bind-key "C-j" 'newline)
;;;_  . dos2unix
(defun dos2unix()
  "convert dos (^M) end of line to unix end of line"
  (interactive)
  (goto-char(point-min))
  (while (search-forward "\r" nil t) (replace-match "")))
;;;_  . 822date
;; Inserts something like "Fri,  1 Dec 2006 15:41:36 +0100"
(defun 822date ()
  "Insert date at point format the RFC822 way."
  (interactive)
  (insert (format-time-string "%a, %e %b %Y %H:%M:%S %z")))
;;;_ * Org-Mode (must be before helm)
(use-package org
  :defer t
  :init
  (progn
    ;; allow Shift-Cursor to mark stuff
    (csetq org-replace-disputed-keys t)
    (csetq org-default-notes-file (expand-file-name "notes.org" emacs-d))
    ;; Time stamp handling
    (csetq org-display-custom-times t)
    (csetq org-time-stamp-formats '("<%Y-%m-%d>" . "<%Y-%m-%d %H:%M>"))
    (csetq org-time-stamp-custom-formats '("<%Y-%m-%d>")))
  :config
    (add-hook 'org-mode-hook 'auto-fill-mode)
    (setq org-startup-folded 'content)
    (setq org-return-follows-link t)
    ;; Various org-src related things:
    (setq org-src-window-setup 'current-window)
    ;; inside src block use the colors like the major mode of the src type
    (csetq org-src-fontify-natively t)
    ;; inside a src block let tab act like it was in major mode of the src type
    (csetq org-src-tab-acts-natively t)
    ;; don't add two indentation spaces into src blocks
    (csetq org-src-preserve-indentation t)
    ;; normally I'd need C-c ' to exit, but this enables the same exit
    ;; method I have in when doing a commit in magit.
    (bind-key "C-c C-c" 'org-edit-src-exit org-src-mode-map))
;;;_  . ox
(use-package ox
  :defer t
  :config
  ;; #+OPTIONS ':t
  (csetq org-export-with-smart-quotes t)
  ;; #+OPTIONS num:nil
  (csetq org-export-with-section-numbers nil)
  ;; #+OPTIONS stat:t
  ;; (csetq org-export-with-statistics-cookies nil)
  ;; #+OPTIONS toc:nil, use "#+TOC: headlines 2" or similar if you need a headline
  (csetq org-export-with-toc nil)
  ;; #+OPTIONS ^:{}
  (csetq org-export-with-sub-superscripts nil))
;;;_  . ox-html
(use-package ox-html
  :defer t
  :config
    (csetq org-html-postamble-format '(("en" "<p class=\"author\">Author: %a</p><p class=\"creator\">Created with %c</p>")))
    (csetq org-html-validation-link nil)
    (csetq org-html-postamble nil)
    (csetq org-html-style-default "<style type=\"text/css\">\n <!--/*--><![CDATA[/*><!--*/\n  body { text-align: center; font-family: \"Aria\", sans-serif; }\n  #content { margin: 0 auto; width: 860px; text-align: left; }\n  #text-table-of-contents > ul > li { margin-top: 1em; }\n  .title  { text-align: center; }\n  .todo   { font-family: monospace; color: red; }\n  .done   { color: green; }\n  .tag    { background-color: #eee; font-family: monospace;\n            padding: 2px; font-size: 80%; font-weight: normal; }\n  .timestamp { color: #bebebe; }\n  .timestamp-kwd { color: #5f9ea0; }\n  .right  { margin-left: auto; margin-right: 0px;  text-align: right; }\n  .left   { margin-left: 0px;  margin-right: auto; text-align: left; }\n  .center { margin-left: auto; margin-right: auto; text-align: center; }\n  .underline { text-decoration: underline; }\n  #postamble p, #preamble p { font-size: 90%; margin: .2em; }\n  p.verse { margin-left: 3%; }\n  pre {\n    border: 1px solid #ccc;\n    box-shadow: 3px 3px 3px #eee;\n    padding: 8pt;\n    font-family: monospace;\n    overflow: auto;\n    margin: 1em 0;\n  }\n  pre.src {\n    position: relative;\n    overflow: visible;\n    padding-top: 8pt;\n  }\n  pre.src:before {\n    display: none;\n    position: absolute;\n    background-color: white;\n    top: -10px;\n    right: 10px;\n    padding: 3px;\n    border: 1px solid black;\n  }\n  pre.src:hover:before { display: inline;}\n  pre.src-sh:before    { content: 'sh'; }\n  pre.src-bash:before  { content: 'sh'; }\n  pre.src-emacs-lisp:before { content: 'Emacs Lisp'; }\n  pre.src-R:before     { content: 'R'; }\n  pre.src-perl:before  { content: 'Perl'; }\n  pre.src-java:before  { content: 'Java'; }\n  pre.src-sql:before   { content: 'SQL'; }\n\n  table { border-collapse:collapse; }\n  caption.t-above { caption-side: top; }\n  caption.t-bottom { caption-side: bottom; }\n  td, th { vertical-align:top;  }\n  th.right  { text-align: center;  }\n  th.left   { text-align: center;   }\n  th.center { text-align: center; }\n  td.right  { text-align: right;  }\n  td.left   { text-align: left;   }\n  td.center { text-align: center; }\n  dt { font-weight: bold; }\n  .footpara:nth-child(2) { display: inline; }\n  .footpara { display: block; }\n  .footdef  { margin-bottom: 1em; }\n  .figure { padding: 1em; }\n  .figure p { text-align: center; }\n  .inlinetask {\n    padding: 10px;\n    border: 2px solid gray;\n    margin: 10px;\n    background: #ffffcc;\n  }\n  #org-div-home-and-up\n   { text-align: right; font-size: 70%; white-space: nowrap; }\n  textarea { overflow-x: auto; }\n  .linenr { font-size: smaller }\n  .code-highlighted { background-color: #ffff00; }\n  .org-info-js_info-navigation { border-style: none; }\n  #org-info-js_console-label\n    { font-size: 10px; font-weight: bold; white-space: nowrap; }\n  .org-info-js_search-highlight\n    { background-color: #ffff00; color: #000000; font-weight: bold; }\n  .ulClassNameOrID > li {}\n  /*]]>*/-->\n</style>")
    (csetq org-html-table-default-attributes '(:border "2" :cellspacing "0" :cellpadding "6"))
    (csetq org-html-postamble t))
;;;_ * Packages
;;;_  . allout
(use-package allout
  :defer t
  :diminish allout-mode
  :commands allout-mode
  :config
  (csetq allout-command-prefix (kbd "C-c C-c"))
  ;; honor allout-layout in file-local variables
  (csetq allout-auto-activation t)
  ;; (csetq allout-default-layout '()
  (add-to-list 'allout-prefixed-keybindings '("[(control ?h)]" allout-hide-current-subtree))
  (add-to-list 'allout-prefixed-keybindings '("[?s]" allout-show-current-subtree))
  (add-to-list 'allout-prefixed-keybindings '("[?S]" allout-show-all))
  (add-to-list 'allout-prefixed-keybindings '("[?H]" allout-hide-bodies))
  (add-to-list 'allout-prefixed-keybindings '("[?b]" allout-hide-bodies))
  ;; (bind-key "C-c s" 'allout-show-current-subtree allout-mode-map-value) ; also on C-c C-c C-s
  (allout-compose-and-institute-keymap)
  )
;;;_  . dired
(use-package dired
    :commands dired
    :init
    (setq dired-listing-switches
          "-laGh1v --group-directories-first"))
(use-package dired-x
    :commands dired-jump)
;;;_  . ispell
;; change dictionary with (ispell-change-dictionary) or put this into the first line:
;; -*- ispell-dictionary: "german-new8" -*-
(use-package ispell
 :defer t
 ;; try ispell-message-dictionary-alist
 ;;make aspell faster but less correctly
 (csetq ispell-extra-args '("--sug-mode=ultra")))
;;;_  . flyspell
(use-package flyspell
 :defer t
 :diminish flyspell-mode
 :commands (flyspell-mode flyspell-prog-mode)
 :config
 (add-to-list 'flyspell-dictionaries-that-consider-dash-as-word-delimiter "german-new8")
 (csetq flyspell-issue-welcome-flag nil)
 ;; M-Tab is owned by the window manager, correct with C-M-i
 (csetq flyspell-use-meta-tab nil)
 ;; Flyspell hijacked C-., which I want to use for tags
 (define-key flyspell-mode-map [(control ?\.)] nil)
 (bind-key "C-c C-s" 'flyspell-correct-word-before-point flyspell-mode-map)
 )
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
;;;_  . helm
;; Very good intro: http://tuhdo.github.io/helm-intro.html
(defun my-helm-imenu ()
  "This is just like helm-imenu, but it will maximize the buffer"
  (interactive)
  (let ((helm-full-frame t))
    (helm-imenu)))
(use-package helm
  :defer nil
  :ensure helm
  :diminish helm-mode
  :bind (
	 ("C-h a"   . helm-apropos)
	 ("C-x C-f" . helm-find-files)
	 ("M-s o"   . helm-occur)
	 ("M-s i"   . my-helm-imenu)
	 ("M-s m"   . my-helm-imenu)
	 ("M-x"     . helm-M-x)
	 ("M-y"     . helm-show-kill-ring)
         ("C-x C-b"   . helm-mini)
	 )
  :init
  (progn
    (require 'helm-config)
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

    (csetq helm-imenu-delimiter " ")
    (csetq helm-candidate-number-limit 100)
    (csetq helm-quick-update t)
    (setq helm-M-x-requires-pattern nil)
    (csetq helm-ff-skip-boring-files t)

    ;; open helm buffer inside current window, not occupy whole other window
    (csetq helm-split-window-in-side-p t)
    ;; move to end or beginning of source when reaching top or bottom of source.
    (csetq helm-move-to-line-cycle-in-source t)
    ;; search for library in `require' and `declare-function' sexp.
    (csetq helm-ff-search-library-in-sexp t)
    ;; scroll 8 lines other window using M-<next>/M-<prior>
    (csetq helm-scroll-amount 8)

    ;; test this out
    (csetq helm-ff-file-name-history-use-recentf t)

    ;; define browser
    (setq helm-browse-url-chromium-program "x-www-browser")
    (csetq helm-google-suggest-default-browser-function 'helm-browse-url-chromium)
    (csetq helm-home-url "http://www.google.de")
    (csetq helm-autoresize-mode t)

    ;; ignore Emacs save files
    (add-to-list 'helm-boring-file-regexp-list "\\.#")

    ;; see (customize-group "helm-files-faces")
    (set-face-attribute 'helm-ff-directory       nil :foreground 'unspecified :background 'unspecified)
    (set-face-attribute 'helm-ff-executable      nil :foreground 'unspecified :background 'unspecified)
    (set-face-attribute 'helm-ff-file            nil :foreground 'unspecified :background 'unspecified :inherit 'unspecified)
    (set-face-attribute 'helm-ff-invalid-symlink nil :foreground 'unspecified :background 'unspecified)
    ;;(set-face-attribute 'helm-ff-prefix          nil :foreground 'unspecified :background 'unspecified)
    (set-face-attribute 'helm-ff-symlink         nil :foreground 'unspecified :background 'unspecified)
    (set-face-attribute 'helm-history-deleted    nil :foreground 'unspecified :background 'unspecified)
    (set-face-attribute 'helm-history-remote     nil :foreground 'unspecified :background 'unspecified)

    ;; this is kind of a goto, you can visit all marks
    (bind-key "g"   'helm-all-mark-rings helm-command-map)))
;;;_  . helm-descbinds
(use-package helm-descbinds
  :defer t
  :bind ("C-h b" . helm-descbinds))
;;;_  . helm-swoop
;; https://github.com/ShingoFukuyama/helm-swoop
(use-package helm-swoop
  :defer t
  :bind (("M-s s"  . helm-swoop)
	 ("M-s M-s" . helm-swoop)
	 ("M-s S"   . helm-swoop-back-to-last-point))
  :config
  (csetq helm-swoop-split-direction 'split-window-sensibly)
  ;; Switch to edit mode with C-c C-e, and exit edit mode with C-c C-c
  (bind-key "C-c C-c" 'helm-swoop--edit-complete helm-swoop-edit-map)
  ;; When doing isearch, hand the word over to helm-swoop
  (bind-key "M-s s"   'helm-swoop-from-isearch isearch-mode-map)
  (bind-key "M-s M-s" 'helm-swoop-from-isearch isearch-mode-map)
  ;; Move up and down like isearch
  (bind-key "C-r" 'helm-previous-line helm-swoop-map)
  (bind-key "C-s" 'helm-next-line     helm-swoop-map)
  (bind-key "C-r" 'helm-previous-line helm-multi-swoop-map)
  (bind-key "C-s" 'helm-next-line     helm-multi-swoop-map))
;;;_ * Programming
;;;_  . Tab handling
(use-package tabify
  :defer t
  :commands (tabify untabify)
  :config
  ;; Tabify only initial whitespace
  (setq tabify-regexp "^\t* [ \t]+"))

;; Deleting past a tab normally changes tab into spaces. Don't do that,
;; kill the tab instead.
(csetq backward-delete-char-untabify-method nil)
;;;_  . Disable vc backends
;; We only use git, not other version controls:
(setq vc-handled-backends nil)
;;;_  . Let parenthesis behave
(show-paren-mode 1)
(setq show-paren-delay 0
      blink-matching-parent nil)
;;;_  . qmake project files
;; Don't open Qt's *.pro files as IDLWAVE files.
;; TODO: look for a real qmake-mode
(add-to-list 'auto-mode-alist '("\\.pro$" . fundamental-mode))
;;;_  . Commenting
(bind-key "C-c c" 'comment-dwim)
;;;_  . Compilation
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

;;; *** Auto close compile log if there are no errors
;; [[http://www.emacswiki.org/emacs/ModeCompile]]
(defun compile-autoclose (buffer string)
  (cond ((string-match "finished" string)
	 ;; (message "Build maybe successful: closing window.")
	 (run-with-timer 1 nil
			 'delete-window
			 (get-buffer-window buffer t)))
	(t
	 (message "Compilation exited abnormally: %s" string))))
(use-package compile
    :diminish compilation-in-progress
    :config
    (setq compilation-finish-functions 'compile-autoclose)
    (csetq compilation-ask-about-save nil)
    (csetq compilation-scroll-output t))

;;; *** Error navigation
(bind-key "<f8>" 'next-error)
(bind-key "S-<f8>" 'previous-error)
;;;_  . Automatically safe files with shebang executable
(add-hook 'after-save-hook
	  'executable-make-buffer-file-executable-if-script-p)
;;;_  . Mode: C, C++
(defvar c-syntactic-element)
(eval-when-compile (require 'cc-mode))
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
  (newline-and-indent))
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
			  (other . "linux"))))
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
  (bind-key "(" 'self-insert-command c-mode-map)
  (bind-key ")" 'self-insert-command c-mode-map)
  (bind-key "{" 'my-c-electric-brace-open c-mode-map)
  (turn-off-auto-fill)
  ;; Normally electric mode should be off
  (electric-indent-local-mode -1)
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
	))
(add-hook 'c-mode-common-hook 'my-c-mode-common-setup)
;;;_  . Mode: Ediff
;; http://oremacs.com/2015/01/17/setting-up-ediff/
(defun my--ediff-hook ()
  (ediff-setup-keymap)
  (bind-key "j" 'ediff-next-difference ediff-mode-map)
  (bind-key "k" 'ediff-previous-difference ediff-mode-map))

(use-package ediff
  :config
  (csetq ediff-window-setup-function 'ediff-setup-windows-plain)
  (csetq ediff-split-window-function 'split-window-horizontally)
  (csetq ediff-diff-options "-w")
  (add-hook 'ediff-mode-hook 'my--ediff-hook)
  (add-hook 'ediff-after-quit-hook-internal 'winner-undo))
;;;_  . Mode: ELisp
(defun my--elisp-setup ()
  ;; Setup imenu TODO
  (add-to-list 'imenu-generic-expression '(""  "^;;;_ \\(.*\\)" 1) t)
  ;; automatically give help about function syntax
  (eldoc-mode t)
  ;; "-" is almost always part of a function- or variable-name
  (modify-syntax-entry ?- "w")
  ;; Compile Emacs Lisp source files after the visiting buffers are saved.
  (unless (string= (buffer-name) "*scratch*")
    (auto-compile-mode 1)
    (require 'allout))
  )
(add-hook 'emacs-lisp-mode-hook 'my--elisp-setup)
;;;_  . Mode: Markdown
(use-package markdown-mode
  :defer t
  :mode (("\\.md\\'"       . markdown-mode)
	 ("\\.markdown\\'" . markdown-mode)))
;;;_  . Mode: Python
(eval-when-compile (require 'python))
(defun my-python-setup ()
  (interactive)
  (setq indent-tabs-mode t
	python-indent-offset 4
	tab-width 4
	;; this fixes the weird indentation when entering a colon
	;; from http://emacs.stackexchange.com/questions/3322/python-auto-indent-problem
	electric-indent-chars (delq ?: electric-indent-chars)))
(add-hook 'python-mode-hook 'my-python-setup)
;;;_  . Mode: Rust
(use-package rust-mode
  :defer t
  :mode (("\\.rs\\'" . rust-mode)))
;;;_  . Mode: Shell
(defun my-shell-tab-setup ()
  (interactive)
  (csetq indent-tabs-mode t)
  (csetq tab-width 4)
  (csetq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84))
  (bind-key "C-i" 'self-insert-command text-mode-map))
(add-hook 'shell-mode-hook 'my-shell-tab-setup)
(add-hook 'sh-mode-hook    'my-shell-tab-setup)
;;;_  . Mode: web-mode
;; Home page: http://web-mode.org/
(eval-when-compile (require 'web-mode))
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
	web-mode-block-padding 0))
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
  (setq web-mode-enable-block-partial-invalidation t
	web-mode-engines-alist '(("ctemplate" . "\\.html$"))))
;;;_  . column-marker
(defun my--column-marker-at-80 ()
  (interactive)
  (column-marker-2 80))
(use-package column-marker
  :defer t
  :commands (column-marker-1 column-marker-2)
  :init
  (add-hook 'c-mode-hook 'my--column-marker-at-80)
  )
;;;_  . Package: magit
;; Must be set before magit is loaded. It will remove the new key
;; bindings that use pop-up buffers.
(setq magit-rigid-key-bindings t)
(use-package magit
  :defer t
  :diminish magit-auto-revert-mode  ;; disable "MRev" in the status line
  :init
  ;; disable warning about magit-auto-revert-mode
  (setq magit-last-seen-setup-instructions "1.4.0")
  :config
  (progn
    ;; Save modified buffers without asking
    (csetq magit-save-some-buffers 'dontask)
    ;; (setq magit-commit-all-when-nothing-staged nil)
    (csetq magit-stage-all-confirm nil)
    (csetq magit-unstage-all-confirm nil)
    ;; switch the current window to magit-status (was pop-to-buffer)
    (csetq magit-status-buffer-switch-function 'switch-to-buffer)
    ;; (csetq magit-refresh-file-buffer-hook '(revert-buffer))
    (csetq magit-use-overlays nil)
    (csetq magit-item-highlight-face nil)
    (csetq magit-completing-read-function 'completing-read)
    ;; (set-face-foreground 'magit-diff-add "green4")
    ;; (set-face-foreground 'magit-diff-del "red3")
     )
  :bind ("C-c m" . magit-status)
  :commands (magit-get-top-dir))
;;;_  . Package: magit-timemachine
(use-package git-timemachine
  :defer t
  :commands git-timemachine
  )
;;;_ * Emacs server
(require 'server)
(add-hook 'server-switch-hook 'raise-frame)
(unless (server-running-p)
  (server-start))
;;;_ * Local file variables
;; Local Variables:
;;   allout-layout: 0
;; End:
