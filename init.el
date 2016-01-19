;;;_ * Optional debug
;; (toggle-debug-on-error)
;;;_ * Load paths
(defvar emacs-d (file-name-directory
		 (file-chase-links load-file-name))
  "My emacs dotfiles directory, ~/.emacs.d on Linux")

(package-initialize)

;; Please don't load outdated byte code
(setq load-prefer-newer t)

(let ((emacs-git (expand-file-name "git/" emacs-d)))
  (mapc (lambda (x)
          (add-to-list 'load-path (expand-file-name x emacs-git)))
        (delete ".." (directory-files emacs-git))))

(add-to-list 'load-path (expand-file-name "elisp/" emacs-d))
;;;_ * Package infrastructure
;;;_ ** package
;; ELPA might use Emacs-W3 to get files, and this in turn sets cookies.
;; Move the cookie file out into the =tmp/= directory.
(let ((dir (concat emacs-d "tmp/")))
  (ignore-errors (make-directory dir))
  (setq url-configuration-directory dir))
(require 'package)
(setq package-enable-at-startup nil
      package-archives
      '(("melpa"           . "http://melpa.org/packages/")
        ;; ("melpa-stable" . "http://stable.melpa.org/packages/")
	;; ("gnu"          . "http://elpa.gnu.org/packages/")
	;; ("org"          . "http://orgmode.org/elpa/") ;; for org-plus-contrib
	))

;; This is my own version. The original version doesn't read
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
;;;_ ** use-package
;; See http://github.com/jwiegley/use-package/
;; or http://www.lunaryorn.com/2015/01/06/my-emacs-configuration-with-use-package.html
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
;; :mode          Form to be added to `auto-mode-alist'.
;; :interpreter   Form to be added to `interpreter-mode-alist'.
;; :commands      Define autoloads for commands that will be defined by the
;;                package.  This is useful if the package is being lazily loaded,
;;                and you wish to conditionally call functions in your `:init'
;;                block that are defined in the package.
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
;; :after         Defer loading of a package until after any of the named
;;                features are loaded.
;; :demand        Prevent deferred loading in all cases.
;; :if EXPR       Initialize and load only if EXPR evaluates to a non-nil value.
;; :disabled      The package is ignored completely if this keyword is present.
;; :defines       Declare certain variables to silence the byte-compiler.
;; :functions     Declare certain functions to silence the byte-compiler.
;; :load-path     Add to the `load-path' before attempting to load the package.
;; :diminish      Support for diminish.el (if installed).
;; :ensure        Loads the package using package.el if necessary.
;; :pin           Pin the package to an archive.


;; Automatically install `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
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
;;;_ ** Window Decorations
(csetq tool-bar-mode nil)
;(csetq menu-bar-mode nil)
(csetq scroll-bar-mode nil)
(eval-after-load "startup" '(fset 'display-startup-echo-area-message 'ignore))
(csetq inhibit-startup-screen t)
;;;_ ** Window manager
;; Avoid Emacs hanging for a while after changing default font
(modify-frame-parameters nil '((wait-for-wm . nil)))
;;;_ ** Theme
(use-package afternoon-theme
  :ensure t)
;; put something like this into ~/.Xresources
;; Emacs.geometry: 120x55
;; Emacs.Font:     Terminus 11
;;;_ ** Blend fringe
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
;;;_ ** Entering/exiting Emacs
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
;;;_ ** Emacs internals
(csetq gc-cons-threshold (* 10 1024 1024))
(csetq message-log-max 10000)
;; Use new byte codes from Emacs 24.4
(setq byte-compile--use-old-handlers nil)
(csetq ad-redefinition-action 'accept)
;;;_ ** allow some commands
(put 'erase-buffer 'disabled nil)
;;;_ ** Default browser
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "x-www-browser")
;;;_ ** Simpler yes or no prompt
;  Get rid of yes-or-no questions - y or n is enough
(fset 'yes-or-no-p 'y-or-n-p)
;;;_ ** Private data
(csetq user-full-name "Holger Schurig")
(csetq user-mail-address "holgerschurig@gmail.com")
(load (concat emacs-d "private.el") 'noerror 'nomessage)
;;;_ ** Load customization file
(if (file-exists-p custom-file) (load-file custom-file))
;;;_ ** Mouse
;; Paste at text-cursor, not at mouse-cursor:
(csetq mouse-yank-at-point t)
;;;_ ** Localisation
;; A sentence doesn't end with two spaces:
(csetq sentence-end-double-space nil)
;;;_ ** Customization buffer
;; keep lisp names in the custom buffers, don't capitalize
(csetq custom-unlispify-tag-names nil)
;;;_ * Editing
;;;_ ** Transpose
;; http://endlessparentheses.com/transposing-keybinds-in-emacs.html

(bind-key "\C-t" #'transpose-lines)
(bind-key "\C-t" #'transpose-chars  ctl-x-map)
;;;_ ** Undo
;; This lets you use C-z or C-x u (undo-tree-visualize) to visually walk
;; through the changes you've made, undo back to a certain point (or
;; redo), and go down different branches.
(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :commands (undo-tree-visualize)
  :bind ("C-z" . undo-tree-visualize)
  :config
  (progn
    (global-undo-tree-mode)
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-visualizer-diff t)))
;;;_ * Display
;;;_ ** Font locking
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
;;;_ ** Line truncation
;; don't display continuation lines
(setq-default truncate-lines t)
;; Do `M-x toggle-truncate-lines` to toggle truncation mode.
;; `truncate-partial-width-windows' has to be nil for `toggle-truncate-lines'
;; to work even in split windows
(csetq truncate-partial-width-windows nil)
;;;_ ** Show trailing whitespace
(defun my--show-trailing-whitespace ()
  (interactive)
  (csetq show-trailing-whitespace t))
(defun my--hide-trailing-whitespace ()
  (interactive)
  (message "hide trailing whitespace")
  (csetq show-trailing-whitespace nil))
(add-hook 'prog-mode-hook 'my--show-trailing-whitespace)
;;;_ ** Buffers without toolbar, extra frame etc
(add-to-list 'special-display-buffer-names "*Backtrace*")
(add-to-list 'special-display-frame-alist '(tool-bar-lines . 0))
;;;_ ** Misc settings for text vs. windowing systems
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
;;;_ ** No audible bell
(csetq visible-bell t)
;;;_ ** Let emacs react faster to keystrokes
(csetq echo-keystrokes 0.1)
(csetq idle-update-delay 0.35)
;;;_ ** Mode line setup
;; Show line and column numbers in the mode-line
(line-number-mode 1)
(column-number-mode 1)
;;;_ ** Smart mode line
;; https://github.com/Malabarba/smart-mode-line
(use-package smart-mode-line
  :ensure t
  :init
  (use-package smart-mode-line-powerline-theme
    :ensure t)
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
;;;_ ** Home / End
(defvar my--previous-position)

(defun my-home ()
  "Depending on how many times it was called moves the point to:

   - begin of indentation
   - beginning of line
   - begin of function
   - beginning of buffer
   - back to where it was"
  (interactive)
  (seq-times-do nil (setq my--previous-position (point))
    (back-to-indentation)
    (beginning-of-line)
    (beginning-of-defun)
    (goto-char (point-min))
    (goto-char my--previous-position)))
;; (substitute-key-definition 'move-beginning-of-line 'my-home (current-global-map))
(bind-key "C-a" 'my-home)
(bind-key "<home>" 'my-home)


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
;;;_ ** Recenter
(csetq recenter-positions '(middle 4 -4))
;;;_ ** Nicer goto-line
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
;;;_ ** expand-region
;; Home page: https://github.com/magnars/expand-region.el
;;
;; C-+ Expand region increases the selected region by semantic units.
;;
;; You can then either continue to press C-+ to expand even further, or
;; use + and - after the first expand to expand further / shrink again.
(use-package expand-region
  :ensure t
  :bind ("C-+" . er/expand-region)
  :config
  (csetq expand-region-reset-fast-key    "<ESC><ESC>"))
;;;_ ** bookmark
(use-package bookmark
  :config
  (csetq bookmark-default-file (concat emacs-d "tmp/bookmarks.el"))
  )
;;;_ ** avy (alternative to ace-jump-mode)
(use-package avy
  :ensure t
  :bind ("C-#" . avy-goto-char-timer)
  :config (progn
	    (csetq avy-keys (append (number-sequence ?a ?z)
				    (number-sequence ?0 ?9)))
	    (csetq avy-style 'at-full)
	    (csetq avy-all-windows nil)
	    (csetq avy-highlight-first t)))
;;;_ ** smartscan
;; This makes =M-n= and =M-p= look for the symbol at point. This is
;; very un-intrusive, no pop-up, no nothing,
(use-package smartscan
  :config
  (global-smartscan-mode t)
  )
;;;_ ** Mouse scrolling
;; Smooth scrolling (default is 5).
(setq mouse-wheel-scroll-amount '(2 ((shift) . 1) ((control) . nil))
      mouse-wheel-progressive-speed nil)
;;;_ * Yank and Delete
;;;_ ** Delete word or yank
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
;;;_ ** Selection deletion
;; Use delete-selection mode:
(delete-selection-mode t)
;;;_ ** Deletion in readonly buffer
;; Be silent when killing text from read only buffer:
(csetq kill-read-only-ok t)
;;;_ ** Join lines at killing
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
;;;_ ** Dynamic char deletion
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
;;;_ ** X11 clipboard
(when (display-graphic-p)
  (csetq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))
;;;_ * Completion
;; ignore case when reading a file name completion
(csetq read-file-name-completion-ignore-case t)
;; do not consider case significant in completion (GNU Emacs default)
(setq completion-ignore-case t)
;; lets TAB do completion as well
(csetq tab-always-indent 'complete)
(csetq completions-format 'vertical)
;;;_ * Windows handling
;;;_ ** delete-window
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
;;;_ ** new kill-buffer-and-window
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
;;;_ ** Window sizing
(bind-key "<M-down>" 'enlarge-window)
(bind-key "<M-up>" 'shrink-window)
;;;_ ** Window zooming (F5)
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
;;;_ ** Winner mode
(use-package winner
  :defer 10
  :init
  (winner-mode 1))
;;;_ * Buffers
;;;_ ** Insert buffer
;; |-------+---------------|
;; | C-x i | insert file   |
;; |-------+---------------|
;; | C-x I | insert buffer |
;; |-------+---------------|
;; Insert buffer at current position
(bind-key "C-x I" 'insert-buffer)
;;;_ ** Protect buffers
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
;;;_ ** Easier kill buffers with processes
;; Don't asks you if you want to kill a buffer with a live process
;; attached to it:
;; http://www.masteringemacs.org/articles/2010/11/14/disabling-prompts-emacs/
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
	     kill-buffer-query-functions))
;;;_ ** Cycle buffers
;;;_ ** simple toggle
(defun my-switch-to-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))
(bind-key "<f6>" 'my-switch-to-buffer)
;;;_ *** iflipb
;; http://www.emacswiki.org/emacs/iflipb
(use-package iflipb
  :ensure t
  :commands (iflipb-next-buffer iflipb-previous-buffer)
  :bind ("S-<f6>" . my-iflipb-previous-buffer)
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
	      my-iflipb-ing-internal))))
;;;_ ** ace-jump-buffer DISABLED
(use-package ace-jump-buffer
  :disabled t
  :bind ("C-c C-j" . ace-jump-buffer)
  )
;;;_ * File opening/saving
;;;_ ** Basic settings
;; Never show GTK file open dialog
(csetq use-file-dialog nil)
;; don't add newlines to end of buffer when scrolling, but show them
(csetq next-line-add-newlines nil)
;; Preserve hard links to the file you´re editing (this is
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
;;;_ ** Automatically load .Xresources after changes
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
;;;_ ** Autorevert
(global-auto-revert-mode 1)
;; Don't ask when running revert-buffer when reverting files in this
;; list of regular expressions:
(csetq revert-without-query '(""))
;;;_ ** Decompress compressed files
(auto-compression-mode t)
;;;_ ** Quickly save (F2)
(bind-key "<f2>" 'save-buffer)
;;;_ ** Unique buffer names
(use-package uniquify
  :config (csetq uniquify-buffer-name-style 'forward))
;;;_ ** recentf
(csetq recentf-save-file (concat emacs-d "tmp/recentf.el"))
(csetq recentf-exclude '("^/tmp/"
			 "/.newsrc"
			 "bbdb$"
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
;;;_ ** save mini-buffer history
(use-package savehist
  :init
   (setq savehist-file (concat emacs-d "tmp/history.el")
      history-length 1000)
  :config
  (savehist-mode 1))
;;;_ * Searching
;;;_ ** isearch (incremental search)
;; Scrolling while searching
(csetq isearch-allow-scroll t)
(bind-key "C-y" 'isearch-yank-kill isearch-mode-map)
;;;_ ** Command; my-grep
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
;;;_ ** Go to back to previous help buffer
;; Make 'b' (back) go to the previous position in emacs help.
;; [[http://www.emacswiki.org/cgi-bin/wiki/EmacsNiftyTricks]]
(add-hook 'help-mode-hook
	  '(lambda ()
	     (bind-key "b" 'help-go-back help-mode-map)))
;;;_ ** F1 key searches in help or opens man page
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
;;;_ ** Apropos
(bind-key "C-h a" 'apropos)
;;;_ ** Guide key
;; It's hard to remember keyboard shortcuts. The guide-key package
;; pops up help after a short delay.
(use-package guide-key
  :ensure t
  :defer 2
  :diminish guide-key-mode
  :config
  (csetq guide-key/guide-key-sequence
	'("C-c" "C-h" "C-x" "M-g" "M-s"))
  (csetq guide-key/recursive-key-sequence-flag t)
  (guide-key-mode 1))
;;;_ * Miscelleanous
;;;_ ** Swap RET and C-j
(bind-key "RET" 'newline-and-indent)
(bind-key "C-j" 'newline)
;;;_ ** dos2unix
(defun dos2unix()
  "convert dos (^M) end of line to unix end of line"
  (interactive)
  (goto-char(point-min))
  (while (search-forward "\r" nil t) (replace-match "")))
;;;_ ** 822date
;; Inserts something like "Fri,  1 Dec 2006 15:41:36 +0100"
(defun 822date ()
  "Insert date at point format the RFC822 way."
  (interactive)
  (insert (format-time-string "%a, %e %b %Y %H:%M:%S %z")))
;;;_ * Other packages
;;;_ ** circe (IRC client)
;; see some configuration ideas at https://github.com/jorgenschaefer/circe/wiki/Configuration
(use-package circe
  :commands circe
  :config
  (csetq circe-default-part-message "Fire on mainboard error")
  (csetq circe-quit-part-message "Fire on mainboard error")
  (csetq circe-reduce-lurker-spam t)
  ;; (circe-set-display-handler "JOIN" (lambda (&rest ignored) nil))
  ;; (circe-set-display-handler "QUIT" (lambda (&rest ignored) nil))
  ;; (csetq circe-use-cycle-completion t)
  (csetq circe-format-say "{nick}: {body}")
  (csetq circe-server-killed-confirmation 'ask-and-kill-all)
  ;; Network settings
  (csetq circe-default-ip-family 'ipv4)
  (csetq circe-default-nick "schurig")
  (csetq circe-default-user "schurig")
  (csetq circe-server-auto-join-default-type 'after-auth) ; XXX try after-nick
  (csetq circe-network-options `(("Freenode"
				  :host "kornbluth.freenode.net"
				  :port (6667 . 6697)
				  :channels ("#emacs" "#emacs-circe")
				  :nickserv-password ,freenode-password)
				 ))
  ;; Misc
  ;; (setq circe-format-server-topic "*** Topic change by {userhost}: {topic-diff}")
  (use-package lui-autopaste
    :config
    (add-hook 'circe-channel-mode-hook 'enable-lui-autopaste))
  )

(defun irc ()
  "Connect to IRC"
  (interactive)
  (circe "Freenode"))
;;;_ ** dired
(use-package dired
    :commands dired
    :bind ("C-x C-d" . dired) ;; used to be list-directory, quite useless
    :init
    (setq dired-listing-switches
          "-laGh1v --group-directories-first"))
(use-package dired-x
    :commands dired-jump)
;;;_ ** helm
;; Very good intro: http://tuhdo.github.io/helm-intro.html
(defun my-helm-imenu ()
  "This is just like helm-imenu, but it will maximize the buffer"
  (interactive)
  (let ((helm-full-frame t))
    (helm-imenu)))
(use-package helm
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
      (setq helm-net-prefer-curl t))

    ;; allow "find man at point" for C-c h m (helm-man-woman)
    (add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)

    (csetq helm-imenu-delimiter " ")
    (csetq helm-candidate-number-limit 100)
    (csetq helm-quick-update t)
    (setq helm-M-x-requires-pattern nil)

    (csetq helm-ff-skip-boring-files t)
    ;; search for library in `require' and `declare-function' sexp.
    (csetq helm-ff-search-library-in-sexp t)
    (csetq helm-ff-file-name-history-use-recentf t)
    (csetq helm-ff-newfile-prompt-p nil)

    ;; open helm buffer inside current window, not occupy whole other window
    (csetq helm-split-window-in-side-p t)
    ;; move to end or beginning of source when reaching top or bottom of source.
    (csetq helm-move-to-line-cycle-in-source t)
    ;; scroll 8 lines other window using M-<next>/M-<prior>
    (csetq helm-scroll-amount 8)

    ;; define browser
    (setq helm-browse-url-chromium-program "x-www-browser")
    (csetq helm-google-suggest-default-browser-function 'helm-browse-url-chromium)
    (csetq helm-home-url "http://www.google.de")
    (csetq helm-autoresize-mode t)

    ;; ignore Emacs save files
    (add-to-list 'helm-boring-file-regexp-list "\\.#")

    ;; see (customize-group "helm-files-faces")
    (set-face-attribute 'helm-ff-directory        nil :foreground "red" :background 'unspecified)
    (set-face-attribute 'helm-ff-dotted-directory nil :foreground "red" :background 'unspecified)
    (set-face-attribute 'helm-ff-executable       nil :foreground 'unspecified :background 'unspecified)
    (set-face-attribute 'helm-ff-file             nil :foreground 'unspecified :background 'unspecified :inherit 'unspecified)
    (set-face-attribute 'helm-ff-invalid-symlink  nil :foreground 'unspecified :background 'unspecified)
    ;;(set-face-attribute 'helm-ff-prefix         nil :foreground 'unspecified :background 'unspecified)
    (set-face-attribute 'helm-ff-symlink          nil :foreground 'unspecified :background 'unspecified)
    (set-face-attribute 'helm-history-deleted     nil :foreground 'unspecified :background 'unspecified)
    (set-face-attribute 'helm-history-remote      nil :foreground 'unspecified :background 'unspecified)

    ;; this is kind of a goto, you can visit all marks
    (bind-key "g"   'helm-all-mark-rings helm-command-map)))
;;;_ ** hydra
(use-package hydra
  :defer t
  :commands (defhydra hydra-default-pre)
  )
;;;_ *** helm-descbinds
(use-package helm-descbinds
  :ensure t
  :commands helm-descbinds
  :bind (("C-h b" . helm-descbinds)
	 ("C-h w" . helm-descbinds)) ;; used to be where-is
  )
;;;_ *** helm-swoop
;; https://github.com/ShingoFukuyama/helm-swoop
(use-package helm-swoop
  :ensure t
  :commands (helm-swoop helm-swoop-back-to-last-point)
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
;;;_ * Mail & News
;;;_ ** smtpmail
;; http://emacs.stackexchange.com/questions/6105/how-to-set-proper-smtp-gmail-settings-in-emacs-in-order-to-be-able-to-work-with
;; http://superuser.com/questions/476714/how-to-configure-emacs-smtp-for-using-a-secure-server-gmail
(use-package smtpmail
  :config
  (setq smtpmail-default-smtp-server "smtp.gmail.com"
	smtpmail-smtp-server "smtp.gmail.com"
	smtpmail-stream-type 'starttls
	smtpmail-smtp-service 587
	smtpmail-debug-info t))
;;;_ ** sendmail
;; This is used by GNUS and also by Mutt

;; http://www.emacswiki.org/emacs/MuttInEmacs
;; http://dev.mutt.org/trac/wiki/MuttFaq/Editor
(defun my-mail-quit ()
  (interactive)
  (not-modified)
  (server-edit))
(defun my-mail-save ()
  (interactive)
  (save-buffer)
  (server-edit))
(defun my-mail-mode-hook ()
  (flush-lines "^\\(> \n\\)*> -- \n\\(\n?> .*\\)*") ; kill quoted sigs
  ;; (visual-line-mode t)
  (auto-fill-mode)
  (delete-trailing-whitespace)
  (mail-text)
  (fill-region (point) (point-max))
  (not-modified)
  (setq make-backup-files nil))

(use-package sendmail
  :commands (mail-mode)
  :defines (send-mail-function)
  :mode (("/tmp/mutt-*" . mail-mode))
  :config
  ;; Sending mail
  (setq send-mail-function 'smtpmail-send-it)

  (add-hook 'mail-mode-hook 'my-mail-mode-hook)
  (bind-key "C-c C-c" 'my-mail-done mail-mode-map)
  (bind-key "C-x k" 'my-mail-quit mail-mode-map)
  )
;;;_ ** message
(use-package message
  :defer t
  :config
  ;; When composing a mail, start the auto-fill-mode.
  (add-hook 'message-mode-hook 'turn-on-auto-fill)
  ;; (add-hook 'message-setup-hook 'bbdb-define-all-aliases)
  (add-hook 'gnus-startup-hook 'bbdb-insinuate-message)

  ;; Generate the mail headers before you edit your message.
  (setq message-generate-headers-first t)

  ;; The message buffer will be killed after sending a message.
  (setq message-kill-buffer-on-exit t)

  ;;(require 'starttls)
  ;;(require 'smtpmail)
  :commands message-mode
  ;;:mode (("/mutt" . message-mode))
)
;;;_ ** gnus
;; https://github.com/redguardtoo/mastering-emacs-in-one-year-guide/blob/master/gnus-guide-en.org
;; http://www.emacswiki.org/emacs/GnusGmail
;; http://www.xsteve.at/prg/gnus/
;; https://github.com/jwiegley/dot-emacs/blob/master/dot-gnus.el
(use-package gnus
  :bind ("C-c n" . gnus)
  :config

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; GNUS general
  ;; Unconditionally read the dribble file
  (setq gnus-always-read-dribble-file t)

  ;; Store gnus specific files to ~/gnus, maybe also set nnml-directory
  (setq gnus-directory (concat emacs-d "News/")
  	message-directory (concat emacs-d "Mail/")
  	gnus-article-save-directory (concat emacs-d "News/saved/")
  	gnus-kill-files-directory (concat emacs-d "News/scores/")
  	gnus-cache-directory (concat emacs-d "News/cache/"))

  (add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; GNUS sources
  ;;http://www.xsteve.at/prg/gnus/
  (setq gnus-select-method '(nntp "news.gmane.org"))

  ;; Local offlineimap repository
  (add-to-list 'gnus-secondary-select-methods
	       '(nnmaildir "" (directory "~/Maildir/")))

  ;; don't substitute my e-mail with some "-> RECEIVER" magic
  (setq gnus-ignored-from-addresses nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; GNUS summary
  ;; don't refrain to show a big amount of messages
  (setq gnus-large-newsgroup nil)

  ;; If you prefer to see only the top level message. If a message has
  ;; several replies or is part of a thread, only show the first
  ;; message. 'gnus-thread-ignore-subject' will ignore the subject and
  ;; look at 'In-Reply-To:' and 'References:' headers.
  ;; (setq gnus-thread-hide-subtree t)

  ;; Sort by date:
  (setq gnus-thread-sort-functions
  		'((not gnus-thread-sort-by-date)
  		  (not gnus-thread-sort-by-number)))


  ;;  %U  "Read" status of this article.
  ;;  %R  "A" if this article has been replied to, " "
  ;;  %d  Date of the article (string) in DD-MMM format
  ;;  %L  Number of lines in the article (integer)
  ;;  %n  Name of poster
  ;;  %B  A complex trn-style thread tree (string), see gnus-sum-thread-*
  ;;  %S  Subject (string)
  ;; Some others:
  ;;  %z  Article zcore (character), try %i
  ;;  %I  Indentation based on thread level
  ;;  %f  Contents of the From: or To: headers (string)
  ;;  %s  Subject if it is at the root of a thread, and "" otherwise
  ;;  %O  Download mark (character).
  ;; Original                    "%U%R%z%I%(%[%4L: %-23,23f%]%) %s\n"
  (setq gnus-summary-line-format "%U%R %11,11&user-date; %-22,22n %B%-80,80S\n")
  (setq gnus-user-date-format-alist '(
				      ((gnus-seconds-today)           . "%H:%M")
				      ((+ 86400 (gnus-seconds-today)) . "gest %H:%M")
				      ((gnus-seconds-year)            . "%d.%m %H:%M")
				      (t                              . "%d.%m. %Y")
				      ))

  ;; Added some keybindings to the gnus summary mode
  (define-key gnus-summary-mode-map [(meta up)] '(lambda() (interactive) (scroll-other-window -1)))
  (define-key gnus-summary-mode-map [(meta down)] '(lambda() (interactive) (scroll-other-window 1)))
  (define-key gnus-summary-mode-map [(control down)] 'gnus-summary-next-thread)
  (define-key gnus-summary-mode-map [(control up)] 'gnus-summary-prev-thread)

  ;; stop the annoying "move to colon" function
  (defun gnus-summary-position-point ()
    )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; GNUS articles
  ;; re-use one article buffer for every group
  (setq gnus-single-article-buffer t)

  ;; And this switches the cursor into this other window when we select an article
  (defun my--after-select-article (&rest args)
    (let ((window (get-buffer-window (get-buffer "*Article*"))))
      (when window
	(select-window window))))
  (advice-add 'gnus-summary-select-article :after 'my--after-select-article)

  (defun my--delete-window ()
	(interactive)
	(if (one-window-p)
		(bury-buffer)
	  (delete-window)))
  (bind-key "q"      'my--delete-window   gnus-article-mode-map)
  (bind-key "<home>" 'beginning-of-buffer gnus-article-mode-map)
  (bind-key "<end>"  'end-of-buffer       gnus-article-mode-map)
)
;;;_ ** gnus-group
(use-package gnus-group
  :defer t
  :config
  (defhydra hydra-gnus-group (:color pink :hint nil)
    "
^Local^                        | ^Server^
-----------------------------+-------------------------------------
_l_  list all groups           | _L_  list active groups on IMAP server
_c_  catch up all (mark read)  | _s_  search on server
_g_  get new news              | _S_  enter server mode
-----------------------------+-------------------------------------
_m_  mail new post
"
       ("l" gnus-group-list-all-groups)
       ("c" gnus-group-catchup-current)
       ("g" gnus-group-get-new-news)

       ("L" gnus-group-list-active :exit t)
       ("s" gnus-group-make-nnir-group :exit t)
       ("S" gnus-group-enter-server-mode :exit t)

       ("m" gnus-group-new-mail :exit t)
       ("q" nil "quit" :color blue)
       ;; ("r" gnus-group-list-active "REMOTE groups A A")
       ;; ("l" gnus-group-list-all-groups "LOCAL groups L")
       ;; ("c" gnus-topic-catchup-articles "Read all c")
       ;; ("G" gnus-group-make-nnir-group "Search server G G")
       ;; ("g" gnus-group-get-new-news "Refresh g")
       ;; ("s" gnus-group-enter-server-mode "Servers")
       ;; ("m" gnus-group-new-mail "Compose m OR C-x m")
       ;; ("#" gnus-topic-mark-topic "mark #")
       ;; ("q" nil "cancel")
       )
     ;; y is not used by default
     (bind-key "y" #'hydra-gnus-group/body gnus-group-mode-map ))
;;;_ ** gnus-sum
(use-package gnus-sum
  :defer t
  :commands (gnus-summary-last-subject gnus-summary-goto-subject)
  :config
  (defhydra hydra-gnus-summary (:color pink :hint nil)
    "
^Posting^                  ^Handling^
-----------------------+------------------
_f_  forward             | _!_  tick & keep
_e_  resend & edit       | _p_  untick
_R_  reply & quote       | _c_  catchup
_r_  reply personal      | _C_  catchup thread
_W_  wide reply & quote  | _u_  unmark
_w_  wide reply          |

_p_  save as patch
"
    ;; ("n" gnus-summary-insert-new-articles "Refresh / N")
    ("f" gnus-summary-mail-forward :exit t)
    ("e" gnus-summary-resend-message-edit :exit t)
    ("R" gnus-summary-reply-with-original :exit t)
    ("r" gnus-summary-reply :exit t)
    ("W" gnus-summary-wide-reply-with-original :exit t)
    ("w" gnus-summary-wide-reply :exit t)

    ("!" gnus-summary-tick-article-forward)
    ("p" gnus-summary-put-mark-as-read)
    ("c" gnus-summary-catchup-and-exit)
    ("C" gnus-summary-kill-thread)
    ("u" gnus-summary-clear-mark-forward)

    ("p" gnus-summary-save-article-body-file)

    ("q" nil "quit"))
  ;; y is not used by default
  (bind-key "y" #'hydra-gnus-summary/body gnus-summary-mode-map)
  )
;;;_ ** gnus-art
(use-package gnus-art
  :defer t
  :config

  ;; Test code, position cursor inside and run C-C-x to evaluate:
  ;;
  ;; (let ((filename "[PATCH 03/05] foo"))
  ;;   (when (string-match "\\[PATCH.+?0*\\([0-9]+\\)/[0-9]+\\]" filename)
  ;;     (message "%s -> %04d" filename (string-to-number (match-string 1 filename)))
  ;;     ;; (message "%s" filename)
  ;;     )
  ;;   ))

  ;; save news article body as patch, via gnus-summary-save-article-body-file "O b"
  (defun gnus-read-save-file-name (prompt &optional filename
					  function group headers variable
					  dir-var)
    (let ((patchnum))
      (setq filename (gnus-summary-article-subject))
      (when (string-match "\\[PATCH.+?0*\\([0-9]+\\)/[0-9]+\\]" filename)
    	(setq patchnum (string-to-number (match-string 1 filename)))
    	(message "%s" patchnum))
      ;; (when string-match "\\[PATCH.+?\\]" filename)
	  (message "FILENAME %s" filename)
      (setq filename (replace-regexp-in-string "\\[PATCH.*\\]" "" filename))
      (setq filename (replace-regexp-in-string "\[^a-zA-Z0-9]" "-" filename))
      (setq filename (replace-regexp-in-string "\\-+" "-" filename))
      (setq filename (replace-regexp-in-string "^-" "" filename))
      (setq filename (replace-regexp-in-string "-$" "" filename))
      (when patchnum
		(setq filename (concat (format "%04d" patchnum) "-" filename)))
      (setq filename (concat "/tmp/" filename ".patch"))
      (when (file-exists-p filename)
		(delete-file filename))
	  filename))

    (defhydra hydra-gnus-article (:color pink :hint nil)
    "
^Posting^                  ^Handling^
-----------------------+---------------------------
_f_  forward             | _!_  tick & keep
_R_  reply & quote       | _p_  untick
_r_  reply personal      | _c_  catchup
_W_  wide reply & quote  | _C_  catchup thread
_w_  wide reply          | _u_  unmark
                       | _o_  save attachment at point
_p_  save as patch
"
    ;; ("n" gnus-summary-insert-new-articles "Refresh / N")
    ("f" gnus-summary-mail-forward :exit t)
    ("R" gnus-article-reply-with-original :exit t)
    ("r" gnus-article-reply :exit t)
    ("W" gnus-article-wide-reply-with-original :exit t)
    ("w" gnus-article-wide-reply :exit t)

    ("!" gnus-summary-tick-article-forward)
    ("p" gnus-summary-put-mark-as-read)
    ("c" gnus-summary-catchup-and-exit)
    ("C" gnus-summary-kill-thread)
    ("u" gnus-summary-clear-mark-forward)
    ("o" gnus-mime-save-part)

    ("p" gnus-summary-save-article-body-file)

    ("q" nil "quit"))
  ;; y is not used by default
  (bind-key "y" #'hydra-gnus-article/body gnus-article-mode-map)
  )
;;;_ ** mm-decode
(use-package mm-decode
  :config
  ;; Hide HTML mail
  (require 'dash) ;; for -difference
  (setq mm-discouraged-alternatives '("text/html" "text/richtext")
  	mm-automatic-display (-difference mm-automatic-display '("text/html" "text/enriched" "text/richtext"))
	)
  )
;;;_ ** bbdb
(use-package bbdb
  :ensure t
  :defer t
  :commands (bbdb bbdb-insinuate-gnus bbdb-insinuate-message)
  :bind ("C-c b" . bbdb)
  :config
  (bbdb-initialize 'gnus 'message)

  (setq bbdb-file (concat emacs-d "db.bbdb"))
  (setq bbdb-update-records-p 'create)
  ;; (setq bbdb-mua-pop-up nil)
  (setq bbdb-silent t)
  (setq bbdb-user-mail-address-re "\\<holgerschurig@gmail.com\\>")
  (setq bbdb-add-name t)
  (setq bbdb-add-aka t)
  (setq bbdb-add-mails t)
  (setq bbdb-new-mails-primary t)
  (setq bbdb-complete-mail-allow-cycling t)
  (setq bbdd-phone-style nil)
  (setq bbdb-ignore-message-alist
		'(("From" . "mailer-daemon")
		  ("From" . "bugs.launchpad.net")
		  ("From" . "postmaster.twitter.com")
		  ("From" . "plus.google.com")
		  ("From" . "notify@twitter.com")
		  (("To" "From") . "review@openstack.org")))
  ;; (setq bbdb-allow-duplicates t)
)
;;;_ ** org
;;;_ *** org
(use-package org
  :bind (("C-c l" . org-store-link)
	 ("C-c o" . org-open-at-point-global))
  :commands (org-open-file orgstruct++-mode)
  :init
  ;; allow Shift-Cursor to mark stuff
  (csetq org-replace-disputed-keys t)

  ;; modules to load together with org-mode
  (setq org-modules '(
                      ;; org-annotate-file
                      ;; org-bbdb
                      ;; org-bibtex
                      ;; org-collector
                      ;; org-docview
                      ;; org-drill
                      ;; org-eval
                      ;; org-expiry
                      ;; org-gnus
                      ;; org-habit
                      ;; org-info
                      ;; org-interactive-query
                      ;; org-irc
                      ;; org-jsinfo
                      ;; org-man
                      ;; org-mhe
                      ;; org-mouse
                      ;; org-panel
                      ;; org-protocol
                      ;; org-rmail
                      ;; org-screen
                      ;; org-toc
                      ;; org-w3m
                      ))

  :config
  ;; My main file
  (csetq org-default-notes-file (expand-file-name "todo.org" emacs-d))

  ;; Handle deletion inside elipsis
  (setq org-catch-invisible-edits 'error)

  ;; don't fold for now
  (setq org-startup-folded nil)

  ;; Time stamp format
  (csetq org-display-custom-times t)
  (csetq org-time-stamp-formats '("<%Y-%m-%d>" . "<%Y-%m-%d %H:%M>"))
  (csetq org-time-stamp-custom-formats '("<%Y-%m-%d>"))

  ;; :bind cannot bind into a different map
  (bind-key "C-TAB"   'org-cycle org-mode-map)
  (bind-key "C-c C-j" 'helm-org-in-buffer-headings org-mode-map) ;; was org-goto
  (bind-key "C-c k"   'org-cut-subtree org-mode-map)
  (bind-key "C-c R"   'org-reveal org-mode-map)
  ;; (bind-key "C-c t"   'org-show-todo-tree org-mode-map)

  ;; adjust level
  (setq org-yank-adjusted-subtrees t)

  (add-hook 'org-mode-hook 'visual-line-mode)

  ;; make enter open the link
  (setq org-return-follows-link t)

  ;; some speed commands, use ? at the start of an org-header to see which one we have
  (add-to-list 'org-speed-commands-user '("x" org-todo "DONE"))
  (add-to-list 'org-speed-commands-user '("y" org-todo-yesterday "DONE"))
  (add-to-list 'org-speed-commands-user '("!" my/org-clock-in-and-track))
  (add-to-list 'org-speed-commands-user '("s" call-interactively 'org-schedule))
  (add-to-list 'org-speed-commands-user '("i" call-interactively 'org-clock-in))
  (add-to-list 'org-speed-commands-user '("o" call-interactively 'org-clock-out))
  (add-to-list 'org-speed-commands-user '("$" call-interactively 'org-archive-subtree))
  (add-to-list 'org-speed-commands-user '("N" org-narrow-to-subtree))
  (add-to-list 'org-speed-commands-user '("W" widen))
  (add-to-list 'org-speed-commands-user '("k" org-cut-subtree))
  ;; (add-to-list 'org-speed-commands-user '("P" call-interactively 'org2blog/wp-post-subtree))

  ;; "!"    record time stamp
  ;; "@"    add note with time
  ;; "x/y"  use x when entering state, y when leaving state
  ;; the first letter can be used with C-c C-t
  (setq org-todo-keywords
	'((sequence "TODO(t)" "STARTED(s!)" "|" "DONE(x!)")
	  (sequence "WAIT(w@/!)" "DELEGATED(d@/!)" "|" "CANCELED(c@)")
	  ))

  (setq org-todo-keyword-faces
      '(("TODO"      . (:foreground "#b70101" :weight bold))
        ("STARTED"   . (:foreground "#b70101" :weight bold))
        ("DONE"      . (:foreground "forestgreen" :weight bold))
        ("WAIT"      . (:foreground "orange" :weight bold))
        ("DELEGATED" . (:foreground "forestgreen" :weight bold))
        ("CANCELED"  . shadow)))

  ;; stamp time when done
  (setq org-log-done 'time)
  ;; use extra drawer
  (setq org-log-into-drawer t)

  ;; when my day ends
  (setq org-use-effective-time t
	org-extend-today-until 17)

  ;; Resume clocking tasks when emacs is restarted
  ;; (org-clock-persistence-insinuate)

  ;; TODO creates error
  ;; (setq org-global-properties
  ;; 	'("Effort_ALL" . "0:10 0:30 1:00 2:00 3:00 4:00 5:00 6:00 8:00"))

  ;; Try column with this:
  ;; (setq org-columns-default-format "%80ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM")

  ;; misc refile settings
  (setq org-reverse-note-order t)
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (setq org-refile-targets '((org-agenda-files . (:maxlevel . 6))))

  ;; (setq org-blank-before-new-entry nil)

  ;; export and open
  (defun my-org-export-to-html-and-open ()
    (interactive)
    (org-open-file (org-html-export-to-html)))
  (bind-key "<f7>" 'my-org-export-to-html-and-open org-mode-map)
)
;;;_ *** org-agenda
;; http://www.suenkler.info/docs/emacs-orgmode/
(use-package org-agenda
  :bind (("C-c a" . org-agenda)
	 ("C-c w" . org-agenda-list)  ;; w like week
	 )
  :config
  (bind-key "i" 'org-agenda-clock-in org-agenda-mode-map)
  ;; (bind-key "!" 'my/org-clock-in-and-track org-agenda-mode-map)

  ;; Highlight current line
  (add-hook 'org-agenda-mode-hook (defun my-org-agenda-hookfunc () (hl-line-mode 1 )))

  ;; which files the agenda should consider
  (setq org-agenda-files (list org-default-notes-file))

  ;; Let date stand out
  (setq org-agenda-format-date
	"%Y-%m-%d ---------------------------------------------------------------------")

  (setq org-agenda-show-outline-path t)

  ;; colorize priorities
  (setq org-agenda-fontify-priorities
	'((65 (:foreground "Red"))
	  (66 (:foreground "Blue"))
	  (67 (:foreground "Darkgreen"))))

  ;; hide done tasks
  (setq org-agenda-skip-deadline-if-done t)
  (setq org-agenda-skip-scheduled-if-done t)

  ;; normally hide the "someday" (nice-to-have) things
  (setq org-agenda-filter-preset '("-someday"))

  ;; show day schedule, not week schedule
  (setq org-agenda-span 'day)

  ;; own views
  (setq org-agenda-custom-commands
	'(("n" "Agenda and all TODO's"
	   ((agenda "")
	    (alltodo "")))
  	  ;; ("f" "Agenda and flagged tasks"
  	  ;;  ((tags "flagged")
  	  ;;   (agenda "")))
	  ("s" "Tagged 'someday'" tags "someday" ((org-agenda-filter-preset '("+someday"))
						  (org-agenda-todo-ignore-with-date nil)))
	  ))

  ;; show clock report
  ;; (setq org-agenda-start-with-clockreport-mode nil)

  ;; Keine Links, maximal bis Level 4 herunter:
  ;; (setq org-agenda-clockreport-parameter-plist '(:link t :maxlevel 4))
  )
;;;_ *** org-capture
(use-package org-capture
  :bind ("C-c r" . my-org-capture-todo)
  ;; ("<f9> <f8>" . (lambda () (interactive) (org-capture nil "r")))
  :config
  (defun my-org-capture-todo ()
    (interactive)
    (org-capture nil "o"))
  (setq org-capture-templates
	`(("o" "Open task" entry
	   (file+headline org-default-notes-file "Unsortiert")
	   "* TODO %?\n\n")
	  ("n" "Note" item
	   (file+headline org-default-notes-file "Infos"))
	   ))
  )
;;;_ *** org-clock
(use-package org-clock
  :bind ("C-c j" . org-clock-goto) ;; jump to current task from anywhere
  :config
  (setq org-clock-into-drawer "CLOCK")

  ;; Yes it's long... but more is better ;)
  (setq org-clock-history-length 35)

  ;; Resume clocking task on clock-in if the clock is open
  (setq org-clock-in-resume t)

  ;; Change task state to STARTED when clocking in
  (setq org-clock-in-switch-to-state "STARTED")

  ;; this removes clocked tasks with 0:00 duration
  ;; (setq org-clock-out-remove-zero-time-clocks t)

  ;; Don't clock out when moving task to a done state
  ;; (setq org-clock-out-when-done nil)

  ;; Save the running clock and all clock history when exiting Emacs,
  ;; load it on startup
  ;; (setq org-clock-persist t)

  ;; Disable auto clock resolution
  (setq org-clock-auto-clock-resolution nil)
  )
;;;_ *** org-list
(use-package org-list
  :defer t
  :functions (org-item-re)
  :config
  ;; tab changes visibility of lists like headers
  (setq org-cycle-include-plain-lists 'integrate)

  ;; speed commands are fun, not only on the headers, but also on lists
  (defun my/org-use-speed-commands-for-headings-and-lists ()
    "Activate speed commands on list items too."
    (or (and (looking-at org-outline-regexp) (looking-back "^\**"))
	(save-excursion (and (looking-at (org-item-re)) (looking-back "^[ \t]*")))))
  (setq org-use-speed-commands 'my/org-use-speed-commands-for-headings-and-lists)
)
;;;_ *** org-src
(use-package org-src
  :defer t
  :config
  ;; Open source editor in current window
  (setq org-src-window-setup 'current-window)
  ;; inside src block use the colors like the major mode of the src type
  (csetq org-src-fontify-natively t)
  ;; inside a src block let tab act like it was in major mode of the src type
  (csetq org-src-tab-acts-natively t)
  ;; don't add two indentation spaces into src blocks
  (csetq org-src-preserve-indentation t)

  ;; normally I'd need C-c ' to exit, but this enables the same exit
  ;; method I have in when doing a commit in magit.
  (bind-key "C-c C-c" 'org-edit-src-exit org-src-mode-map)
)
;;;_ *** ox
(use-package ox
  :defer t
  :config
  ;; The following make some +OPTIONS permanent:
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
;;;_ *** ox-html
(use-package ox-html
  :defer t
  :commands org-html-export-to-html
  :config
  (csetq org-html-postamble-format '(("en" "<p class=\"author\">Author: %a</p><p class=\"creator\">Created with %c</p>")))
  (csetq org-html-validation-link nil)
  (csetq org-html-postamble nil)
  (csetq org-html-style-default "<style type=\"text/css\">\n <!--/*--><![CDATA[/*><!--*/\n  body { text-align: center; font-family: \"Aria\", sans-serif; }\n  #content { margin: 0 auto; width: 860px; text-align: left; }\n  #text-table-of-contents > ul > li { margin-top: 1em; }\n  .title  { text-align: center; }\n  .todo   { font-family: monospace; color: red; }\n  .done   { color: green; }\n  .tag    { background-color: #eee; font-family: monospace;\n            padding: 2px; font-size: 80%; font-weight: normal; }\n  .timestamp { color: #bebebe; }\n  .timestamp-kwd { color: #5f9ea0; }\n  .right  { margin-left: auto; margin-right: 0px;  text-align: right; }\n  .left   { margin-left: 0px;  margin-right: auto; text-align: left; }\n  .center { margin-left: auto; margin-right: auto; text-align: center; }\n  .underline { text-decoration: underline; }\n  #postamble p, #preamble p { font-size: 90%; margin: .2em; }\n  p.verse { margin-left: 3%; }\n  pre {\n    border: 1px solid #ccc;\n    box-shadow: 3px 3px 3px #eee;\n    padding: 8pt;\n    font-family: monospace;\n    overflow: auto;\n    margin: 1em 0;\n  }\n  pre.src {\n    position: relative;\n    overflow: visible;\n    padding-top: 8pt;\n  }\n  pre.src:before {\n    display: none;\n    position: absolute;\n    background-color: white;\n    top: -10px;\n    right: 10px;\n    padding: 3px;\n    border: 1px solid black;\n  }\n  pre.src:hover:before { display: inline;}\n  pre.src-sh:before    { content: 'sh'; }\n  pre.src-bash:before  { content: 'sh'; }\n  pre.src-emacs-lisp:before { content: 'Emacs Lisp'; }\n  pre.src-R:before     { content: 'R'; }\n  pre.src-perl:before  { content: 'Perl'; }\n  pre.src-java:before  { content: 'Java'; }\n  pre.src-sql:before   { content: 'SQL'; }\n\n  table { border-collapse:collapse; }\n  caption.t-above { caption-side: top; }\n  caption.t-bottom { caption-side: bottom; }\n  td, th { vertical-align:top;  }\n  th.right  { text-align: center;  }\n  th.left   { text-align: center;   }\n  th.center { text-align: center; }\n  td.right  { text-align: right;  }\n  td.left   { text-align: left;   }\n  td.center { text-align: center; }\n  dt { font-weight: bold; }\n  .footpara:nth-child(2) { display: inline; }\n  .footpara { display: block; }\n  .footdef  { margin-bottom: 1em; }\n  .figure { padding: 1em; }\n  .figure p { text-align: center; }\n  .inlinetask {\n    padding: 10px;\n    border: 2px solid gray;\n    margin: 10px;\n    background: #ffffcc;\n  }\n  #org-div-home-and-up\n   { text-align: right; font-size: 70%; white-space: nowrap; }\n  textarea { overflow-x: auto; }\n  .linenr { font-size: smaller }\n  .code-highlighted { background-color: #ffff00; }\n  .org-info-js_info-navigation { border-style: none; }\n  #org-info-js_console-label\n    { font-size: 10px; font-weight: bold; white-space: nowrap; }\n  .org-info-js_search-highlight\n    { background-color: #ffff00; color: #000000; font-weight: bold; }\n  .ulClassNameOrID > li {}\n  /*]]>*/-->\n</style>")
  (csetq org-html-table-default-attributes '(:border "2" :cellspacing "0" :cellpadding "6"))
  (csetq org-html-postamble t))
;;;_ ** flyspell
(use-package flyspell
 :diminish flyspell-mode
 :commands (flyspell-mode flyspell-prog-mode)
 :config
 (add-to-list 'flyspell-dictionaries-that-consider-dash-as-word-delimiter "german-new8")
 (csetq flyspell-issue-welcome-flag nil)
 ;; M-Tab is owned by the window manager, correct with C-M-i
 (csetq flyspell-use-meta-tab nil)
 ;; Flyspell hijacked C-., which I want to use for tags
 (define-key flyspell-mode-map [(control ?\.)] nil)
 )
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'org-mode-hook 'flyspell-mode)
(add-hook 'latex-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
;;;_ *** helm-flyslepp
;; https://github.com/pronobis/helm-flyspell
(use-package helm-flyspell
  :commands helm-flyspell-correct
  :config
  (bind-key "C-;" 'helm-flyspell-correct flyspell-mode-map)
  )
;;;_ * Programming
;;;_ ** Tab handling
(use-package tabify
  :commands (tabify untabify)
  :config
  ;; Tabify only initial whitespace
  (setq tabify-regexp "^\t* [ \t]+"))

;; Deleting past a tab normally changes tab into spaces. Don't do that,
;; kill the tab instead.
(csetq backward-delete-char-untabify-method nil)
;;;_ ** Disable vc backends
;; We only use git, not other version controls:
(setq vc-handled-backends nil)
;;;_ ** Let parenthesis behave
(show-paren-mode 1)
(setq show-paren-delay 0
      blink-matching-parent nil)
;;;_ ** qmake project files
;; Don't open Qt's *.pro files as IDLWAVE files.
;; TODO: look for a real qmake-mode
(add-to-list 'auto-mode-alist '("\\.pro$" . fundamental-mode))
;;;_ ** Commenting
(bind-key "C-c c" 'comment-dwim)
;;;_ ** Compilation
;; set initial compile-command to nothing, so that F7 will prompt for one
(csetq compile-command nil)


(defvar compile-commands nil
  "The compile commands are an alist where the key is
   is the command and the value is the time when it was
   executed the last time. The latter is used for sorting.

   Example:

   '((\"make\" .  \"1448748904\")
     (\"make -C ~/test\" . \"1448748866\"))")


;; automatically save our compile-commands
(add-to-list 'savehist-minibuffer-history-variables 'compile-commands)


(defun comp--sort-command-alist ()
  "Sorts compile-commands by the value of their cons elements.
   This sorts the entries so that recently used compile commands
   are near the top."

  (setq compile-commands (sort compile-commands (lambda (x y)
							(not (string< (cdr x) (cdr y)))))))

(defun comp--add-command (cmd)
  "Adds a command to compile-commands if it isn't already in it.

  It inserts the seconds since 1970 into the value."

  ;; (message "adding command '%s'" cmd)
  (unless (assoc cmd compile-commands)
    (add-to-list 'compile-commands (cons cmd (format-time-string "%s")))))


(defun comp--get-compile-commands-from-buffers ()
  "Searches all open buffers that have a file-name associated
   and adds compile commands from to compile-commands. Valid
   forms for compile commands in the source code are:

   // @command: make
   ## @command: make
   /* @command: make */
   (setq compile-command \"make\")"

  ;; (message "loading commands from buffers")
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (buffer-file-name)
	(save-excursion
	  (goto-char (point-min))
	  (while (re-search-forward "^.. @compile: \\(.*\\)$" nil t)
	    (let ((s (match-string-no-properties 1)))
	      ;; \s- whitespace character class
	      (setq s (replace-regexp-in-string "\s-*\\*/$" "" s))
	      (comp--add-command s)))
	  (goto-char (point-min))
	  (while (re-search-forward "(setq compile-command \"\\(.*\\)\")" nil t)
	    ;; (message "via setq '%s'" (match-string-no-properties 1))
	    (comp--add-command (match-string-no-properties 1))))))))


(defun comp--delete-command (candidate)
  "This deletes the selected compile-command from compile-commands."

  (assq-delete-all candidate compile-commands)
  (setq compile-commands (delq (assoc candidate compile-commands) compile-commands))
  ;; (message "deleted: %s" candidate)
  nil)


(defun comp--persistent-action (candidate)
  (with-selected-window (minibuffer-window)
	(delete-minibuffer-contents)
	(set-text-properties 0 (length candidate) nil candidate)
	(insert candidate)))


;; http://wikemacs.org/wiki/How_to_write_helm_extensions
(defun comp--helm-compile-command (arg)
  "Interactively select a compile-command.

   Releads possible commends from open buffers when run with argument."

  (interactive "P")
  (unless compile-commands
    (setq arg 1))
  (when arg
    (comp--get-compile-commands-from-buffers))
  (comp--sort-command-alist)
  ;; compile-commands is now something like:
  ;; '(("make -C foo" . 1) ("ccmake && make" . 2))
  ;; (message "commands: %s" compile-commands)
  (let* ((src (helm-build-sync-source "Select compile command"
		:candidates (mapcar 'car compile-commands)
		:action '(("Select" . identity)
			  ("Delete" . comp--delete-command))
		:mode-line "F1 select, F2 delete"
		:persistent-action 'comp--persistent-action
		;; :volatile nil
		))
	 (ent (helm-build-dummy-source "Or enter new compile command ..."
		:mode-line ""
		))
	 (cmd (helm :sources '(src ent)
		    :prompt "cmd: "
		    :buffer "*compile-command*"
		    )))
    (when cmd
      (message "command: %s" cmd)
      (comp--add-command cmd)
      (setq compile-command cmd)
      )))
;; (progn (set-compile-command) compile-command)
(bind-key "S-<f7>" 'comp--helm-compile-command)


(defun comp--compile ()
  (interactive)
  (delete-other-windows)
  (save-buffer)
  (unless compile-command
    (comp--helm-compile-command nil))
  (when compile-command
    (message "compile command: %s" compile-command)
    (let ((cmd (assoc compile-command compile-commands)))
      (when cmd
	(message "assoc: %s" (assoc compile-command compile-commands))
	(setcdr cmd (format-time-string "%s"))
	(message "assoc: %s" (assoc compile-command compile-commands))
	)))
    (compile compile-command))
(bind-key "<f7>" 'comp--compile)





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

;;;_ *** Error navigation
(bind-key "<f8>" 'next-error)
(bind-key "S-<f8>" 'previous-error)
;;;_ ** Automatically make files with shebang executable
(add-hook 'after-save-hook
	  'executable-make-buffer-file-executable-if-script-p)
;;;_ ** Mode: C, C++
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
;;;_ ** Mode: Ediff
;; http://oremacs.com/2015/01/17/setting-up-ediff/
(use-package ediff
  :config
  (csetq ediff-window-setup-function 'ediff-setup-windows-plain)
  (csetq ediff-split-window-function 'split-window-horizontally)
  (csetq ediff-diff-options "-w")
  (defun my--ediff-hook ()
    (ediff-setup-keymap)
    (bind-key "j" 'ediff-next-difference ediff-mode-map)
    (bind-key "k" 'ediff-previous-difference ediff-mode-map))
  (add-hook 'ediff-mode-hook 'my--ediff-hook)
  (add-hook 'ediff-after-quit-hook-internal 'winner-undo))
;;;_ ** Mode: ELisp
(defun my--elisp-setup ()
  ;; Setup imenu TODO
  (add-to-list 'imenu-generic-expression '(""  "^;;;_ \\(.*\\)" 1) t)
  ;; automatically give help about function syntax
  (eldoc-mode t)
  ;; "-" is almost always part of a function- or variable-name
  (modify-syntax-entry ?- "w")
;; TODO
;; Compile Emacs Lisp source files after the visiting buffers are saved.
;; (unless (string= (buffer-name) "*scratch*")
;;    (auto-compile-mode 1))
  )
(add-hook 'emacs-lisp-mode-hook 'my--elisp-setup)
;;;_ ** Mode: Markdown
(use-package markdown-mode
  :ensure t
  :mode (("\\.md\\'"       . markdown-mode)
	 ("\\.markdown\\'" . markdown-mode)))
;;;_ ** Mode: Python
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
;;;_ ** Mode: Rust
(use-package rust-mode
  :mode (("\\.rs\\'" . rust-mode)))
;;;_ ** Mode: Shell
(defun my-shell-tab-setup ()
  (interactive)
  (csetq indent-tabs-mode t)
  (csetq tab-width 4)
  (csetq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84))
  (bind-key "C-i" 'self-insert-command text-mode-map))
(add-hook 'shell-mode-hook 'my-shell-tab-setup)
(add-hook 'sh-mode-hook    'my-shell-tab-setup)
;;;_ ** Mode: web-mode
;; Home page: http://web-mode.org/
(use-package web-mode
  :ensure t
  :commands web-mode
  :mode (("\\.html\\'" . web-mode)
	 ("\\.css\\'" . web-mode)
	 ("\\.json\\'" . web-mode)
	 ("\\.js\\'" . web-mode)
	 )
  :config
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
  (add-hook 'web-mode-hook 'my-web-mode-hook)
  (setq web-mode-enable-block-partial-invalidation t
	web-mode-engines-alist '(("ctemplate" . "\\.html$"))))
;;;_ ** Package: column-marker
(defun my--column-marker-at-80 ()
  (interactive)
  (column-marker-2 80))
(use-package column-marker
  :ensure t
  :commands (column-marker-1 column-marker-2)
  :init
  (add-hook 'c-mode-hook 'my--column-marker-at-80)
  )
;;;_ ** Package: auto-compile
(use-package auto-compile
  :ensure t
  :config
  (auto-compile-on-load-mode 1)
  (auto-compile-on-save-mode 1)
  )
;;;_ ** Package: eshell
;; https://www.masteringemacs.org/article/complete-guide-mastering-eshell
(use-package eshell
  :defer t
  :bind ("C-c e" . eshell)
  :config
  ;; If I ever want my own eshell/foo commands overwrite real commands ...
  (setq eshell-prefer-lisp-functions t)

  ;; check if this is ok for my usage
  ;; eshell-visual-commands

  (add-hook 'eshell-mode-hook 'my--hide-trailing-whitespace)
  )
;;;_ ** Package: magit
;; Must be set before magit is loaded. It will remove the new key
;; bindings that use pop-up buffers.
(setq magit-rigid-key-bindings t)
(use-package magit
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
;;;_ ** Package: magit-timemachine
(use-package git-timemachine
  :commands git-timemachine
  )
;;;_ * Emacs server
(require 'server)
(add-hook 'server-switch-hook 'raise-frame)

;; Disable prompt asking you if you want to kill a
;; buffer with a live process attached to it.
;; http://stackoverflow.com/questions/268088/how-to-remove-the-prompt-for-killing-emacsclient-buffers
(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)

(unless (server-running-p)
  (server-start))
;;;_ * Local file variables
;; Local Variables:
;; orgstruct-heading-prefix-regexp: ";;;_ "
;; eval: (orgstruct++-mode)
;; End:
