;;; helm-compile.el --- use helm with your compile commands           -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Holger Schurig

;; Author: Holger Schurig <holgerschurig@gmail.com>
;; Keywords: development, compile, helm
;; Version: 0.0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 2 of the License

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package helps you in maintaining different compilation commands.
;;
;; Emacs itself comes with one compilation command preset: "make -k ".
;; Switching to another one is cumbersome, and therefore often not
;; done on-the-fly.
;;
;; However, this module uses helm to prompt you for compile commands
;; and keeps a history of previous used compile commands. Being helm,
;; this allows you to easily search through your history. There's also
;; an helm action defined so that you
;;
;; @compile: (byte-compile-file "helm-compile.el")

;;; Code:


(require 'helm-source)    ;; helm-build-sync-source
(require 'magit-git)      ;; for magit-toplevel
(require 'magit-process)  ;; for magit-process-file


;; Normally this is "make -k" and that's quite useless
(when (string= compile-command "make -k ")
  (setq compile-command ""))




(defvar helm-compile-commands nil
  "List of previous compilation commands.

The compile commands are an alist where the key is
is the command and the value is the time when it was
executed the last time.  The latter is used for sorting.

 Example:

'((\"make\" .  \"1448748904\")
  (\"make -C ~/test\" . \"1448748866\"))")


;; automatically save our helm-compile-commands
(defvar savehist-minibuffer-history-variables)
(add-to-list 'savehist-minibuffer-history-variables 'helm-compile-commands)


(defun helm-compile-sort-command-alist ()
  "Sort ‘helm-compile-commands’ by the value of their cons elements.

This sorts the entries so that recently used compile commands
are near the top."
  (setq helm-compile-commands (sort helm-compile-commands (lambda (x y)
												  (not (string< (cdr x) (cdr y)))))))

(defun helm-compile-add-command (cmd)
  "Add CMD to ‘helm-compile-commands’ if it isn't already in it.

It inserts the seconds since 1970 into the value."
  ;; (message "adding command '%s'" cmd)
  (unless (string= "" cmd)
	(unless (assoc cmd helm-compile-commands)
	  (add-to-list 'helm-compile-commands (cons cmd (format-time-string "%s"))))))


(defun helm-compile-del-command (cmd)
  "This deletes CMD from the ‘helm-compile-commands’ list."
  (setq helm-compile-commands
		(delq (assoc cmd helm-compile-commands)
			  helm-compile-commands)))


(defun helm-compile-get-helm-commands-from-buffers ()
  "Get compilation commands from open buffers.

Searches all open buffers that have a file-name associated and
adds compile commands from to ‘helm-compile-commands’.  Valid forms
for compile commands in the source code are:

- // @compile: make foo
- ## @compile: make bar
- /* @compile: make foobar
- ;; @compile: (byte-compile-file \"foo.el\")
- (setq compile-command \"make\")

Or, in words: on two characters at the beginning of a line,
followed by ' @compile: ', followed by the compilation command.

As a special case for C commands, remove ' */' at the end of the
compilation command.

As a special case for elisp, also consider '(setq compile-command
\"foo\"), this doesn't need to be at the start of the line."
  (save-excursion
	(goto-char (point-min))
	(while (re-search-forward "^..? @compile: \\(.*\\)$" nil t)
	  (let ((s (match-string-no-properties 1)))
		;; \s- whitespace character class
		;; (message "FOUND '%s'" s)
		(setq s (replace-regexp-in-string "\s-*\\*/$" "" s))
		(helm-compile-del-command s)
		(helm-compile-add-command s)))
	(goto-char (point-min))
	(while (re-search-forward "(setq compile-command \"\\(.*\\)\")" nil t)
	  (let ((s (match-string-no-properties 1)))
		;; (message "via setq '%s'" s)
		(helm-compile-del-command s)
		(helm-compile-add-command s)))))


(defun helm-compile-default-action (cmd &optional UNUSED1 UNUSED2)
  "Default action that execute CMD.

UNUSED1 and UNUSED2 are just there to comply with the internal helm API"
  ;; (message "helm-compile-default-action: '%s'" cmd)
  (when cmd
  	(helm-compile-add-command cmd)
  	(setq compile-command cmd)
	(helm-compile)))


(defun helm-compile-del-action (cmd)
  "Action that deletes CMD from the list of commands."
  ;; (message "helm-compile-del-action: '%s'" cmd)
  (helm-compile-del-command cmd)
  (when (string= compile-command cmd)
	(setq compile-command nil)))


(defun helm-select-compile-command ()
  "Interactively select a compilation command.

Relead possible commands from open buffers when run with argument."

  (interactive)

  ;; Always get list of compilation commands and sort them
  (helm-compile-get-helm-commands-from-buffers)
  (helm-compile-sort-command-alist)
  ;; helm-compile-commands is now something like:
  ;; '(("make -C foo" . 1) ("ccmake && make" . 2))

  (let ((cmds (helm-build-sync-source "Select compile command"
				:candidates (delq "" (mapcar 'car helm-compile-commands))
				:action '(("Run"    . helm-compile-default-action)
						  ("Delete" . helm-compile-del-action))
				:mode-line "F1 run, F2 delete"
				))
		(dummy (helm-build-dummy-source "Or enter new command ..."
				 :mode-line ""
				 :action #'helm-compile-default-action)))
	(helm :sources '(cmds dummy)
		  :buffer "*helm compile command*"
		  :prompt "cmd: ")))


(defun helm-compile ()
  "Start a compilation.

If we haven't yet defined a compile command, a new one will be
selected with helm's help."
  (interactive)
  (delete-other-windows)
  (save-buffer)
  (if (string= compile-command "")
	  (helm-select-compile-command)
	;; (message "compile command: %s" compile-command)
	(let ((cmd (assoc compile-command helm-compile-commands)))
	  (when cmd
		;; (message "assoc: %s" (assoc compile-command helm-compile-commands))
		(setcdr cmd (format-time-string "%s"))
		;; (message "assoc: %s" (assoc compile-command helm-compile-commands))
		))
	;; (message "compile command: %s" compile-command)
	(setq compile-command (replace-regexp-in-string
						   " \\(%\\) "
						   (buffer-file-name (window-buffer))
						   compile-command
						   nil nil 1))
	;; (message "compile command: %s" compile-command)
	(if (string= (substring compile-command 0 1) "(")
		(eval (car (read-from-string compile-command)))
	  (progn
		(let ((default-directory (or (magit-toplevel) default-directory)))
		  (compile compile-command))))))




(provide 'helm-compile)
;;; helm-compile.el ends here
