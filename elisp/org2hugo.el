;;; org2hugo.el --- export org to hugo    -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Holger Schurig

;; Author: Holger Schurig <holgerschurig@gmail.com>
;; Maintainer: Holger Schurig <holgerschurig@gmail.com>
;; URL: TODO
;; Keywords: org, hugo, blogging
;; Version: 0.0.1
;; Package-Requires: ((dash "20160820.501") (org "8.3.4"))

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

;; TODO
;;
;; The following line is for my helm-compile code:
;; @compile: (byte-compile-file "org2hugo.el")

;;; Code:
;;* Requires
(require 'org)
(require 'dash)  ;; for -first

;;* Customize
(defgroup org2hugo nil
  "Blogging from org to hugo."
  :group 'foo
  :prefix "org2hugo-")

(defcustom org2hugo-content-dir "~/www.hugo/content/"
  "Path to Hugo's content directory."
  :type 'string
  :group 'org2hugo)


;;* Implementation
(defun org2hugo-ensure-property (property)
  "Make sure that PROPERTY exists.

If not, it will be created.

Returns the property name if the property has been created,
otherwise nil."
  (if (org-entry-get nil property)
	  nil
	(progn (org-entry-put nil property "")
		   property)))


(defun org2hugo-ensure-properties ()
  "Ensure that all needed properties exist.

This ensures that several properties exists.  If not, these
properties will be created in an empty form.  In this case, the
drawer will also be opened and the cursor will be positioned
at the first element that needs to be filled.

Returns list of properties that still must be filled in"
  (require 'dash)
  (let ((current-time (format-time-string (org-time-stamp-format t t) (org-current-time)))
		first)
	(save-excursion
	  (unless (org-entry-get nil "TITLE")
		(org-entry-put nil "TITLE" (nth 4 (org-heading-components))))
	  (setq first (-first #'identity (mapcar #'org2hugo-ensure-property '("HUGO_TAGS" "HUGO_TOPICS" "HUGO_FILE"))))
	  (unless (org-entry-get nil "HUGO_DATE")
		(org-entry-put nil "HUGO_DATE" current-time)))
	(when first
	  (goto-char (org-entry-beginning-position))
	  ;; The following opens the drawer
	  (forward-line 1)
	  (beginning-of-line 1)
	  (when (looking-at org-drawer-regexp)
		(org-flag-drawer nil))
	  ;; And now move to the drawer property
	  (search-forward (concat ":" first ":"))
	  (end-of-line))
	first))
;; This is GPLv2. If you still don't know the details, read
;; http://www.gnu.org/licenses/old-licenses/gpl-2.0.en.html

(defun org2hugo ()
  "Export a org mode section towards the blogging software \"hugo\"."
  (interactive)
  (unless (org2hugo-ensure-properties)
	(let* ((title    (concat "title = \"" (org-entry-get nil "TITLE") "\"\n"))
		   (date     (concat "date = \"" (format-time-string "%Y-%m-%d" (apply 'encode-time (org-parse-time-string (org-entry-get nil "HUGO_DATE"))) t) "\"\n"))
		   (topics   (concat "topics = [ \"" (mapconcat 'identity (split-string (org-entry-get nil "HUGO_TOPICS") "\\( *, *\\)" t) "\", \"") "\" ]\n"))
		   (tags     (concat "tags = [ \"" (mapconcat 'identity (split-string (org-entry-get nil "HUGO_TAGS") "\\( *, *\\)" t) "\", \"") "\" ]\n"))
		   (fm (concat "+++\n"
					   title
					   date
					   tags
					   topics
					   "+++\n\n"))
		   (file     (org-entry-get nil "HUGO_FILE"))
		   (coding-system-for-write buffer-file-coding-system)
		   (backend  'md)
		   (blog))
	  ;; try to load org-mode/contrib/lisp/ox-gfm.el and use it as backend
	  (if (require 'ox-gfm nil t)
		  (setq backend 'gfm)
		(require 'ox-md))
	  (setq blog (org-export-as backend t))
	  ;; Normalize save file path
	  (unless (string-match "^[/~]" file)
		(setq file (concat org2hugo-content-dir file))
	  (unless (string-match "\\.md$" file)
		(setq file (concat file ".md")))
	  ;; save markdown
	  (with-temp-buffer
		(insert fm)
		(insert blog)
		(untabify (point-min) (point-max))
		(write-file file)
		(message "Exported to %s" file))
	  ))))


(provide 'org2hugo)
;;; org2hugo.el ends here
