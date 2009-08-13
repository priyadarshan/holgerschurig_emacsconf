(defgroup wsmm-mode nil
  "WordStar minor-mode."
  :version "23.1"
  :group 'emulation)

(require 'advice)




;;; Begin/End of block & higthlighting

(defvar wsmm--block-overlay nil
  "Overlay to show the block.")
(make-variable-buffer-local 'wsmm--block-overlay)

(defface wsmm-block-face
  '((t :inherit highlight))
  "Face used for the current block."  :group 'wsmm-mode
)

(defcustom wsmm-block-face 'wsmm-mode
  "Face with which to mark the current block in WordStar minor-mode."
  :type 'face
  :group 'wsmm-mode
  :set (lambda (symbol value)
         (set symbol value)
         (dolist (buffer (buffer-list))
           (with-current-buffer buffer
             (when wsmm--block-overlay
               (overlay-put wsmm--block-overlay 'face wsmm-block-face))))))

(defvar wsmm--marker-block-begin nil
  "Beginning of \"Block\" in WordStar mode.")
(make-variable-buffer-local 'wsmm--marker-block-begin)

(defvar wsmm--marker-block-end nil
  "End of \"Block\" in WordStar mode.")
(make-variable-buffer-local 'wsmm--marker-block-end)

(defun wsmm--block-show ()
  "Marks a block by defining an overlay with it's own face."
  (when (and wsmm--marker-block-begin wsmm--marker-block-end)
    (when wsmm--block-overlay
      (delete-overlay wsmm--block-overlay))
    (setq wsmm--block-overlay (make-overlay wsmm--marker-block-begin wsmm--marker-block-end))
    (overlay-put wsmm--block-overlay 'face 'wsmm-block-face)
  ))

(defun wsmm--block-hide ()
  "Hides a block by deleting it's overlay."
  (when wsmm--block-overlay
    (delete-overlay wsmm--block-overlay)
    (setq wsmm--block-overlay nil)))

(defun wsmm-set-block-begin ()
  "In WordStar mode: Set block begin marker to current cursor position."
  (interactive)
  (setq wsmm--marker-block-begin (point-marker))
  (wsmm--block-show))

(defun wsmm-set-block-end ()
  "In WordStar mode: Set block end marker to current cursor position."
  (interactive)
  (setq wsmm--marker-block-end (point-marker))
  (wsmm--block-show))

(defun wsmm-toggle-markers ()
  "In WordStar mode: Hide/show block markers."
  (interactive)
  (if wsmm--block-overlay
      (wsmm--block-hide)
    (wsmm--block-show)))

(defun wsmm-word-mark ()
  "In WordStar mode: Mark current word as block."
  (interactive)
  (save-excursion
    (forward-word 1)
    (wsmm-set-block-end)
    (forward-word -1)
    (wsmm-set-block-begin)))



;; Block operations

(defun wsmm-block-indent ()
  "In WordStar mode: Indent block according to major mode."
  (interactive)
  (if (and wsmm--marker-block-begin wsmm--marker-block-end)
      (indent-region wsmm--marker-block-begin wsmm--marker-block-end)
    (message "No block marked")))

(defun wsmm-block-unindent ()
  "TODO: should de-indent a block:"
  (interactive)
  (if (and wsmm--marker-block-begin wsmm--marker-block-end)
      (save-excursion
	(goto-char wsmm--marker-block-begin)
	(indent-rigidly wsmm--marker-block-begin wsmm--marker-block-end -1))
    (message "No block marked")))

(defun wsmm-block-write ()
  "In WordStar mode: Write block to file."
  (interactive)
  (if (and wsmm--marker-block-begin wsmm--marker-block-end)
      (let ((filename (read-file-name "Write block to file: ")))
	(write-region wsmm--marker-block-begin wsmm--marker-block-end filename))
    (message (cond (wsmm--marker-block-begin "Block end marker not set")
		    (wsmm--marker-block-end "Block begin marker not set")
		    (t "Block markers not set")))))

(defun wsmm-block-delete ()
  "In WordStar mode: Delete block."
  (interactive)
  (if (and wsmm--marker-block-begin wsmm--marker-block-end)
      (let ()
	(kill-region wsmm--marker-block-begin wsmm--marker-block-end)
	(setq wsmm--marker-block-end nil)
	(setq wsmm--marker-block-begin nil)
	(wsmm--block-hide))
    (message (cond (wsmm--marker-block-begin "Block end marker not set")
		    (wsmm--marker-block-end "Block begin marker not set")
		    (t "Block markers not set")))))

(defun wsmm-block-copy ()
  "In WordStar mode: Copy block to current cursor position."
  (interactive)
  (if (and wsmm--marker-block-begin wsmm--marker-block-end)
      (progn
	(copy-region-as-kill wsmm--marker-block-begin wsmm--marker-block-end)
	(yank))
    (message (cond (wsmm--marker-block-begin "Block end marker not set")
		   (wsmm--marker-block-end "Block begin marker not set")
		   (t "Block markers not set")))))

(defun wsmm-block-move ()
  "In WordStar mode: Move block to current cursor position."
  (interactive)
  (if (and wsmm--marker-block-begin wsmm--marker-block-end)
      (progn
        (kill-region wsmm--marker-block-begin wsmm--marker-block-end)
        (yank)
	(setq wsmm--marker-block-begin (region-beginning))
	(setq wsmm--marker-block-end (region-end))
	(wsmm--block-show))
))



;;; Cursor moving

(defvar wsmm--marker-last-cursorpos nil)

(defun wsmm-goto-block-begin ()
  "In WordStar mode: Go to block begin marker."
  (interactive)
  (if wsmm--marker-block-begin
      (progn
	(setq wsmm--marker-last-cursorpos (point-marker))
	(goto-char wsmm--marker-block-begin))
    (message "Block begin marker not set")))

(defun wsmm-goto-block-end ()
  "In WordStar mode: Go to block end marker."
  (interactive)
  (if wsmm--marker-block-end
      (progn
	(setq wsmm--marker-last-cursorpos (point-marker))
	(goto-char wsmm--marker-block-end))
    (message "Block end marker not set")))

(defun wsmm-goto-last-cursorposition ()
  "In WordStar mode: "
  (interactive)
  (if wsmm--marker-last-cursorpos
    (goto-char wsmm--marker-last-cursorpos)
    (message "No last cursor position available.")))

(defun wsmm-goto-window-begin ()
  "Go to the first line of the displayed window."
  (interactive)
  (setq wsmm--marker-last-cursorpos (point-marker))
  (goto-char (window-start)))

(defun wsmm-goto-window-end ()
  "Go to the last line of the displayed window."
  (interactive)
  (setq wsmm--marker-last-cursorpos (point-marker))
  ;(message "goto %s" (- (window-end) 1))
  (goto-char (- (window-end) 1))
  (beginning-of-line)
  )


;; Screen scrolling

(defun wsmm-scroll-up ()
  "Scrolls one line up, keeping the cursor in the current row."
  (interactive)
  (scroll-up 1)
  (forward-line 1))

(defun wsmm-scroll-down ()
  "Scrolls one line down, keeping the cursor in the current row."
  (interactive)
  (forward-line -1)
  (scroll-down 1))



;;; Deleting

(defun wsmm-kill-eol ()
  "In WordStar mode: Kill to end of line (like WordStar, not like Emacs)."
  (interactive)
  (let ((p (point)))
    (end-of-line)
    (kill-region p (point))))



;;; Positions
;;
;; Remembering positions with "C-k 0" ... "C-k 9"
;; Restoring positions with   "C-q 0" ... "C-q 9"

(defvar wsmm--marker-0 nil)
(defvar wsmm--marker-1 nil)
(defvar wsmm--marker-2 nil)
(defvar wsmm--marker-3 nil)
(defvar wsmm--marker-4 nil)
(defvar wsmm--marker-5 nil)
(defvar wsmm--marker-6 nil)
(defvar wsmm--marker-7 nil)
(defvar wsmm--marker-8 nil)
(defvar wsmm--marker-9 nil)

(defun wsmm-set-marker-0 ()
  "In WordStar mode: Set marker 0 to current cursor position."
  (interactive)
  (setq wsmm--marker-0 (point-marker))
  (message "Marker 0 set"))

(defun wsmm-set-marker-1 ()
  "In WordStar mode: Set marker 1 to current cursor position."
  (interactive)
  (setq wsmm--marker-1 (point-marker))
  (message "Marker 1 set"))

(defun wsmm-set-marker-2 ()
  "In WordStar mode: Set marker 2 to current cursor position."
  (interactive)
  (setq wsmm--marker-2 (point-marker))
  (message "Marker 2 set"))

(defun wsmm-set-marker-3 ()
  "In WordStar mode: Set marker 3 to current cursor position."
  (interactive)
  (setq wsmm--marker-3 (point-marker))
  (message "Marker 3 set"))

(defun wsmm-set-marker-4 ()
  "In WordStar mode: Set marker 4 to current cursor position."
  (interactive)
  (setq wsmm--marker-4 (point-marker))
  (message "Marker 4 set"))

(defun wsmm-set-marker-5 ()
  "In WordStar mode: Set marker 5 to current cursor position."
  (interactive)
  (setq wsmm--marker-5 (point-marker))
  (message "Marker 5 set"))

(defun wsmm-set-marker-6 ()
  "In WordStar mode: Set marker 6 to current cursor position."
  (interactive)
  (setq wsmm--marker-6 (point-marker))
  (message "Marker 6 set"))

(defun wsmm-set-marker-7 ()
  "In WordStar mode: Set marker 7 to current cursor position."
  (interactive)
  (setq wsmm--marker-7 (point-marker))
  (message "Marker 7 set"))

(defun wsmm-set-marker-8 ()
  "In WordStar mode: Set marker 8 to current cursor position."
  (interactive)
  (setq wsmm--marker-8 (point-marker))
  (message "Marker 8 set"))

(defun wsmm-set-marker-9 ()
  "In WordStar mode: Set marker 9 to current cursor position."
  (interactive)
  (setq wsmm--marker-9 (point-marker))
  (message "Marker 9 set"))

(defun wsmm-find-marker-0 ()
  "In WordStar mode: Go to marker 0."
  (interactive)
  (if wsmm--marker-0
      (let ()
	(setq wsmm--marker-last-cursorpos (point-marker))
	(goto-char wsmm--marker-0))
    (message "Marker 0 not set")))

(defun wsmm-find-marker-1 ()
  "In WordStar mode: Go to marker 1."
  (interactive)
  (if wsmm--marker-1
      (let ()
	(setq wsmm--marker-last-cursorpos (point-marker))
	(goto-char wsmm--marker-1))
    (message "Marker 1 not set")))

(defun wsmm-find-marker-2 ()
  "In WordStar mode: Go to marker 2."
  (interactive)
  (if wsmm--marker-2
      (let ()
	(setq wsmm--marker-last-cursorpos (point-marker))
	(goto-char wsmm--marker-2))
    (message "Marker 2 not set")))

(defun wsmm-find-marker-3 ()
  "In WordStar mode: Go to marker 3."
  (interactive)
  (if wsmm--marker-3
      (let ()
	(setq wsmm--marker-last-cursorpos (point-marker))
	(goto-char wsmm--marker-3))
    (message "Marker 3 not set")))

(defun wsmm-find-marker-4 ()
  "In WordStar mode: Go to marker 4."
  (interactive)
  (if wsmm--marker-4
      (let ()
	(setq wsmm--marker-last-cursorpos (point-marker))
	(goto-char wsmm--marker-4))
    (message "Marker 4 not set")))

(defun wsmm-find-marker-5 ()
  "In WordStar mode: Go to marker 5."
  (interactive)
  (if wsmm--marker-5
      (let ()
	(setq wsmm--marker-last-cursorpos (point-marker))
	(goto-char wsmm--marker-5))
    (message "Marker 5 not set")))

(defun wsmm-find-marker-6 ()
  "In WordStar mode: Go to marker 6."
  (interactive)
  (if wsmm--marker-6
      (let ()
	(setq wsmm--marker-last-cursorpos (point-marker))
	(goto-char wsmm--marker-6))
    (message "Marker 6 not set")))

(defun wsmm-find-marker-7 ()
  "In WordStar mode: Go to marker 7."
  (interactive)
  (if wsmm--marker-7
      (let ()
	(setq wsmm--marker-last-cursorpos (point-marker))
	(goto-char wsmm--marker-7))
    (message "Marker 7 not set")))

(defun wsmm-find-marker-8 ()
  "In WordStar mode: Go to marker 8."
  (interactive)
  (if wsmm--marker-8
      (let ()
	(setq wsmm--marker-last-cursorpos (point-marker))
	(goto-char wsmm--marker-8))
    (message "Marker 8 not set")))

(defun wsmm-find-marker-9 ()
  "In WordStar mode: Go to marker 9."
  (interactive)
  (if wsmm--marker-9
      (let ()
	(setq wsmm--marker-last-cursorpos (point-marker))
	(goto-char wsmm--marker-9))
    (message "Marker 9 not set")))



;;; Buffer related

(defun wsmm-save-kill-buffer ()
  (interactive)
  (save-buffer 0)
  (kill-buffer))
(defun wsmm-kill-buffer ()
  (interactive)
  (kill-buffer))


;;; Test code

(defun wsmm-x ()
  (interactive)
  (setq wsmm--marker-block-begin 5)
  (setq wsmm--marker-block-end 10)
  (wsmm--block-show)
)
;;(global-set-key "\C-t" 'wsmm-x)



;;; Keymap handling

(defvar wsmm--keymap-initialized nil)

(defvar wsmm-keymap (make-sparse-keymap)
  "Keymap for WordStar minor-mode.")
(defvar wsmm-k-keymap (make-sparse-keymap)
  "Keymap for WordStar minor-mode (commands beginning with C-k).")
(defvar wsmm-q-keymap (make-sparse-keymap)
  "Keymap for WordStar minor-mode (commands beginning with C-q).")

(defun wsmm--init-keymap ()
  (define-key wsmm-keymap "\C-a" 'backward-word)           ; my-home
  (define-key wsmm-keymap "\C-b" 'fill-paragraph)
  (define-key wsmm-keymap "\C-c" 'scroll-up)               ; ctl-c-prefix
  (define-key wsmm-keymap "\C-d" 'forward-char)
  (define-key wsmm-keymap "\C-e" 'previous-line)
  (define-key wsmm-keymap "\C-f" 'forward-word)
  (define-key wsmm-keymap "\C-g" 'delete-char)
  (define-key wsmm-keymap "\C-h" 'delete-backward-char)    ; ctl-h-prefix
  ;; ^i TAB
  (define-key wsmm-keymap "\C-j" help-map)                 ; newline-and-indent
  (define-key wsmm-keymap "\C-k" wsmm-k-keymap)
  (define-key wsmm-keymap "\C-l" ctl-x-map)                ; recenter-top-bottom
  ;; ^m RET
  (define-key wsmm-keymap "\C-n" 'open-line)
  ;;(define-key wsmm-keymap "\C-o" wsmm-ctl-o-map)
  (define-key wsmm-keymap "\C-p" 'quoted-insert)
  (define-key wsmm-keymap "\C-q" wsmm-q-keymap)
  (define-key wsmm-keymap "\C-r" 'scroll-down)             ; isearch-backward
  (define-key wsmm-keymap "\C-s" 'backward-char)           ; isearch-forward
  (define-key wsmm-keymap "\C-t" 'kill-word)               ; transpose-chars
  (define-key wsmm-keymap "\C-u" 'keyboard-quit)
  (define-key wsmm-keymap "\C-v" 'overwrite-mode)
  (define-key wsmm-keymap "\C-w" 'wsmm-scroll-down)          ; kill-region
  (define-key wsmm-keymap "\C-x" 'next-line)
  (define-key wsmm-keymap "\C-y" 'wsmm-scroll-up)            ; yank
  (define-key wsmm-keymap "\C-z" 'kill-whole-line)

  (define-key wsmm-keymap [delete] 'delete-char)

  (define-key wsmm-q-keymap "0"    'wsmm-find-marker-0)
  (define-key wsmm-q-keymap "1"    'wsmm-find-marker-1)
  (define-key wsmm-q-keymap "2"    'wsmm-find-marker-2)
  (define-key wsmm-q-keymap "3"    'wsmm-find-marker-3)
  (define-key wsmm-q-keymap "4"    'wsmm-find-marker-4)
  (define-key wsmm-q-keymap "5"    'wsmm-find-marker-5)
  (define-key wsmm-q-keymap "6"    'wsmm-find-marker-6)
  (define-key wsmm-q-keymap "7"    'wsmm-find-marker-7)
  (define-key wsmm-q-keymap "8"    'wsmm-find-marker-8)
  (define-key wsmm-q-keymap "9"    'wsmm-find-marker-9)
  (define-key wsmm-q-keymap "a"    'replace-string)
  (define-key wsmm-q-keymap "\C-a" 'replace-string)
  (define-key wsmm-q-keymap "b"    'wsmm-goto-block-begin)
  (define-key wsmm-q-keymap "\C-b" 'wsmm-goto-block-begin)
  (define-key wsmm-q-keymap "c"    'end-of-buffer)
  (define-key wsmm-q-keymap "\C-c" 'end-of-buffer)
  (define-key wsmm-q-keymap "d"    'end-of-line)
  (define-key wsmm-q-keymap "\C-d" 'end-of-line)
  (define-key wsmm-q-keymap "e"    'wsmm-goto-window-begin)
  (define-key wsmm-q-keymap "\C-e" 'wsmm-goto-window-begin)
  ;;(define-key wsmm-q-keymap "f"    'wsmm-search)
  ;;(define-key wsmm-q-keymap "\C-f" 'wsmm-search)
  (define-key wsmm-q-keymap "f"    'isearch-forward)
  (define-key wsmm-q-keymap "\C-f" 'isearch-forward)
  ;; ^Qg
  ;; ^Qh
  ;; ^Qi
  ;; ^Qj
  (define-key wsmm-q-keymap "k"    'wsmm-goto-block-end)
  (define-key wsmm-q-keymap "\C-k" 'wsmm-goto-block-end)
  (define-key wsmm-q-keymap "l"    'undo)
  (define-key wsmm-q-keymap "\C-l" 'undo)
  ;; ^Qm
  ;: ^Qn
  ;; ^Qo
  (define-key wsmm-q-keymap "p"    'wsmm-goto-last-cursorposition)
  (define-key wsmm-q-keymap "\C-p" 'wsmm-goto-last-cursorposition)
  (define-key wsmm-q-keymap "r"    'beginning-of-buffer)
  (define-key wsmm-q-keymap "\C-r" 'beginning-of-buffer)
  (define-key wsmm-q-keymap "s"    'beginning-of-line)
  (define-key wsmm-q-keymap "\C-s" 'beginning-of-line)
  ;; ^Qt
  (define-key wsmm-q-keymap "\C-u" 'keyboard-quit)
  ;; ^Qv
  ;;(define-key wsmm-q-keymap "w"    'wsmm-last-error)
  ;;(define-key wsmm-q-keymap "\C-w" 'wsmm-last-error)
  (define-key wsmm-q-keymap "x"    'wsmm-goto-window-end)
  (define-key wsmm-q-keymap "\C-x" 'wsmm-goto-window-end)
  (define-key wsmm-q-keymap "y"    'wsmm-kill-eol)
  (define-key wsmm-q-keymap "\C-y" 'wsmm-kill-eol)

  (define-key wsmm-k-keymap "0"    'wsmm-set-marker-0)
  (define-key wsmm-k-keymap "1"    'wsmm-set-marker-1)
  (define-key wsmm-k-keymap "2"    'wsmm-set-marker-2)
  (define-key wsmm-k-keymap "3"    'wsmm-set-marker-3)
  (define-key wsmm-k-keymap "4"    'wsmm-set-marker-4)
  (define-key wsmm-k-keymap "5"    'wsmm-set-marker-5)
  (define-key wsmm-k-keymap "6"    'wsmm-set-marker-6)
  (define-key wsmm-k-keymap "7"    'wsmm-set-marker-7)
  (define-key wsmm-k-keymap "8"    'wsmm-set-marker-8)
  (define-key wsmm-k-keymap "9"    'wsmm-set-marker-9)
  (define-key wsmm-k-keymap "b"    'wsmm-set-block-begin)
  ;; ^Ka
  (define-key wsmm-k-keymap "\C-b" 'wsmm-set-block-begin)
  (define-key wsmm-k-keymap "c"    'wsmm-block-copy)
  (define-key wsmm-k-keymap "\C-c" 'wsmm-block-copy)
  (define-key wsmm-k-keymap "d"    'wsmm-save-kill-buffer)
  (define-key wsmm-k-keymap "\C-d" 'wsmm-save-kill-buffer)
  ;; ^Ke
  (define-key wsmm-k-keymap "f"    'find-file)
  (define-key wsmm-k-keymap "\C-f" 'find-file)
  ;; ^Kg
  (define-key wsmm-k-keymap "h"    'wsmm-toggle-markers)
  (define-key wsmm-k-keymap "\C-h" 'wsmm-toggle-markers)
  (define-key wsmm-k-keymap "i"    'wsmm-block-indent)
  (define-key wsmm-k-keymap "\C-i" 'wsmm-block-indent)
  :; ^Kj
  (define-key wsmm-k-keymap "k"    'wsmm-set-block-end)
  (define-key wsmm-k-keymap "\C-k" 'wsmm-set-block-end)
  ;; ^Kl
  ;; ^Km  ; Rectangle/Stream mode
  ;; ^Kn
  ;; ^Ko
  ;; ^Kp print block?
  (define-key wsmm-k-keymap "q"    'wsmm-kill-buffer)
  (define-key wsmm-k-keymap "\C-q" 'wsmm-kill-buffer)
  (define-key wsmm-k-keymap "r"    'insert-file)
  (define-key wsmm-k-keymap "\C-r" 'insert-file)
  (define-key wsmm-k-keymap "s"    'save-buffer)
  (define-key wsmm-k-keymap "\C-s" 'save-buffer)
  (define-key wsmm-k-keymap "S"    'save-some-buffers)
  (define-key wsmm-k-keymap "t"    'wsmm-word-mark)
  (define-key wsmm-k-keymap "\C-t" 'wsmm-word-mark)
  (define-key wsmm-k-keymap "u"    'wsmm-block-unindent)
  (define-key wsmm-k-keymap "\C-u" 'keyboard-quit)
  (define-key wsmm-k-keymap "v"    'wsmm-block-move)
  (define-key wsmm-k-keymap "\C-v" 'wsmm-block-move)
  (define-key wsmm-k-keymap "w"    'wsmm-block-write)
  (define-key wsmm-k-keymap "\C-w" 'wsmm-block-write)
  ;; ^Kx
  (define-key wsmm-k-keymap "y"    'wsmm-block-delete)
  (define-key wsmm-k-keymap "\C-y" 'wsmm-block-delete)
  :; ^Kz
  )

(defvar wsmm--keymap-active nil)

(defvar wsmm--keymap-alist
  `((wsmm-mode . ,wsmm-keymap)))

(defun wsmm--uninstall-keymap ()
  (when wsmm--keymap-active
    (setq emulation-mode-map-alists
	  (delq 'wsmm--keymap-alist emulation-mode-map-alists)
	  wsmm--keymap-active nil)
    ;(message "DEBUG: Keymap deactivated")
    ))

(defun wsmm--install-keymap ()
  (unless wsmm--keymap-active
    (unless wsmm--keymap-initialized
      (wsmm--init-keymap)
      (setq wsmm--keymap-initialized t))
    (wsmm--uninstall-keymap)
    (add-to-ordered-list 'emulation-mode-map-alists 'wsmm--keymap-alist 400)
    (setq wsmm--keymap-active t)
    ;(message "DEBUG: Keymap activated")
    ))



;;; Minor mode stuff

;; (defcustom wsmm-allowed-mode-list
;;   '(fundamental-mode
;;     elist-mode
;;     )
;;   "Major modes that allow WordStar mode."
;;   :type '(repeat symbol)
;;   :group 'wsmm-mode)

(defcustom wsmm-disallowed-mode-list
  '(Custom-mode

    dired-mode
    efs-mode
    tar-mode

    browse-kill-ring-mode
    recentf-mode
    recentf-dialog-mode
    occur-mode

    mh-folder-mode
    gnus-group-mode
    gnus-summary-mode

    completion-list-mode

    Buffer-menu-mode
    compilation-mode

    rcirc-mode

    jde-javadoc-checker-report-mode

    view-mode
    vm-mode
    vm-summary-mode
    )
  "Major mode that won't co-exist with Wordstar mode."
  :type '(repeat symbol)
  :group 'wsmm-mode)

(defvar wsmm--minibuffer-active nil
  "Stores if the WordStar keymap is disabled because of an active minibuffer.")

(defun wsmm--mode-allowed ()
  (interactive)
  (save-match-data
    (cond (wsmm--minibuffer-active nil)
	  ((string-match "\*Minibuf-" (buffer-name)) nil)
	  ;;TODO ((memq major-mode wsmm-allowed-mode-list) t)
	  ((memq major-mode wsmm-disallowed-mode-list) nil))))
;	  (t (and (eq (key-binding "a") 'self-insert-command)
;		  (eq (key-binding " ") 'self-insert-command))))))

(defun wsmm--minibuffer-setup ()
   (wsmm--uninstall-keymap)
   (setq wsmm--minibuffer-active t)
)
(defun wsmm--minibuffer-exit ()
   (wsmm--install-keymap)
   (setq wsmm--minibuffer-active nil)
)

(defun wsmm--find-file ()
   (message "FIND_FILE    a %s, mb %s, bn %s, md %s"
	    (wsmm--mode-allowed) wsmm--minibuffer-active
	    (buffer-name) major-mode)
)
(defun wsmm--major-mode-change ()
   (message "MAJOR_MODE   a %s, mb %s, bn %s, md %s"
	    (wsmm--mode-allowed) wsmm--minibuffer-active
	    (buffer-name) major-mode)
)

(defadvice other-buffer (after other-buffer)
   (message "OTH_BUF      a %s, mb %s, bn %s, md %s"
	    (wsmm--mode-allowed) wsmm--minibuffer-active
	    (buffer-name) major-mode)
)
(defadvice other-window (after other-window)
   (message "OTH_WIN mb   a %s, mb %s, bn %s, md %s"
	    (wsmm--mode-allowed) wsmm--minibuffer-active
	    (buffer-name) major-mode)
)
(defadvice internal-show-cursor (after internal-show-cursor)
   (message "INT_SHOW_CUR a %s, mb %s, bn %s, md %s"
	    (wsmm--mode-allowed) wsmm--minibuffer-active
	    (buffer-name) major-mode)
)
(defadvice switch-to-buffer (after switch-to-buffer)
   (message "WS_TO_BUF    a %s, mb %s, bn %s, md %s"
	    (wsmm--mode-allowed) wsmm--minibuffer-active
	    (buffer-name) major-mode)
)




;;;###autoload
(define-minor-mode wsmm-mode
  "Toggle WordStar minor-mode."
  :global t
  :group 'wsmm-mode
  :require 'wsmm
  :lighter "-WS"
  (if wsmm-mode
      (progn

	(when nil
	  (progn
	    (split-window-vertically)
	    (other-window 1)
	    (switch-to-buffer "*Messages*")
	    (kill-region (point-min) (point-max))
	    (other-window 1)))

	; Turn off WordStar mode while in minibuffer
	(add-hook 'minibuffer-setup-hook 'wsmm--minibuffer-setup nil nil)
	(add-hook 'minibuffer-exit-hook 'wsmm--minibuffer-exit nil nil)
	;(add-hook 'find-file-hooks 'wsmm--find-file nil nil)
	;(add-hook 'change-major-mode-hook 'wsmm--major-mode-change nil nil)
	;(ad-activate 'other-buffer)
	;(ad-activate 'other-window)
	;(ad-activate 'internal-show-cursor)
	;(ad-activate 'switch-to-buffer)

	;;^X o   other-window
	;;^X k   kill-this-buffer
	;;       -> kill-buffer

	(wsmm--install-keymap)
	(wsmm--block-show)
	)
    (progn
      (remove-hook 'minibuffer-setup-hook 'wsmm--minibuffer-setup)
      (remove-hook 'minibuffer-exit-hook 'wsmm--minibuffer-exit)
      ;(remove-hook 'find-file-hooks 'wsmm--find-file)
      (add-hook 'change-major-mode-hook 'wsmm--major-mode-change)
      (wsmm--uninstall-keymap)
      (wsmm--block-hide)
    )))


(provide 'wsmm)
