;;; -*- mode: emacs-lisp; lexical-binding: t; -*-

;; crg-utils --- operational utilities for emacs init and sessions
;; Copyright (C) 2014 Christopher R. Genovese, all rights reserved.

;; Author: Christopher Genovese <genovese@cmu.edu>
;; Maintainer: Christopher R. Genovese <genovese@cmu.edu>
;; URL: http://www.stat.cmu.edu/~genovese/emacs/

;; Version: 1.0.0
;; Update#: 1
;; Created:      Wed 05 Nov 2014
;; Last-Updated: Fri 07 Nov 2014 at 16:27 EST
;; By: Christopher R. Genovese

;;; Commentary:

;; These utilities are used in my init file (and emacs sessions).
;; Hence, they are unprefixed.

;;; Code:

;ATTN: consider adapting this like forward-list-or-char below.
;ALSO: There is a common abstraction here for many of the navigation
;      commands. Consider writing a macro to handle the transformation.
;      use the model below for forward-list-or-char
(defun forward-sexp-or-char (&optional arg)
  "Move forward across one balanced expression (sexp) or forward
one character if none. With ARG, do it that many times.  Negative
arg -N means move backward across N balanced expressions. In
comparison to backward-sexp, this allows you to continue moving
across expressions without having to change keys."
  (interactive "p")
  (or arg (setq arg 1))
  (condition-case nil
      (forward-sexp arg)
    (error
     (let ((at-sexp-boundary? ; do we see a sexp delimiter in proper direction?
            (if (< arg 0)
                (if (char-equal (char-syntax (char-before (point))) ?\() t nil)
              (if (char-equal (char-syntax (char-after (point))) ?\)) t nil))))
       (when at-sexp-boundary?
           (forward-char arg))))))

(defun backward-sexp-or-char (&optional arg)
  "Move backward across one balanced expression (sexp) or
backward one character if at the boundary.  With ARG, do it that
many times.  Negative arg -N means move forward across N balanced
expressions.  In comparison to backward-sexp, this allows you to
continue moving across expressions without having to change
keys."
  (interactive "p")
  (or arg (setq arg 1))
  (forward-sexp-or-char (- arg)))

;; ATTN: Provisional
(defun forward-list-or-char (&optional arg)
  "Move forward across one balanced group of parentheses,
or forward one character at a list boundary, or forward a
word otherwise. With ARG, do it that many times.
Negative arg -N means move backward across N groups of parentheses.
This command assumes point is not in a string or comment."
  (interactive "p")
  (or arg (setq arg 1))
  (condition-case nil
      (forward-list arg)
    (scan-error ; problem, so move 1 at a time
     (let ((arg-sgn (if (< arg 0) -1 1))
           (arg-abs (abs arg)))
       (while (> arg-abs 0)
         (condition-case nil
             (forward-list arg-sgn)
           (scan-error
            (forward-char arg-sgn)))
         (setq arg-abs (1- arg-abs)))))))

(defun backward-list-or-char (&optional arg)
  "Move backward across one balanced group of parentheses,
or backward one character at a list boundary, or backward a word
otherwise. With ARG, do it that many times. Negative arg -N means
move forward across N groups of parentheses. This command assumes
point is not in a string or comment."
  (interactive "p")
  (or arg (setq arg 1))
  (forward-list-or-token (- arg)))

(defun my/move-beginning-of-line (&optional n)
  "Move to beginning of N-1st line ahead or to first non-whitespace character.
If not at the beginning of a line, move point to the beginning of the line,
moving forward n - 1 lines first if n is not nil or 1.
If at the beginning of a line, move instead back to the first
non-whitespace character on that line, ignoring N."
  (interactive "p")
  (if (looking-at "^")
      (back-to-indentation)
    (move-beginning-of-line n)))

(defun my/kill-ring-save (beg end)
  "Save the region as if killed, but don't kill it.
In Transient Mark mode, deactivate the mark.
If `interprogram-cut-function' is non-nil, also save the text for a window
system cut and paste.

If you want to append the killed line to the last killed text,
use \\[append-next-kill] before \\[kill-ring-save].

This command is similar to `copy-region-as-kill', except that it gives
visual feedback indicating the extent of the region being copied, and
prints a message to the echo area accordingly."
  (interactive "r")
  (copy-region-as-kill beg end)
  ;; This use of interactive-p is correct
  ;; because the code it controls just gives the user visual feedback.
  (if (interactive-p)
      (let ((other-end (if (= (point) beg) end beg))
	    (opoint (point))
	    ;; Inhibit quitting so we can make a quit here
	    ;; look like a C-g typed as a command.
	    (inhibit-quit t))
	(if (pos-visible-in-window-p other-end (selected-window))
	    (if (and transient-mark-mode
                     (face-background 'region))
                (message "Region Copied")
	      ;; Swap point and mark.
	      (set-marker (mark-marker) (point) (current-buffer))
	      (goto-char other-end)
	      (sit-for blink-matching-delay)
	      ;; Swap back.
	      (set-marker (mark-marker) other-end (current-buffer))
	      (goto-char opoint)
	      ;; If user quit, deactivate the mark
	      ;; as C-g would as a command.
	      (and quit-flag mark-active
		   (deactivate-mark)))
	  (let* ((killed-text (current-kill 0))
		 (message-len (min (length killed-text) 40)))
	    (if (= (point) beg)
		;; Don't say "killed"; that is misleading.
		(message "Saved text until \"%s\""
			(substring killed-text (- message-len)))
	      (message "Saved text from \"%s\""
		      (substring killed-text 0 message-len))))))))

(defun delete-whitespace (&optional direction-or-multiline)
  "Delete all spaces and tabs around point.
If DIRECTION-OR-MULTILINE is non-nil, then its value controls how
the deletion is done, as follows: 

  + When it is an integer > 1 (or with a single
    \\[universal-argument] when called interactively), deletion
    is forward only.

  + When it is a negative integer (or a negative prefix argument
    or \\[negative-argument] when called interactively), deletion
    is backward only.

  + When it is 0 or <-1 or > 4 (a zero digit argument or multiple
    prefix arguments, positive or negative), deletion spans
    multiple lines, in the direction specified by the sign.

Thus a zero argument deletes across multiple lines in both
directions."
  (interactive "*p")
  (let* ((orig-pos (point))
         (multi-line (or
                      (= direction-or-multiline 0)
                      (< direction-or-multiline -1)
                      (> direction-or-multiline  4)))
         (backward-only (< direction-or-multiline 0))
         (forward-only (> direction-or-multiline 1))
         (skip-characters (if multi-line " \t\f\r\n" " \t")))
    (delete-region
     (if forward-only
         orig-pos
       (save-excursion
         (skip-chars-backward skip-characters)
         (constrain-to-field nil orig-pos)))
     (if backward-only
         orig-pos
       (save-excursion
         (skip-chars-forward skip-characters)
         (constrain-to-field nil orig-pos t))))))


(defun new-buffer (&optional noselect)
  "Create a new buffer named *Untitled*<n> for the n that makes it unique.
If NOSELECT is nil, switch to that buffer; otherwise just display it."
  (interactive "P")
  (let ((buf (get-buffer-create (generate-new-buffer-name "*Untitled*"))))
        (if (null noselect)
            (switch-to-buffer buf)
          (display-buffer buf))))

(defun start-shell (&optional dir buffer)
  (interactive "DWorking directory: \nBShell buffer name: ")
  (let ((default-directory (or (file-name-as-directory dir) default-directory)))
    (shell buffer)))


(defun with-infinite-fill (func)
  "Set effectively infinite fill column during function FUNC.
FUNC should be a symbol with non-void, interactive function
definition. Inspired by `ourcomments-util' and Sean Burke."
  (let ((fill-column most-positive-fixnum))
    (call-interactively func)))

(defun unfill-paragraph ()
  "Convert a paragraph a single line of text."
  (interactive)
  (with-infinite-fill 'fill-paragraph))

(defun unfill-region ()
  "Convert all paragraphs in the region into
single lines of text."
  (interactive)
  (with-infinite-fill 'fill-region))

(defun unfill-individual-paragraphs ()
  "Convert each paragraph in the region into
single lines of text, respecting each individual
paragraph's indentation and (apparent) fill prefix."
  (interactive)
  (with-infinite-fill 'fill-individual-paragraphs))


(defun eval-and-replace-sexp ()
  "Replace the preceding emac-lisp sexp in current buffer with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(defun view-url-in-buffer ()
  "Open a new buffer containing the contents of URL.
If thingatpt+ is loaded, tries to find URL nearest point as
default; otherwise, no default is given."
  (interactive)
  (let* ((default (if (featurep 'thingatpt+) (thing-nearest-point 'url) "")) 
         (url (read-from-minibuffer "URL: " default))
         (urlbuf (url-retrieve-synchronously url)))
    (when urlbuf
      (switch-to-buffer urlbuf)
      (rename-buffer url t)
      (goto-char (point-min))
      ;; For an http request, header is included in the buffer,
      ;; so remove up to and including first blank line
      (when (and (looking-at "HTTP")
                 (re-search-forward "^\\s-*$" nil t))
        (condition-case nil (forward-char 1) (error nil)) ; move past blank line
        (delete-region (point-min) (point)))
      (setq case-fold-search t) ; buffer local
      ;; This should use nxml-mode or nxhtml-mode automatically
      (cond ((looking-at "<?xml") (xml-mode))
            ((looking-at "<\\(!DOCTYPE\\s-+\\)html") (html-mode))))))

(defun yank-cwd ()
  (interactive)
  (thread-first default-directory
    abbreviate-file-name
    kill-new))

(defun isearch-yank-lisp-symbol ()
  (interactive)
  (isearch-yank-internal
   (lambda ()
     (let ((distance (skip-syntax-forward "w_")))
       (when (zerop distance)
         (forward-char 1))
       (point)))))

(defun my/bury-completions ()
  "Bury the *Completions* buffer unless it doesn't exist or we're in it."
  (let ((buffer (get-buffer "*Completions*")))
    (when (and buffer (not (eq buffer (current-buffer))))
      (bury-buffer buffer))))

(defun set-abbrev-mark ()
  "Mark current point as the beginning of an abbrev.
Abbrev to be expanded starts here rather than at beginning of word before point."
  (interactive)
  (setq abbrev-start-location (point-marker)
	abbrev-start-location-buffer (current-buffer))
  (message "Abbrev Mark Set"))

(defun toggle-window-dedication (&optional window)
  "Toggle dedication state of WINDOW, or current window by default."
  (interactive)
  (let* ((the-window (or window (selected-window)))
         (dedicated (window-dedicated-p the-window)))
    (set-window-dedicated-p the-window (not dedicated))
    (when (called-interactively-p 'interactive)
      (message "Window %sdedicated to %s"
               (if dedicated "no longer " "")
               (buffer-name (window-buffer the-window))))))


(defvar my/cua-rectangle-key [(control ?c) (control return)])
(defun enable-cua-rectangles ()
  "Turn on (only) the rectangle feature of cua mode.
Also, take care of conflicting keybindings with icicles. Should
be called before `my-set-completion-mode' in
`my-set-operating-state' so that icicles properly sets up its
bindings. But if called manually, then icy-mode should be cycled
on and off afterwards."
  (interactive)
  (cond
   ((and (boundp 'cua-rectangle-mark-key)
         (memq 'standard-value (symbol-plist 'cua-rectangle-mark-key))
         (bound-and-true-p my/cua-rectangle-key))
    (customize-set-variable 'cua-rectangle-mark-key my/cua-rectangle-key))
   ((bound-and-true-p my/cua-rectangle-key)
    (setq cua-rectangle-mark-key my/cua-rectangle-key)))
  (setq cua-enable-cua-keys nil)
  (require-soft 'cua-rect)
  (eval-after-load 'cua-rect
    (progn
      (cua-mode t)
      (cua--rect-M/H-key ?\ 'cua-close-rectangle)
      (cua--rect-M/H-key ?c 'cua-copy-rectangle)
      (message "In cua rectangle now M-<space>: close, M-c: copy")))
  (delete-selection-mode -1))


(defun c-up-list-neg (arg)
  "Like up-list but with a negated arg"
  (interactive "P")
  (if (not arg)
      (up-list -1)
    (up-list arg)))

(defun my/c-mode-balanced-brace ()
  "Insert balanced electric brace pair when appropriate.
   Expects closing brace to be electric, though this is not
   required."
  (interactive)
  (let ((pps (syntax-ppss)))
    (if (or (not (eolp)) (nth 3 pps) (nth 4 pps)) 
        (insert "{") ;; not EOL or in string or comment
      (just-one-space)
      (c-electric-brace nil)
      (let ((pos (point)))
        (insert "\n")
        (execute-kbd-macro "}")
        (goto-char pos))
      (c-indent-line))))

(defun my/get-makefile-targets ()
  "Scan the contents of all files makefiles in the current
directory and return a list of targets.  The files scanned match
the glob [mM]akefile* within .  All targets are returned except
those containing $ (for variable substitution) or % for pattern
matching."
  (with-temp-buffer
    (let ((makefiles (nreverse (file-expand-wildcards "[mM]akefile*")))
          targets)
      (dolist (file makefiles)
        (insert-file-contents (concat default-directory file)))
      (goto-char (point-min))
      ; Simply exclude $ and %. Can add more generality later if needed.
      (while (re-search-forward "^\\s-*\\([^$%: 	\n]+?\\)\\s-*:" nil t)
        (setq targets (cons (match-string 1) targets)))
      (nreverse (cons "" targets)))))


(defun turn-on-show-paren-mode ()
  "Set up and initiate `show-paren-mode'. If
   This turns on the mode globally if the
   mode variable is not local."
  (setq show-paren-delay 0)  ; default 0.125
  (setq show-paren-style 'parenthesis)
  (show-paren-mode '1)
  (set-face-background 'show-paren-mismatch "red"))

(defun turn-on-local-show-paren-mode ()
  (make-local-variable 'show-paren-mode)
  (turn-on-show-paren-mode))

(defun turn-on-local-comment-auto-fill ()
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (auto-fill-mode '1))

(defun highlight-attn-words ()
  (font-lock-add-keywords
   nil '(("\\<\\(ATTN\\(?::[A-Za-z0-9_-]+\\)?\\|TODO\\|FIX\\(?:ME\\)?\\|HACK\\):?"
          1 font-lock-warning-face t))))

(defun generic-text-modes-hook ()
  "Sets up environment useful for a variety of text editing modes.
   Add this to the corresponding hooks."
  (turn-on-flyspell)
  (turn-on-local-comment-auto-fill))

(defun generic-programming-modes-hook ()
  "Sets up environment useful for a variety of programming modes.
   Add this to the corresponding hooks."
  (highlight-attn-words)
  (flyspell-prog-mode)               ;ATTN: good idea?
  (turn-on-local-comment-auto-fill))


(defun keyboard-escape-quit ()
  "Exit the current \"mode\" (in a generalized sense of the word).
This command can exit an interactive command such as
`query-replace', can clear out a prefix argument or a region, can
get out of the minibuffer or other recursive edit, cancel the use
of the current buffer (for special-purpose buffers)."
  (interactive)
  (cond ((eq last-command 'mode-exited) nil)
        ((region-active-p)
         (deactivate-mark))
        ((> (minibuffer-depth) 0)
         (abort-recursive-edit))
        (current-prefix-arg
         nil)
        ((> (recursion-depth) 0)
         (exit-recursive-edit))
        (buffer-quit-function
         (funcall buffer-quit-function))
        ((string-match "^ \\*" (buffer-name (current-buffer)))
         (bury-buffer))))

(defun make-backup-file-name (file)
  "Create the non-numeric backup file name for FILE.
This is a separate function so you can redefine it for customization."
  (concat (file-name-directory file)
          (concat "." (concat (file-name-nondirectory file) "~"))))

;;ATTN: need to redefine `backup-file-name-p' and `file-name-sans-version' also
;; to be consistent with the above definition!
;; Note also the variable `make-backup-file-name-function'
;; which can be used instead of wholesale redefining make-backup-file-name
;; but since I have to redefine two others, might as well redefine all three

(defun backup-file-name-p (file)
  "Return non-nil if FILE is a backup file name (numeric or not).
This is a separate function so you can redefine it for customization.
You may need to redefine `file-name-sans-versions' as well."
    (string-match "\\`\\..*~\\'" file))

(defvar file-name-version-regexp
  "\\(?:~\\|\\.~[-[:alnum:]:#@^._]+\\(?:~[[:digit:]]+\\)?~\\)"
  ;; The last ~[[:digit]]+ matches relative versions in git,
  ;; e.g. `foo.js.~HEAD~1~'.
  "Regular expression matching the backup/version part of a file name.
Used by `file-name-sans-versions'.")

;; ATTN this is not updated yet
(defun file-name-sans-versions (name &optional keep-backup-version)
  "Return file NAME sans backup versions or strings.
This is a separate procedure so your site-init or startup file can
redefine it.
If the optional argument KEEP-BACKUP-VERSION is non-nil,
we do not remove backup version numbers, only true file version numbers.
See also `file-name-version-regexp'."
  (let ((handler (find-file-name-handler name 'file-name-sans-versions)))
    (if handler
	(funcall handler 'file-name-sans-versions name keep-backup-version)
      (substring name 0
		 (unless keep-backup-version
                   (string-match (concat file-name-version-regexp "\\'")
                                 name))))))

(provide 'crg-ops)
