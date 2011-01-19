;;; -*- Mode: Emacs-Lisp; Mode: Linkd -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                    ;;
;; Emacs Initialization                                               ;;
;;                                                                    ;;
;; Version:    2.5.5                                                  ;;
;; Author:     Christopher R. Genovese                                ;;
;; Maintainer: Christopher R. Genovese                                ;;
;;                                                                    ;;
;; Last Updated: 2010 Dec 22 Wed 22:55                                ;;
;; By:           Christopher R. Genovese                              ;;
;; Update #:     260                                                  ;;
;;                                                                    ;;
;; Copyright (C) 2010, Christopher R. Genovese, all rights reserved.  ;;                    
;;                                                                    ;;
;;                                                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Commentary
;;
;; This emacs initialization is designed to support GNU Emacs 22+.
;; It has so far been tested with versions 22.2.1 and 23.2. 
;; Support for earlier emacs versions and for XEmacs is not 
;; guaranteed, although much of the functionality may work.
;;
;; This initialization file is built from a number of components
;; and scripts. If you have access to the original files, you
;; should modify those and not this one. You can access the
;; original files at
;;
;;     (@url :file-name "http://www.stat.cmu.edu/~genovese/code/emacs-init/" :display "Genovese Emacs Init")
;;
;; or email the author at (@url :file-name "mailto:genovese@cmu.edu" :display "genovese@cmu.edu").
;;
;; The intialization file works best with a number of third-party
;; and pre-installed modes. See the README file at the above
;; link for a list with version and update numbers. Most, if not
;; all, can be obtained at (@url :file-name "http://www.emacswiki.org/" :display "EmacsWiki"). The code tests for
;; the presence of these libraries, so no errors should occur
;; if they are missing. The code can be customized to incorporate
;; different or additional libraries; see the aforementioned README
;; file for instructions and see imports.el in the build directory
;; or (@> "Imported Mode Requirements") below.
;;
;; NOTE: If you have (@url :file-name "http://www.emacswiki.org/emacs/LinkdMode" :display "linkd.el"), you can engage \\[linkd-mode]
;;       to convert the table of contents below into links 
;;       throughout this file. Also, major divisions are separated
;;       by two consecutive newlines so including \n\n+ in your
;;       page-delimiter regexp will make it easy to navigate
;;       within sections.
;;
;; 1. Added Functionality
;;
;; This file extends the functionality to editing, navigation, dired,
;; icicles, ibuffer, TeX modes, and shell directory tracking, as well as
;; defining a variety of utility functions and commands. See the
;; extended documentation in the sections below.
;;
;; With editing and scrolling see the commands `my-move-beginning-of-line',
;; `scroll-down-or-beg', `scroll-up-or-end', `forward-sexp-or-char', and
;; `backward-sexp-or-char' for some examples.
;;
;; 2. Keybindings
;;
;; The keybindings section below extensively remaps defaults both in
;; the global map for specific modes that I've refined over the years.
;; Each person has different preferences, of course, but I think you
;; will find these useful. I've found these bindings to be both
;; memorable and efficient. An underlying design goal is to make
;; as much of the functionality as possible easily accessible.
;;
;; In addition, there is a consistent theme to some of the bindings
;; that differs from the default GNU keymaps. This has implications
;; for the bindings in the global and various mode maps. These
;; differences are as follows:
;;
;;     A. I use C-w/C-v instead of C-v/M-v for scrolling.
;;        This keeps common level scrolling at the same level,
;;        and I find it easier to use these keys than to
;;        change modifiers.
;;
;;     B. I use M-w/M-v for beginning and end of buffer.
;;
;;     C. I use M-k for kill region rather than C-w,
;;        along with other kill commands C-k and C-M-k.
;;        This is both mnemonic and consistent.
;;
;;     D. Although help is important, an extra key stroke
;;        for help is a small price to pay to reduce a more
;;        commonly used command. Specifically, help-command
;;        is bound to C-c h (and C-c C-h and M-? for convenience).
;;        C-h is bound to `delete-backward-char' (to save pinky
;;        extension for the backspace key) and M-h is bound to
;;        `delete-backward-word' which is very frequently used.
;;
;;     E. Although M-x is still available, I find it more
;;        convenient to bind `execute-extended-command'
;;        to C-x C-m and C-c C-m. 
;;       
;; In addition to the manifold other bindings, I propogate
;; this theme throughout the keybindings when the logical
;; functions in A-E are relevant. Eventually, I will add
;; a switch to make the bindings use the default theme, but
;; for now, you might want to try them as is.
;;
;; See also my quick-nav package for an even more efficient
;; keybinding theme and associated modes.
;;
;; 3. Configuration
;;
;; Configuration for various modes and other tools is given
;; by sections as indicated by the following contents. Several
;; new configuration variables are also introduced. See the
;; documentation below.
;;
;; 4. Conventions
;;
;; I use a prefix my- for *most* new functions and variables to avoid
;; conflicts now or in future versions, although this is used somewhat
;; inconsistently in older code. Most hooks are defined with
;; a my- version first which is then added to the hook. Interactive
;; functions in several modes are modified into my- versions and used
;; for keybindings (see for instance, `my-comint-bol'), although many
;; do not use my-. Variables or functions for purely internal use
;; are prefixed by private/my- with obvious meaning. In a future
;; version, I am likely to transition to a two name space with slash
;; formulation, using pvt-crg/ and crg/  in place of my-, using my-
;; only for configuration variables for which that makes the most sense.
;; I am still considering this, however. Note also that for some
;; hook functions, where I add lines with some frequency,
;; I leave the closing paren on its own line for convenient adding,
;; contrary to established lisp convention. 
;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TABLE OF CONTENTS

;;  1. (@> "Emacs Lisp Utility Functions")
;;  2. (@> "Environment Configuration")
;;  3. (@> "Imported Mode Requirements")
;;  4. (@> "Text Modes")
;;  5. (@> "Programming Modes")
;;  6. (@> "Shell-based Modes")
;;  7. (@> "Navigation and Action Modes")
;;  8. (@> "TeX-related Modes")
;;  9. (@> "Other Modes and Features")
;; 10. (@> "ESS Mode for R and S Code")
;; 11. (@> "Global Key Bindings")
;; 12. (@> "Settings and Initialization")
;; 13. (@> "System Dependent Settings")
;; 14. (@> "Faces")
;; 15. (@> "Initial State")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                           ;;
;;(@* "Emacs Lisp Utility Functions")                        ;;;;;;
;;                                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Section Contents:
;; 
;;   A. (@> "Aliases")
;;   B. (@> "Predicates for List Operations")
;;   C. (@> "List Functions")
;;   D. (@> "Macros")
;;   E. (@> "File and Directory Names")
;;   F. (@> "Scrolling and Cursor Movement")
;;   G. (@> "Emacs Operations")
;;   H. (@> "Imports and Features")
;;   I. (@> "Tools")
;;   J. (@> "Hook Setters and Toggles")
;;


(eval-when-compile (require 'cl)) ; Common Lisp facilities (only used here and there)


;; (@* "Aliases")
;;
;; Aliases to existing functions or macros that
;; I find useful or elegant or that I find myself
;; wanting to do frequently.
;;

(defalias 'filter           'remove-if-not)  ; NOTE: cl dependence
(defalias 'foreach          'dolist)

(defalias '==               'equal)
(defalias '===              'eql)   
(defalias 'num-equal        '=)   ; goes with 'string-equal
(defalias 'num-not-equal    '/=) 

(defalias 'to-string        'prin1-to-string)
(defalias 'stringify        'prin1-to-string)

(defalias 'symbol-string    'symbol-name)
(defalias 'string-symbol    'intern)
(defalias 'symbol-to-string 'symbol-name)
(defalias 'string-to-symbol 'intern)

;; Scheme/Clojure style predicates read nicely.
;; It's probably not a good idea to use these
;; aliases in portable emacs-lisp code because
;; it imposes the aliases on anyone who loads it.
;; But for casual use and in the .emacs file,
;; it might make sense. I've recently added these,
;; so much of the code below does not use them.
;; We'll see how things develop....

(defalias 'buffer-alive?    'buffer-live-p)
(defalias 'feature-loaded?  'featurep)
(defalias 'frame-alive?     'frame-live-p)
(defalias 'nil?             'null)
(defalias 'num-equal?       '=) 
(defalias 'num-not-equal?   '/=) 
(defalias 'string-equal?    'string-equal)
(defalias 'whole-number?    'wholenump)
(defalias 'window-alive?    'window-live-p)

(defalias 'file-is-directory?            'file-directory-p)
(defalias 'file-is-symlink?              'file-symlink-p)
(defalias 'file-is-accessible-directory? 'file-accessible-directory-p)

(dolist
    (sym '(arrayp atom bobp bolp booleanp boundp bound-and-true-p 
		  bufferp buffer-modified-p char-or-string-p char-table-p
		  commandp consp fboundp file-executable-p 
                  file-exists-p file-locked-p file-name-absolute-p
                  file-newer-than-file-p file-readable-p file-regular-p
                  file-writeable-p floatp framep functionp
		  integer-or-marker-p integerp keymapp keywordp
		  listp markerp nlistp number-or-marker-p numberp
		  overlayp processp sequencep stringp string-lessp
		  string-match-p string-or-null-p string-prefix-p
		  subrp symbolp vectorp windowp zerop))
  (defalias
    (string-symbol (replace-regexp-in-string "\\(?:-?p\\)?$" "?" (symbol-string sym)))
    sym))

;; (@* "Predicates for List Operations")
;;
;; Functions defined here:
;; `eq-car', `equal-car',
;;

(defun eq-car (l1 l2)
  "Returns t if car of l1 and l2 are eq, nil otherwise.
If l1 (or l2) are not cons cells, l1 (or l2) itself is used in the comparison.
This is useful as a predicate for removing elements from an association list;
see `remove-matching-elements'.  In this case, if the keys are symbols then
only the symbol of interest need be passed to `remove-matching elements'."
  (eq (if (consp l1) (car l1) l1) (if (consp l2) (car l2) l2)))

(defun equal-car (l1 l2)
  "Returns t if car of l1 and l2 are equal, nil otherwise.
If l1 (or l2) are not cons cells, l1 (or l2) itself is used in the comparison.
This is useful as a predicate for removing elements from an association list;
see remove-matching-elements.  In this case, if the keys are symbols then
only the symbol of interest need be passed to `remove-matching elements'."
  (equal (if (consp l1) (car l1) l1) (if (consp l2) (car l2) l2)))

;; (@* "List Functions")
;;
;; Note: The first two of these reproduce some functionality in the cl
;; package, with less generality, but I wrote them early in my elisp
;; career, before discovering cl. Because they are still used in my
;; shell-mode dirtracking, I am keeping them for now. However, their
;; future use should perhaps be deprecated.
;;
;; See also (@> "Predicates for List Operations") for predicates that
;; can be used with these functions.
;;
;; `concatenate-list' is a convenience function, which also replicates
;; cl functionality but tersely. It is used in several places in what
;; follows.
;;
;; Functions defined here:
;; `remove-matching-elements', `remove-non-matching-elements',
;; `concatenate-list'
;;

(defun remove-matching-elements (elt lst &optional pred)
  "Remove elements in LST matching ELT according to the
predicate PRED. If PRED is not supplied, it defaults to equal."
  (let ((pred (or pred (symbol-function 'equal)))
	(newlst nil)
	cmp)
    (while lst
      (setq cmp (car lst))
      (unless (funcall pred elt cmp)
        (setq newlst (cons cmp newlst)))
      (setq lst (cdr lst)))
    (nreverse newlst)))
      
(defun remove-non-matching-elements (elt lst &optional pred)
  "Remove elements in LST matching ELT according to the
predicate PRED. If PRED is not supplied, it defaults to equal."
  (let ((pred (or pred (symbol-function 'equal)))
	(newlst nil)
	cmp)
    (while lst
      (setq cmp (car lst))
      (when (funcall pred elt cmp)
	  (setq newlst (cons cmp newlst)))
      (setq lst (cdr lst)))
    (nreverse newlst)))

(defun concatenate-list (&rest seqs)
  "Concatenate arguments into a flattened list."
  (apply 'append (append seqs '(nil))))

;; (@* "Macros")
;;
;; Macros defined here:
;; `do-only-once', `my-aif', `my-awhen'
;;
;; The my- naming convention should probably be removed here.
;;

(defmacro do-only-once (loadvar &rest body)
  "Do BODY if LOADVAR is nil and set LOADVAR to t.
If LOADVAR is not nil, do nothing. LOADVAR can be
an arbitrary place, as accepted by setf. It should
be NIL initially."
  (declare (indent 1) (debug t))
  `(if ,loadvar
       nil
     ,@body
     (setf ,loadvar t)))

  ;; Adapted for elisp from Paul Graham's "ANSI Common Lisp"
(defmacro my-aif (test true-body &rest false-body)
  "Evaluate TRUE-BODY or FALSE-BODY depending on value of TEST.
If TEST returns non-nil, bind `it' to the value, and evaluate
TRUE-BODY.  Otherwise, evaluate forms in FALSE-BODY as if in `progn'.
Compare with `if'."
  (declare (indent 2))
  (let ((sym (make-symbol "my-tmp-aif-sym7814")))
    `(let ((,sym ,test))
       (if ,sym
	   (let ((it ,sym))
	     ,true-body)
	 (progn
	   ,@false-body)))))

(defmacro my-awhen (test &rest body)
  "Evaluate BODY if TEST returns non-nil.
During evaluation of body, bind `it' to the value returned by TEST."
  (declare (indent 1))
  `(my-aif ,test
       (progn ,@body)
     nil))

;; (@* "File and Directory Names")
;;
;; Operations on file and directory (and other) names
;; 
;; Functions defined here:
;; `expand-directory-name', `directory-name-and-sep',
;; `file-path-name', `directory-path-name',
;; `equal-file-name', `equal-dir-name',
;; `remove-prefix'
;; 

(defun expand-directory-name (dir)
  "Expands a directory name as in expand-file-name but ensures
that the name ends with a '/'.  This can be used to convert
a directory name, even in relative or ~ form, to a canonical
form, for comparison, concatenation, or other purposes."
  (file-name-as-directory (expand-file-name dir)))

(defalias 'directory-name-with-sep    'file-name-as-directory)
(defalias 'directory-name-without-sep 'directory-file-name)

(defun file-path-name (&rest path-components)
  "Return unexpanded path to *file* with path components given by
the list of strings PATH-COMPONENTS. Leading with an empty string
produces a path to a file from the current directory; leading
with the directory-separator character produces an absolute path.
The last component of the assumed path is treated as a file, so
no directory separator is appended; to get a path with an ending
separator, see `directory-path-name'."
  (directory-name-without-sep
   (mapconcat 'file-name-as-directory path-components "")))

(defun directory-path-name (&rest path-components)
  "Return unexpanded path to *directory* with path components given
by the list of strings PATH-COMPONENTS. Leading with an empty
string produces a path to a directory from the current directory;
leading with the directory-separator character produces an
absolute path. The returned path is treated as a directory,
with directory separator appended; to get a path without an
ending separator,  see `file-path-name'."
  (mapconcat 'directory-name-with-sep path-components ""))


(defun equal-file-name (fn1 fn2 &optional func)
  "Tests whether two file-names FN1 and FN2 refer to the same file.
This uses FUNC to bring both file-names to a common form (e.g., abolute).
If FUNC is not provided, it defaults to a expand-file-name."
  (if (and (stringp fn1) (stringp fn2))
      (let ((func (or func (symbol-function 'expand-file-name))))
	(string-equal (funcall func fn1) (funcall func fn2)))))

(defun equal-dir-name (fn1 fn2 &optional func)
  "Tests whether two file-names FN1 and FN2 refer to the same file.
This uses FUNC to bring both file-names to a common form (e.g., abolute).
If FUNC is not provided, it defaults to a function that does
expand-file-name followed by appending a trailing slash."
  (if (and (stringp fn1) (stringp fn2))
      (let ((func (or func (symbol-function 'expand-directory-name))))
	(string-equal (funcall func fn1) (funcall func fn2)))))

(defun remove-prefix (prefix name)
  "Return string consisting of NAME with
leading PREFIX removed. NAME and PREFIX
are strings, and PREFIX is interpreted as
a regexp. However, because a ^ is added to the
beginning of PREFIX, PREFIX must *not* begin
with a ^-anchor unless it is intended to match
a literal ^ character."
  (if (and (stringp name) (stringp prefix)
           (string-match (concat "^" prefix) name))
      (substring name (match-end 0)) ; match-end only meaningful if match
    name))

;; (@* "Scrolling and Cursor Movement")
;;
;; Improved scrolling and cursor movement functions as alternatives
;; to the standard offerings. One theme here is to avoid errors
;; on boundary conditions by dwim. In addition, context dependent
;; movement (e.g., at the beginning of the line is often useful).
;; For instance, `my-move-beginning-of-line' alternates moving
;; to the true beginning and the first non-whitespace 
;; character (see also `my-comint-bol' later).
;;
;; Functions defined here:
;; `scroll-down-or-beg', `scroll-up-or-end', `forward-sexp-or-char',
;; `backward-sexp-or-char', `my-move-beginning-of-line',
;;

(defun scroll-down-or-beg ()
  "Smart scroll down, if too close to the beginning of the buffer,
it moves you there without error, in contrast to scroll-down."
  (interactive)
  (condition-case nil (scroll-down) (error (goto-char (point-min)))))

(defun scroll-up-or-end ()
  "Smart scroll up, if too close to the end of the buffer,
it moves you there without error, in contrast to scroll-up."
  (interactive)
  (condition-case nil (scroll-up) (error (goto-char (point-max)))))

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
       (if at-sexp-boundary?
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

(defun my-move-beginning-of-line (&optional n)
  "If not at the beginning of a line, move point to the beginning
of the line, moving forward n - 1 lines first if n is not nil or
1. However, if at the beginning of a line, move back to the first
non-ws character on the line."
  (interactive "p")
  (if (looking-at "^")
      (back-to-indentation)
    (move-beginning-of-line n)))

;; (@* "Emacs Operations")
;;
;; Various functions that effect how emacs operates,
;; from the level of copying text to making backup
;; files and more.
;;
;; Functions defined here:
;; `make-backup-file-name', `set-abbrev-mark', `my-other-frame',
;; `my-kill-ring-save', `my-copy-region-as-kill',
;; 

(defun make-backup-file-name (file)
  "Create the non-numeric backup file name for FILE.
This is a separate function so you can redefine it for customization."
  (concat (file-name-directory file) (concat "." (concat (file-name-nondirectory file) "~"))))

(defun set-abbrev-mark ()
  "Mark current point as the beginning of an abbrev.
Abbrev to be expanded starts here rather than at beginning of word before point."
  (interactive)
  (setq abbrev-start-location (point-marker)
	abbrev-start-location-buffer (current-buffer))
  (message "Abbrev Mark Set"))

(defun my-other-frame (arg)
  "Select the ARG'th different visible frame, and raise it, even if
the frame is iconified or invisible. All frames are arranged in a
cyclic order, and this command selects the frame ARG steps away in
that order.  A negative ARG moves in the opposite order."
  (interactive "p")
  (let ((frame (selected-frame)))
    (while (> arg 0)
      (setq frame (next-frame frame))
      (setq arg (1- arg)))
    (while (< arg 0)
      (setq frame (previous-frame frame))
      (setq arg (1+ arg)))
    (raise-frame frame)
    (select-frame frame)
    (set-mouse-position (selected-frame) (1- (frame-width)) 0)
    (if (fboundp 'unfocus-frame)
	(unfocus-frame))))

;; Region copying:
;; 
;; I like a message even if the entire region is visible
;; and transient mark mode is on. Because it is conditional
;; cannot implement this easily with advice.

(defun my-kill-ring-save (beg end)
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

  ;; Standard version is silent, so it is hard to tell if the key was hit
  ;; Only added a message. ATTN: This could be implemented with advice,
  ;; but copy-region-as-kill is used programmatically in other functions
  ;; (e.g., kill-ring-save) that may not want a message printed.

(defun my-copy-region-as-kill (beg end)
  "Save the region as if killed, but don't kill it.
If `interprogram-cut-function' is non-nil, also save the text for a window
system cut and paste.  Produces a message to confirm copy."
  (interactive "r")
  (if (eq last-command 'kill-region)
      (kill-append (buffer-substring beg end) (< end beg))
    (kill-new (buffer-substring beg end)))
  (message "Region Copied")
  (if transient-mark-mode
      (setq deactivate-mark t))
  nil)

;; (@*  "Imports and Features")
;;
;; This is the beginning of a facility for managing requires
;; and imports in a simpler way. See (@> "Imported Mode Requirements")
;; for an example of how it is used. Extensions are likely in the
;; near future.
;;
;; Functions/macros defined here:
;; `require-soft', `with-library', `import'
;;

(defun require-soft (feature &optional file)
  "Try to require FEATURE, but don't signal an error if `require' fails."
  (require feature file 'noerror))

(defmacro with-library (symbol &rest body)
  "Execute BODY conditionally on the availability of library SYMBOL"
  (declare (indent 1))
  `(if (not (require ,symbol nil t))
       (message (format "Library %s is not available." ',symbol))
     ,@body))

(defun import (feature-list &optional action &rest xargs)
  "Apply ACTION to each of features given in FEATURE-LIST,
   supplying additional arguments if necessary. Typical
   use case is for ACTION to be require, require-soft,
   or autoload. If not supplied, ACTION defaults to
   require.

   Elements of FEATURE-LIST can have several forms.
   The feature can one of the following forms:

       1. a SYMBOL,
       2. a cons (CONDITION . SYMBOL),
       3. a list (SYMBOL ARGS...), or
       4. a list (CONDITION SYMBOL ARGS...).

   Here, SYMBOL is a symbol corresponding to a feature,
   typically one that can be validly require'd.
   And CONDITION is an expression that eval's
   to a boolean (validly at load and compile time)
   and is used to selectively load particular features.
   A typical example would be (>= emacs-major-version 22).
   ARGS is a list of 0 or more additional arguments.
   (In cases 1 and 2 above, ARGS will effectively be an
   an empty list.) The lists ARGS and that supplied in
   the &rest argument XARGS will be concatenated, in
   that order, and the corresponding elements will be
   passed as arguments to the ACTION function."
  (let ((req (or action 'require)))
    (dolist (feature feature-list)
      (let (sym args)
        (cond
         ;; Feature is a symbol
         ((symbolp feature)
          (setq sym feature)
          (setq args xargs)
          )
         ;; Feature is (list SYMBOL ARGS...)
         ((and (consp feature)
               (symbolp (car feature)))
          (setq sym (car feature))
          (setq args (concatenate-list (cdr feature) xargs))
          )
         ;; Feature is (CONDITION . SYMBOL) or (list CONDITION SYMBOL ARGS...)
         ((and (consp feature)
               (booleanp (eval (car feature)))
               (or (symbolp (cdr feature))
                   (and (listp (cdr feature))
                        (symbolp (cadr feature)))) )
          (when (eval (car feature))
            (if (symbolp (cdr feature))  ;; have cons (CONDITION . SYMBOL)
                  (progn
                    (setq sym (cdr feature))
                    (setq args xargs)
                    )                    ;; else have (list CONDITION SYMBOL ...)
              (setq sym (cadr feature))
              (setq args (concatenate-list (cddr feature) xargs))
              ))
          )
         (t
          (message (concat "Erroneous feature specification: " (prin1-to-string feature)))
          ))
        (if sym (apply req sym args))
        ))
    ))

(defalias 'deferred-import 'identity
  "Syntactic sugar to allow user to saliently list
   those modules that will be imported later in the
   init file but not in the main import list. This
   is usually done for timing purposes, for instance
   to ensure that hooks are properly defined at
   load time.")

;; (@* "Tools")
;;
;; Useful tools for both interactive and programmatic use.
;;
;; Functions defined here:
;; `new-buffer', `unfill-paragraph', `eval-and-replace-sexp',
;; `view-url-in-buffer', and `my-get-makefile-targets'.
;; 

(defun new-buffer (&optional noselect)
  "Create a new buffer named *Untitled*<n> for the n that makes it unique.
If NOSELECT is nil, switch to that buffer; otherwise just display it."
  (interactive "P")
  (let ((buf (get-buffer-create (generate-new-buffer-name "*Untitled*"))))
        (if (null noselect)
            (switch-to-buffer buf)
          (display-buffer buf))))

;(defun unfill-paragraph ()
;  "Convert a paragraph a single line of text."
;  (interactive)
;  (let ((fill-column (point-max)))
;    (fill-paragraph nil)))

(defun with-infinite-fill (func)
  "Set effectively infinite fill column during function FUNC.
FUNC should be a symbol with non-void, interactive function
definition. Inspired by `ourcomments-util' and Sean Burke."
  (let ((fill-column (1+ (point-max))))
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
  "Replace the preceding emac-lisp sexp in current buffer
   with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(defun view-url-in-buffer ()
  "Open a new buffer containing the contents of URL. If thingatpt+
   is loaded, tries to find URL nearest point as default; otherwise,
   no default is given."
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

(defun my-get-makefile-targets ()
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

;; (@* "Hook Setters and Toggles")
;;
;; Convenience functions for turning on, turning off,
;; or toggling various features in mode hooks.
;;
;; Functions defined here:
;; `turn-on-tex-parser-ispell', `turn-on-local-show-paren-mode',
;; `turn-on-global-show-paren-mode', `turn-on-local-comment-auto-fill',
;; `highlight-attn-words'
;;

(defun turn-on-tex-parser-ispell ()
  (make-local-variable 'ispell-parser)
  (setq ispell-parser 'tex))

(defun turn-on-local-show-paren-mode ()
  (make-local-variable 'show-paren-mode)
  (setq show-paren-delay 0)  ; default 0.125
  (setq show-paren-style 'parenthesis)
  (show-paren-mode '1)
  (set-face-background 'show-paren-mismatch "red"))

(defun turn-on-global-show-paren-mode ()
  (setq show-paren-delay 0)  ; default 0.125
  (setq show-paren-style 'parenthesis)
  (show-paren-mode '1)
  (set-face-background 'show-paren-mismatch "red"))

(defun turn-on-local-comment-auto-fill ()
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (auto-fill-mode '1))

(defun highlight-attn-words ()
  (font-lock-add-keywords
   nil '(("\\<\\(ATTN\\|TODO\\|FIX\\|FIXME\\|HACK\\):?"
          1 font-lock-warning-face t))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                           ;;
;;(@* "Environment Configuration")                           ;;;;;;
;;                                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This component is automatically generated from a template
;; by ./makeconfig, perl, and make. See Makefile for details.


(defvar my-user-name "genovese"
  "*If non-nil, this should be set to a string giving the user name
to use by default in locating the ....  If set to nil, this will
always use the HOME directory in the environment if it is available
as the root for the startup information.  Set this is you wish to
load your startup file when running as someone else via the -u flag.")

(defvar my-home-aliases (list "/Users/genovese/")
  "*This is a list of strings indicating possible aliases for the users
HOME directory that emacs might not recognize.  The first element of the list should be the
prefered or most common or most specific form.  This variable is most useful with
cross-mounted file systems, and is used by functions to determine whether 
or not a directory name should be changed to \"~/\".")

(defvar my-home-dir (if (and (equal my-user-name (getenv "USER")) (getenv "HOME"))
                        (concat (getenv "HOME") "/")
                      (or  (car my-home-aliases) ""))
  "Home directory")

(defvar my-site-lisp-dir (expand-directory-name (if (= emacs-major-version 22) "/Applications/Emacs22.app/Contents/Resources/site-lisp/" "/Applications/Emacs.app/Contents/Resources/site-lisp/"))
  "The site-lisp directory (absolute path) containing the globally available
   add-on modes and .el files. This name must end in /.")

(defvar my-home-lisp-dir (concat "~/.emacs.d/"
                                 (if (null (string-match "/$" "~/.emacs.d/")) "/"))
  "The site-lisp directory (absolute path) containing the users local
   add-on modes and .el files. This name must end in /.")

(when (>= emacs-major-version 23)
  (setq user-emacs-directory my-home-lisp-dir))

(defvar my-frame-alist `((top . 1) (left . 1) (width . ,(if (= emacs-major-version 23) 224 180)) (height . ,(if (= emacs-major-version 23)  64  62)))
  "Geometry parameters that will be used for initial-frame-alist to determine initial window size.")

(if (and (boundp 'my-home-dir) (stringp my-home-dir)
         (not (string= my-home-dir "")) (not (member my-home-dir my-home-aliases)))
    (setq my-home-aliases (cons my-home-dir my-home-aliases)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                           ;;
;;(@* "Imported Mode Requirements")                          ;;;;;;
;;                                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;
;; Lists of Packages to Import
;;

(defvar my-must-features
  '(comint shell calc)
  "*List of *built-in* features that must be required for later
   processing in this initialization file. Each element is either
   a symbol that is a valid argument to require or is a cons-cell
   whose car is a major version number for Emacs and whose cdr is
   a symbol that is a valid argument to require. These features
   will be required, and failure of the require will lead to an
   error.")

(defvar my-ought-features
  '(org-install icicles ido)
  "*List of features that are distributed with emacs that would be
   desireable to require for later processing in this initi file. 
   If for some reason, they are not available, a message will be
   displayed but no error produced.

   Each element of the list is either a symbol that is a valid
   argument to require or is a cons-cell whose car is a major
   version number for Emacs and whose cdr is a symbol that is a
   valid argument to require.")

(defvar my-want-features
  '(bookmark+ linkd lacarte dropdown-list yasnippet anything-config)
  "*List of features, not necessarily available, that it would be 
   desireable to require for later processing in this init file. 
   If for some reason, they are not available, a message will be
   displayed but no error produced.

   Each element of the list is either a symbol that is a valid
   argument to require or is a cons-cell whose car is a major
   version number for Emacs and whose cdr is a symbol that is a
   valid argument to require.")

(defvar my-autoloaded-features
  (list '(nxml-mode                 "nxml-mode.el"        "nXML Mode"      t)
        (if (<= emacs-major-version 22)
            '(nxhtml-mode           "nxml/autostart.el"   "nXHTML Mode"    t)
          '(nxhtml-mode             "nxhtml/autostart.el" "nXHTML Mode"    t))
        '(texmathp-compile          "texmathp"            "Parse TeX Math" t)
        )
  "*List of features to be autoloaded along with the arguments
   to be passed directly to autoload. Elements of the lists are of
   the form (SYMBOL ARGS...). Note that this list need not include
   any (auto-generated) autoloads in the system files.")

(defvar my-special-features
  (list
   (list 'dired-x    '(deferred-import "Loaded when dired is loaded, see my-dired-load-hook."))
   (list 'dired-aux  '(deferred-import "Loaded when dired is loaded, see my-dired-load-hook."))
   (list 'dired+     '(deferred-import "Loaded when dired is loaded, see my-dired-load-hook."))
   (list 'ibuf-ext   '(deferred-import "Only loaded as needed when ibuffer is first instantiated"))
   (list 'texmathp   '(deferred-import "Only loaded as needed when AucTeX is first instantiated"))
   (list 'ess-site   '(deferred-import "Temporarily deferring load to ess.el for timing purposes"))
   (list 'slime-autoloads '(deferred-import "Loaded after SLIME setup."))
   (list 'thingatpt+ '(eval-after-load "thingatpt"
                        '(require-soft 'thingatpt+)))
   (list 'cedet      '(if (not (featurep 'cedet))
                            (load-file (concat my-site-lisp-dir "cedet/common/cedet.el"))))
   (list 'jde        '(progn
                        (setq defer-loading-jde t)
                        (if (not defer-loading-jde)
                            (require-soft 'jde)
                          (autoload 'jde-mode "jde" "JDE mode." t)
                          (add-to-list 'auto-mode-alist '("\\.java\\'" . jde-mode))
                          )))
   (list 'ecb        '(progn
                        (eval-when-compile
                          (if (not (featurep 'cedet))
                              (require 'cedet)))
                        (require-soft 'ecb)
                        ))
   )
  "List of features that will need special processing (usually beyond
   a simple require) for use in this init file. Although somewhat
   awkward, the code to handle these cases should be given as
   a form in the list. Specifically, each element of the list
   should in turn be a list with either two or three elements
   corresponding to 

      (list 'SYMBOL 'BODYFORM) 
      (list  CONDITION 'SYMBOL 'BODYFORM),

   where CONDITION is a lisp expression that evaluates to t
   if this feature should be acted upon. A typical example
   for condition would be (>= emacs-major-version 22).
   In any case, CONDITION must be valid for evaluation at
   either load or compile time.")


;;
;; Do Imports and Autoloads
;;

(import my-must-features       'require)
(import my-ought-features      'require-soft)
(import my-want-features       'require-soft)
(import my-autoloaded-features 'autoload)
(import my-special-features    (lambda (sym body) (eval body)))


;;
;; Automatic Mode Detection
;;

(add-to-list 'auto-mode-alist '("\\.[0-9]\\'" . fundamental-mode))
(add-to-list 'auto-mode-alist '("\\.\\([pP]\\([LlmM]?\\|od\\|erl\\)\\|al\\)\\'" . perl-mode))
(add-to-list 'auto-mode-alist 
             (cons (concat "\\." (regexp-opt '("xml" "xsd" "sch" "rng" "xslt" "svg" "rss") t) "\\'")
                   'nxml-mode))
(add-to-list 'auto-mode-alist '("\\.s?html?\\'" . nxhtml-mode))
(add-to-list 'auto-mode-alist '("\\.pde\\'" . arduino-mode))
(add-to-list 'auto-mode-alist '("\\.[sS]\\'" . S-mode))
(add-to-list 'auto-mode-alist '("\\.[rR]\\'" . R-mode))
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-to-list 'auto-mode-alist '("\\.txt\\'" . org-mode))

(fset 'xml-mode  'nxml-mode)    ;; should rhs be (symbol-function 'nxml-mode) ??
(fset 'html-mode 'nxhtml-mode)

(add-to-list 'magic-mode-alist '("<\\?xml " . nxml-mode))
(add-to-list 'magic-mode-alist '("#!.*perl" . perl-mode))
(add-to-list 'magic-mode-alist '("#!.*python" . python-mode))
(add-to-list 'magic-mode-alist '("#!.*\\(t?c\\|ba\\|k\\|z\\|\\)sh" . shell-script-mode))
(add-to-list 'magic-mode-alist '("\\\\input " . plain-TeX-mode))
(add-to-list 'magic-mode-alist '("\\\\document\\(style\\|class\\)" . LaTeX-mode))


;;
;; Help Configuration
;; 
;; Remapping Help Keys, as in (@> "Global Key Bindings") below,
;; requires some coordination for imported modes. So, describe
;; new help events here for later use.
;;

(setq my-help-events   ; try several for now and see
      (list [?\M-\C-h] [?\C-c ?h] [?\C-c ?\C-h]))  
(setq help-event-list ; only keys seem to be allowed
      (nconc (list ?\M-\C-h) help-event-list))
;;or eventually, if I settle on \M-\C-h, do:
;(setq help-char ?\M-\C-h)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                           ;;
;;(@* "Text Modes")                                          ;;;;;;
;;                                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Generic Text Modes

(defun my-generic-text-modes-hook ()
  "Sets up environment useful for a variety of text editing modes.
   Add this to the corresponding hooks."
  (turn-on-flyspell)
  (turn-on-local-comment-auto-fill))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Org Mode

(defun my-org-load-hook ()
  (setq org-startup-folded 'content) ; nil also good
  (setq org-cycle-separator-lines 2)
  (setq org-log-into-drawer t)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (R . t)
     (ditaa . t)))
  (setq org-todo-keywords
        '((sequence "TODO" "WAIT" "DONE")))
  (setq org-agenda-custom-commands '(("A" "Agenda for two week span" agenda ""
                                      ((org-agenda-span 14) (org-agenda-start-day "-1mon")))))
  (setq org-tags-column -80)
  (copy-face 'org-todo 'org-wait-face) ; bug with string when doing org-write-agenda
  (set-face-foreground 'org-wait-face "lightgoldenrod2")
  (setq org-todo-keyword-faces '(("WAIT" . org-wait-face))) 
  )

(defun my-org-mode-hook ()
  (local-set-key "\C-c\C-x\M-k" 'org-cut-special)
  (local-set-key "\C-c\C-x\M-c" 'org-copy-special)
  (local-set-key "\C-c\M-c"     'org-edit-src-code) ; see which I like best
  (local-set-key "\C-c\M-s"     'org-edit-src-code) ; C-c ' taken by icicles
  (local-set-key [\C-\M-return] 'org-insert-subheading)
  (local-set-key [\C-\M-\S-return] 'org-insert-todo-subheading)
  )

(add-hook 'org-load-hook 'my-org-load-hook)
(add-hook 'org-mode-hook 'turn-on-font-lock)
(add-hook 'org-mode-hook 'my-org-mode-hook)

;(setq org-todo-keywords
;      '((sequence "TODO(t)" "WAIT(w@)" "DONE(d)")
;        (sequence "READING" "REVIEWING" "RESPONDED")
;        (sequence "PREPARED" "DISPATCHED" "PROCESSED" "FINISHED")
;        (sequence "FOUND" "TESTING" "FIXING" "FIXED")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                           ;;
;;(@* "Programming Modes")                                   ;;;;;;
;;                                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Generic Programming Modes

(defun my-generic-prog-modes-hook ()
  "Sets up environment useful for a variety of programming modes.
   Add this to the corresponding hooks."
  (highlight-attn-words)
  (turn-on-local-comment-auto-fill))

(when (featurep 'header2)
  (setq header-copyright-notice "Copyright (C) 2010, Christopher R. Genovese, all rights reserved.\n")
  (setq header-date-format "%Y %b %d %a %H:%M:%S"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; IDE Configuration CEDET/ECB/JDEE  (necessary files loaded in imports.el)

(defun my-semantic-init-hook ()
  (global-semantic-idle-scheduler-mode '1)
  (semantic-load-enable-excessive-code-helpers)
  (require 'semantic-ia))

(add-hook 'semantic-init-hook 'my-semantic-init-hook)

(eval-after-load 'senator   ;; alternatively could put this in a hook
  '(progn
     (define-key senator-mode-map "\C-c,c"    'semantic-ia-describe-class)
     (define-key senator-mode-map "\C-c,d"    'semantic-ia-show-doc)
     (define-key senator-mode-map "\C-c,D"    'semantic-ia-show-summary)
     (define-key senator-mode-map "\C-c,e"    'eassist-list-methods)
     (define-key senator-mode-map "\C-c,F"    'semantic-ia-fast-jump)
     (define-key senator-mode-map "\C-c,l"    'semantic-complete-jump-local-members)
     (define-key senator-mode-map "\C-c,m"    'semantic-ia-complete-symbol)
     (define-key senator-mode-map "\C-c,M"    'semantic-ia-complete-symbol-menu)
     (define-key senator-mode-map "\C-c,s"    'semantic-symref)
     (define-key senator-mode-map "\C-c,S"    'semantic-symref-symbol)
     (define-key senator-mode-map "\C-c,x"    'semantic-symref-regexp)
     (define-key senator-mode-map "\C-c,y"    'semantic-ia-show-summary)
     (define-key senator-mode-map "\C-c,\C-k" 'senator-kill-tag)
     (define-key senator-mode-map "\C-c,\M-c" 'senator-copy-tag)
     (define-key senator-mode-map "\C-c,\C-w" nil)
     (define-key senator-mode-map "\C-c,\M-w" nil)
     (define-key senator-mode-map [?\C-c ?, return] 'semantic-mrub-switch-tags)
     ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Emacs-Lisp Mode

(defun my-emacs-lisp-mode-hook ()
  (local-set-key "\C-c\C-e" 'eval-defun)
  (local-set-key "\C-c\C-o" 'outline-minor-mode)
  (local-set-key "\C-c\C-s" 'eval-last-sexp)
  (defun emacs-lisp-outline-minor-setup ()
    (setq outline-regexp ";;; \\|;; \\|(....")
    (local-set-key "\C-c\C-m" outline-mode-prefix-map)
    (when (featurep 'org)
      (define-key outline-minor-mode-map [(control tab)] 'org-cycle)
      (define-key outline-minor-mode-map [(shift tab)]   'org-global-cycle))) 
  (make-local-variable 'outline-minor-mode-hook)
  (add-hook 'outline-minor-mode-hook 'emacs-lisp-outline-minor-setup))

(add-hook 'emacs-lisp-mode-hook 'highlight-attn-words)
(add-hook 'emacs-lisp-mode-hook 'my-emacs-lisp-mode-hook)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Lisp Interaction Mode

;; (This is for Emacs Lisp, see SLIME below.)
;; 

(defun my-lisp-interaction-mode-hook ()
  (local-set-key "\C-j"     'newline-and-indent)
  (local-set-key [C-return] 'eval-print-last-sexp)
  (local-set-key "\C-c\C-c" 'eval-print-last-sexp))

(add-hook 'lisp-interaction-mode-hook 'my-lisp-interaction-mode-hook)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Python Mode

(defun my-python-mode-hook ()
  (local-set-key "\M-\C-a"  'beginning-of-python-def-or-class)
  )

(add-hook 'python-mode-hook 'my-python-mode-hook)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Common C Modes (cc-mode) (C, C++, Java)

(autoload 'c-mode    "cc-mode" "C Editing Mode" t)    ;; Use cc-mode for all C editing
(autoload 'c++-mode  "cc-mode" "C++ Editing Mode" t)
(autoload 'java-mode "cc-mode" "Java Editing Mode" t)

(defun my-c-mode-common-hook ()
  (if (not (assoc "CRG" c-style-alist))
      (c-add-style "CRG" (cdr c-crg-style)))
  (c-set-style "CRG")
  (if (assq 'c-auto-hungry-string minor-mode-alist)   ;; Inhibit minor mode notation
      (setq minor-mode-alist
	    (cons '(c-auto-hungry-string "") (remove-matching-elements 'c-auto-hungry-string minor-mode-alist 'eq-car))))
  (local-set-key "\C-c\C-a" 'c-beginning-of-statement)
  (local-set-key "\C-c\C-e" 'c-end-of-statement)
  (local-set-key "\C-c\C-b" 'c-backward-conditional)
  (local-set-key "\C-c\C-f" 'c-forward-conditional)
  (local-set-key "\C-c\C-c" 'comment-region)
  (local-set-key "\M-q"     'c-fill-paragraph)
  (local-set-key "\C-c\C-d" 'c-down-conditional)
  (local-set-key "\C-c\C-u" 'c-up-conditional)
  (local-set-key "\C-c\C-i" 'c-indent-exp)
  (local-set-key "\C-c\C-q" 'c-indent-defun)
  (local-set-key "\C-c\C-m" 'compile);; m for make
  (local-set-key "\C-c\C-n" 'up-list)
  (local-set-key "\C-c\C-p" 'c-up-list-neg)
  (local-set-key "\C-c\C-o" 'outline-minor-mode)
  (local-set-key "\C-c\C-t" 'c-set-offset)
  (local-set-key "\C-c\C-x" 'c-macro-expand)
  (local-set-key [f4]       'c-fill-paragraph)
  )
  
(add-hook 'c-mode-common-hook 'highlight-attn-words)
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

(defvar c-crg-style
  '("CRG"
    (c-basic-offset . 4)
    (c-comment-only-line-offset . (0 . 0))
    (c-offsets-alist . ((statement-block-intro . +)
			(substatement-open . 0)
			(label . 0)
			(case-label . *)
			(access-label . /)
			(statement-case-intro . *)
			(statement-cont . +)
			(cpp-macro . 0)
                        (inline-open . 0)
			(inher-intro . c-lineup-inher-intro)
			))
    (c-auto-newline . t)
    (c-hanging-braces-alist . ((brace-list-open)
			       (brace-list-close)
			       (substatement-open before after)
			       (block-close before after)))
    (c-hanging-comment-ender-p . nil)
    (c-cleanup-list . '(scope-operator list-close-comma))
    )
  "My personal formatting style for editing C/C++/Java source.")

(defun c-up-list-neg (arg)
  "Like up-list but with a negated arg"
  (interactive "P")
  (if (not arg)
      (up-list -1)
    (up-list arg)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; C-mode (old)

(defun my-c-mode-hook ()
  (setq indent-tabs-mode nil)
  (local-set-key "\C-c\C-z" 'compilation-close-compilation)
  )

(add-hook 'c-mode-hook 'my-c-mode-hook)

(defvar boc-mode-hook                    
  '((lambda ()
      (setq c-indent-level '4)
      (setq c-continued-statement-offset '4)
      (setq c-continued-brace-offset '-4)
      (setq c-brace-offset '0)
      (setq c-brace-imaginary-offset '0)
      (setq indent-tabs-mode 'nil) 
      (setq c-label-offset '-2)
      (setq c-auto-newline t)
      ))
  "Style hook for \"Boring Old C-mode\" in case
it is needed to run an earlier version of emacs.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; C++-mode

(defun my-c++-mode-hook ()
  (setq indent-tabs-mode nil)
  (local-set-key "\C-c\C-z" 'compilation-close-compilation)
  )

(add-hook 'c++-mode-hook 'my-c++-mode-hook)

(defun c-lineup-inher-intro (langelem)
  "Line up inheritence intro : two characters past class keyword"
  (+ c-basic-offset (/ c-basic-offset 2))) 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; java-mode

(defun my-java-mode-hook ()
  (setq indent-tabs-mode nil)
  )

(add-hook 'java-mode-hook 'my-java-mode-hook)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; compilation-mode

(defun my-compilation-mode-hook ()
  (local-set-key "\C-c\C-z" 'compilation-close-compilation)
  )

(add-hook 'compilation-mode-hook 'my-compilation-mode-hook)

(defun compilation-close-compilation (arg)
  "Iconifies compilation pop-up frame.  With prefix arg,
kills *compilation* buffer and thus its dedicated frame,
if the latter exists."
  (interactive "P")
  (let* (err-msg buf)
    (setq buf (condition-case err-msg
		  (setq buf (compilation-find-buffer))
		(error (and (message (car (cdr err-msg))) nil))))
    (if (and buf (not arg))
	(let ((win (get-buffer-window buf t)))
	  (if win
	      (iconify-frame (window-frame win))))
      (if (and buf arg)
	  (kill-buffer buf))
      )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; perl-mode


(defun my-perl-mode-hook ()
  (setq perl-indent-level '4)
  (setq perl-continued-statement-offset '4)
  (setq perl-continued-brace-offset '-4)
  (setq perl-brace-offset '0)
  (setq perl-brace-imaginary-offset '0)
  (setq indent-tabs-mode 'nil) 
  (setq perl-label-offset '-2)
  )

(add-hook 'perl-mode-hook 'highlight-attn-words)
(add-hook 'perl-mode-hook 'my-perl-mode-hook)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; OCaml tuareg-mode

    ;; no hook yet...


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SLIME

;; Added Functionality
;;
;; SLIME works with a variety of common lisp implementations, and more
;; recently with clojure. However, the interface to clojure -- in
;; particular, how the process is started or stopped -- is different
;; than for the common lisp implementations. The interface defined below
;; works for both.
;;
;; Use `slime/register-a-lisp' to register an implementation as avaiable
;; and indicate any processing that needs to be done to start and stop
;; the process. (See the documentation for this function for discussion
;; of the nontrivial options.) Then, `slime/start' and `slime/end' do
;; the necessary work: making connections, starting and stopping the
;; processes, managing the various buffers.
;;
;; The configuration variable `slime/default-lisp' determines which
;; implementation is used by default. When this is nil, the most
;; recently registered implementation is used, unless `slime/start'
;; is invoked with a prefix argument, in which case the user is
;; prompted for the implementation.
;;
;; Also, see `slime-clojure' for the function that handles setup for the
;; clojure implementation.
;;

(defvar slime/default-lisp nil
  "Default lisp to use when invoking slime
through the new interface. Value should be
either a symbol in `slime/registered-lisps'
or nil, in which case the first registered
lisp will be used.")

(defvar slime/registered-lisps nil
  "List of symbols representing registered
lisp implementations available through this
interface.")

(defvar slime/running nil
  "List of symbols representing lisp implementations
that appear to be currently running. Any actions
taken on the basis of this list should deal gracefully
with the possiblility that the user might have killed
the corresponding processes another way.")

(defun* slime/register-a-lisp (lisp-sym
                               &key (name (symbol-name lisp-sym))
                               &key (command (list name))
                               &key call
                               &key (body (cond
                                           (call
                                            `((,call)))
                                           (command
                                            `((slime ',lisp-sym)))
                                           (t
                                            '((message "Slime invocation missing."))))
                                          body-given?)
                               &key quit)
  "Create and configure a new SLIME lisp implementation and
create the necessary callbacks for running it.

LISP-SYM is a symbol to be associated with the registered lisp.
Unless the :CALL or :BODY keywords are supplied, LISP-SYM is used
as the key in the alist `slime-lisp-implementations'.

:NAME determines the name of the function to start the given lisp
in slime. If this keyword is supplied, its value should be a
string. The invocation function is called `slime/start-NAME',
where NAME is the associated value. By default, NAME is the
symbol-name of LISP-SYM.

:COMMAND should be a list of strings representing a shell command
to invoke in order to begin the associated lisp process. By
default it is a singleton list consisting of NAME. Unless
the :CALL or :BODY keywords are supplied in the call, this
command list will be entered as the value associated with
LISP-SYM in `slime-lisp-implementations.

:CALL is a shortcut for defining a function that invokes the
given lisp when a command in `slime-lisp-implementations' is
insufficient. If this keyword is supplied, its value should be a
symbol with a non-void function definition. This function will be
called when the given lisp is invoked. When missing, the :BODY
keyword defines this function, and more generally.

:BODY defines a function to invoke the given lisp when a command
in `slime-lisp-implementations' is insufficient. If this keyword
is supplied, its value should be a list of S-expressions that
will comprise the body of the function slime/start-NAME. If :BODY
is not supplied, then the :CALL keyword takes precedence in
defining the function. If both :BODY and :CALL are missing, then
the function invokes `slime' with the symbol LISP-SYM.

:QUIT defines the body of a function to invoke when the given
lisp is stopped. If this keyword is supplied, its value should
have the same structure as that of the :BODY keyword, namely a
list of S-expressions. These will be put in a function
`slime/stop-NAME'.

Note that any function namde `slime/start-NAME' will have its
function definition overridden.
"
  (when (not (symbolp lisp-sym))
    (error "add-slime-lisp argument LISP-SYM must be a symbol"))

  ; Create functions to start and end the given lisp
  (let ((ssym (intern (concat "slime/start-" name)))
        (esym (intern (concat "slime/stop-"  name))))
    (fset ssym
          `(lambda ()
             (interactive)
             ,@body
             (add-to-list 'slime/running ',lisp-sym))) 
    (fset esym
          `(lambda (&optional kill-it?)
             (interactive "P")
             (condition-case nil
                 (slime-quit-lisp t)
               (error nil))
             (sit-for 1)
             (message ,(concat "Stopping " (symbol-name lisp-sym)))
             ,@quit
             (my-awhen (get-buffer ,(concat "*slime-repl " name "*"))
               (if kill-it?
                   (kill-buffer it)
                 (with-current-buffer it
                   (goto-char (point-max))
                   (insert "\n\nFinished.\n"))))
             (setq slime/running (remove-matching-elements ',lisp-sym slime/running 'eq)))))

  ; Add lisp to slime's implementation list
  ; unless an explicit function body was given
  (when (and (not body-given?) (not call))
    (add-to-list 'slime-lisp-implementations
                 (cons lisp-sym (list command))))
  (add-to-list 'slime/registered-lisps lisp-sym))

(defun slime/validate-lisp-descriptor (which-lisp prefix-arg?)
  "Construct the symbol and its name from a descriptor
and an optional prefix arg. The descriptor WHICH-LISP,
if non-nil, should be a symbol in `slime/registered-lisps' or a string giving
the name of such a symbol. If WHICH-LISP is nil,
the default implementation is used (see `slime/default-lisp')
when called from a lisp program or when called interactively
without a prefix arg. When called interactively with a prefix
arg, the implementation to use is read from the minibuffer.
Other values for which-lisp raise an error; an error is also
raised if no default value can be constructed when needed.

Returns a cons cell of the form (SYMBOL NAME).
"
  (let (sym name)
    (cond
     ((and (null which-lisp) prefix-arg?)
      (setq name (completing-read
                  "Lisp implementation: "
                  (mapcar #'symbol-name slime/registered-lisps)
                  nil t))
      (setq sym (intern name)))
     ((null which-lisp)
      (setq sym (or slime/default-lisp (car-safe slime/registered-lisps)))
      (setq name (and sym (symbol-name sym))))
     ((stringp which-lisp)
      (setq name which-lisp)
      (setq sym (intern name)))
     ((symbolp which-lisp)
      (setq sym which-lisp)
      (setq name (symbol-name sym)))
     (t
      (error (concat "Invalid lisp implementation, "
                     (prin1-to-string which-lisp)))))
    (when (null sym)
      (error "No lisp implementations registered"))
    (when (null (memq sym slime/registered-lisps))
      (error "Default lisp has not been registered"))
    (list sym name)))

(defun slime/start (&optional which-lisp)
  "Start a lisp implementation in slime. WHICH-LISP, if non-nil,
should be a symbol in `slime/registered-lisps' or a string giving
the name of such a symbol. If WHICH-LISP is nil,
the default implementation is used (see `slime/default-lisp')
when called from a lisp program or when called interactively
without a prefix arg. When called interactively with a prefix
arg, the implementation to use is read from the minibuffer.
"
  (interactive)
  (when (null slime/registered-lisps)
    (error "No Registered Lisp Implementations Available"))

  (let* ((lisp-impl
          (second (slime/validate-lisp-descriptor
                   which-lisp current-prefix-arg)))
         (fsym (intern (concat "slime/start-" lisp-impl))))
    (funcall fsym)))

(defun slime/stop (&optional which-lisp kill-the-buffer?)
  "Stop a running lisp implementation in slime. When called from
a lisp program, WHICH-LISP should either be nil or should be a
symbol in `slime/registered-lisps' or a string giving the name of
such a symbol. If WHICH-LISP is nil, the implementation is read
from the minibuffer with the most recently run as the initial
input. If KILL-THE-BUFFER? is non-nil, the repl buffer is killed,
otherwise a completion message is printed at the end of that
buffer.
"
  (interactive
   (list
    (completing-read "Quit which lisp? "
                     (mapcar #'symbol-name slime/registered-lisps)
                     nil t nil nil
                     (symbol-name (car slime/running)))
    current-prefix-arg))
  (when (null slime/registered-lisps)
    (error "No Registered Lisp Implementations Available"))

  (let* ((lisp-impl
          (second (slime/validate-lisp-descriptor
                   which-lisp current-prefix-arg)))
         (fsym (intern (concat "slime/stop-" lisp-impl))))
    (funcall fsym kill-the-buffer?)))

;; Clojure support

(defvar slime-swank-clojure-logbuf-name "*swank-clojure-log*"
  "Name of buffer associated with the swank-clojure process. All
output and error messages of the process are logged in this
buffer.")

(defvar slime-swank-clojure-command "swank-clojure"
  "Command to start an asynchronous swank process with which
emacs/slime communicates.")

(defvar slime-swank-clojure-timeout 15
  "Number of seconds to wait for server before asking user if we
should try again.")

(defun slime-clojure (&optional read-args?)
  (interactive "P")
  (let ((logbuf (get-buffer-create slime-swank-clojure-logbuf-name))
        (host "127.0.0.1")
        (port 4005)
        (ready nil))
    (when read-args?
      (setq host (read-from-minibuffer
                  "Host: " (first slime-connect-host-history)
                  nil nil '(slime-connect-host-history . 1)))
      (setq port (string-to-number
                  (read-from-minibuffer
                   "Port: " (first slime-connect-port-history)
                   nil nil '(slime-connect-port-history . 1)))))
    (message "Starting swank server...")
    (async-shell-command slime-swank-clojure-command logbuf logbuf)
    (bury-buffer logbuf)
    ; Wait for swank server to acknowledge with buffer output
    (let ((wait-time 0)
          (keep-waiting t))
      (while (and keep-waiting
                  (= 1 (with-current-buffer slime-swank-clojure-logbuf-name
                         (point-max))))
        (sit-for 1)
        (setq wait-time (1+ wait-time))
        (when (> wait-time slime-swank-clojure-timeout)
          (if (y-or-no-p "No response yet from swank server. Shall I try again? ")
              (setq wait-time 0)
            (setq keep-waiting nil))))
      (when keep-waiting (setq ready t)))
    (if (not ready)
        (message "No response from swank server. Giving up.")
      (sit-for 2)
      (message "Connecting to swank server...")
      (slime-connect host port)
      (sit-for 1)
      (message "Ready."))))


;; Configuration

(setq inferior-lisp-program "sbcl")
(setq slime-default-lisp 'sbcl)
(setq slime-lisp-implementations nil)

(slime/register-a-lisp 'cmucl)
(slime/register-a-lisp 'sbcl)
(slime/register-a-lisp 'clojure
                       :call 'slime-clojure
                       :quit '((my-awhen (get-buffer slime-swank-clojure-logbuf-name)
                                 (my-awhen (get-buffer-process it)
                                   (kill-process it))
                                 (kill-buffer it))))
(setq slime/default-lisp 'clojure)

(with-library 'slime-autoloads
  (slime-setup '(slime-repl)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                           ;;
;;(@* "Shell-based Modes")                                   ;;;;;;
;;                                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ipython


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; comint-mode 

;; Added Functionality
;;
;; * Beginning of line that alternates between true beginnng
;;   and prompt. Command `my-comint-bol'.
;;
;; * DWIM search of input history or text, using comint
;;   functionality and a prefix arg to determine which.
;;   See `my-comint-dwim-isearch-backward'.
;;   

(defun my-comint-bol ()
  "Goes to the beginning of line, then skips past the prompt, if any.
However, if already at the prompt, the prompt is not skipped.
This has the side-effect of skipping the prompt if you start at the
beginning of a line with a prompt.  This oscillation between the two
possible beginnings is useful.

The prompt skip is done by skipping text matching the regular expression
`comint-prompt-regexp', a buffer local variable."
  (interactive)
  (let ((now (point))
	(eol (save-excursion (end-of-line) (point))))
    ;; (beginning-of-line)  ;; ATTN: this got confused by "field boundaries" in later versions of emacs
    (forward-line 0)
    (if (looking-at comint-prompt-regexp)
	(if (and (not (= (match-end 0) now))
		 (<= (match-end 0) eol))
	    (goto-char (match-end 0))))))

(defun my-comint-dwim-isearch-backward (&optional use-hist-regexp?)
  "Search incrementally backward for a string or regexp, using
either the input history or the buffer text. If point is at
or after the final prompt line (i.e., the process mark),
then search the input history; otherwise, do a regular isearch
backward in the buffer text. When searching history, the prefix
argument (or regular argument noninteractively) determines whether
a string or regexp search is used. Nil prefix arg gives a
regular string search; non-nil gives a regexp search. When
searching the buffer contents, regexp search is always used."
  (interactive)
  (let ((on-prompt-line? (comint-after-pmark-p))
        (comint-history-isearch 'dwim))
    (if (or (not on-prompt-line?) use-hist-regexp?)
        (isearch-backward-regexp)
      (isearch-backward))))

;; Configuration

(defun my-comint-mode-hook ()
  (setq comint-input-ring-size '1024)
  (setq comint-input-ignoredups t)
  (local-set-key "\C-a"         'my-comint-bol)
  (local-set-key "\M-r"         'my-comint-dwim-isearch-backward) 
  (local-set-key [?\C-c return] 'comint-copy-old-input)
  (local-set-key "\C-c\C-m"     nil)  ;; used globally for extended command
  (add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)
  )
       
(add-hook 'comint-mode-hook 'my-comint-mode-hook)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; shell-mode

;; Added Functionality (tcsh-style)
;;
;; * tcsh-style directory tracking and stack management
;;
;;   Features:
;;    1. Support =n and =- references to directory stack
;;       (without expansion) as in tcsh
;;    2. Fixed expansion of =n and =- (when instigated)
;;       to prevent missing slash
;;    3. Sets shell-last-dir properly so  cd - works
;;       as in tcsh
;;    4. Fixed bug in shell-resync-dirs in case dir
;;       tracking gets lost
;;    5. Improved handling of aliases for HOME directory.
;;
;;   This is old code -- in fact my first real emacs-lisp code, so it's a
;;   bit clunky. But I've been using it for years without problem, and it
;;   really makes moving around in the shell more pleasant.
;; 
;; * Improved shell history autoexpand
;;
;;   See `shell-toggle-autoexpand'.
;;

(defvar shell-pushd-silent nil
  "*If non-nil, show dirstack in minibuffer with every change
of the directory stack.  This mimicks the optional behavior
of tcsh.")

(defvar shell-backup-input-autoexpand (cons nil 'input)
  "*List of autoexpand states among which the user can toggle with
shell-toggle-autoexpand.")

(defvar shell-home-aliases (if (boundp 'my-home-aliases)
                               my-home-aliases
                             (if (getenv "HOME")
                                 (list (concat (getenv "HOME") "/"))
                               nil))
  "*This is a list of strings indicating possible aliases for the users
HOME directory that emacs might not recognize.  This is most useful with
cross-mounted file systems, and is used by shell-map-home-aliases to 
determine whether or not the directory should be changed to \"~/\".")

(defun use-my-shell-ext ()
  "Set selected shell mode functions to my extended versions."
  (fset 'shell-resync-dirs
        (symbol-function 'my-shell-resync-dirs))
  (fset 'shell-directory-tracker
        (symbol-function 'my-shell-directory-tracker))
  (fset 'shell-process-cd
        (symbol-function 'my-shell-process-cd))
  (fset 'shell-process-pushd
        (symbol-function 'my-shell-process-pushd))
  (fset 'shell-dirstack-message
        (symbol-function 'my-shell-dirstack-message))
  (fset 'shell-replace-by-expanded-directory
        (symbol-function 'my-shell-replace-by-expanded-directory)) )

(defun shell-toggle-autoexpand ()
 "Toggle history auto-expansion to eliminate strange behavior in interactive
  applications run within the shell.  The list `shell-backup-input-autoexpand'
  stores the list of values to be toggled among.  At the moment, don't set
  shell/comint-input-autoexpand without changing this list as well."
  (interactive)
  (let ((nextia  (car shell-backup-input-autoexpand))
        (otheria (cdr shell-backup-input-autoexpand))
        (curia   comint-input-autoexpand))
    (setq comint-input-autoexpand nextia)
    (setq shell-backup-input-autoexpand (cons otheria nextia))
    (if nextia
       (message (format "Setting Input Autoexpand to %s" nextia))
       (message "Turning Off Input Autoexpand"))))

 ; Additions and fixes to shell directory tracking mechanism,
 ; based on the code in shell.el.

(defun shell-map-home-aliases (indir &optional noslash)
  "Check if DIR matches one of the aliases for HOME
and thus should be replaced by \"~/\" for consistency."
  (let ((dir indir))
    (if (not (string-match "/$" dir))
	(setq dir (concat dir "/")))
    (if (and shell-home-aliases (member dir shell-home-aliases))
	"~/"
      (if noslash indir dir)) ))

(defun my-shell-resync-dirs ()
  "Resync the buffer's idea of the current directory stack.
This command queries the shell with the command bound to 
`shell-dirstack-query' (default \"dirs\"), reads the next
line output and parses it to form the new directory stack.
DON'T issue this command unless the buffer is at a shell prompt.
Also, note that if some other subprocess decides to do output
immediately after the query, its output will be taken as the
new directory stack -- you lose. If this happens, just do the
command again."
  (interactive)
  (let* ((proc (get-buffer-process (current-buffer)))
	 (pmark (process-mark proc)))
    (goto-char pmark)
    (sit-for 0) ; force redisplay
    (comint-send-string proc shell-dirstack-query) 
    (comint-send-string proc "\n")
    (set-marker pmark (point))
    (let ((pt (point))) ; wait for 1 line
      ;; This extra newline prevents the user's pending input from spoofing us.
      (insert "\n") (backward-char 1)
      (while (not (looking-at ".+\n"))
	(accept-process-output proc)
	(goto-char pt))
      (if (looking-at (concat shell-dirstack-query "\n"))
	  (accept-process-output proc)))
    (goto-char pmark) (delete-char 1) ; remove the extra newline
    (forward-line -1)
    ;; That's the dirlist. grab it & parse it.
;    (let* ((dl (buffer-substring (match-beginning 0) (1- (match-end 0))))
    (let* ((dl (buffer-substring (point) (save-excursion (end-of-line) (point))))
	   (dl-len (length dl))
	   (ds nil)			; new dir stack
	   (i 0)
	   new-dir)
      (while (< i dl-len)
	;; regexp = optional whitespace, (non-whitespace), optional whitespace
	(string-match "\\s *\\(\\S +\\)\\s *" dl i) ; pick off next dir
	(setq new-dir (concat comint-file-name-prefix
			      (substring dl (match-beginning 1) (match-end 1))))
	(setq i (match-end 0))
	(if (not (string-match "/$" new-dir))
	    (setq new-dir (concat new-dir "/")))
	(setq ds (cons new-dir ds)) )
      (goto-char (process-mark proc))
      (let ((ds (nreverse ds)))
	(condition-case nil
	    (progn (shell-cd (car ds))
		   (setq shell-dirstack (cdr ds))
		   (my-shell-dirstack-message t))
	  (error (message (format "Could not cd to %s." (car ds)))))))))

(defun my-shell-directory-tracker (str)
  "Tracks cd, pushd and popd commands issued to the shell.
This function is called on each input passed to the shell.
It watches for cd, pushd and popd commands and sets the buffer's
default directory to track these commands.

You may toggle this tracking on and off with M-x dirtrack-toggle.
If emacs gets confused, you can resync with the shell with M-x dirs.

See variables `shell-cd-regexp', `shell-pushd-regexp', and `shell-popd-regexp',
while `shell-pushd-tohome', `shell-pushd-dextract' and `shell-pushd-dunique'
control the behavior of the relevant command.

Environment variables are expanded, see function `substitute-in-file-name'."
  (if shell-dirtrackp
      ;; We fail gracefully if we think the command will fail in the shell.
      (condition-case chdir-failure
	  (let ((start (progn (string-match "^[; \t]*" str) ; skip whitespace
			      (match-end 0)))
		(def-dir  default-directory)
		end cmd arg1)
	    (while (string-match shell-command-regexp str start)
	      (setq end (match-end 0)
		    cmd (comint-arguments (substring str start end) 0 0)
		    arg1 (comint-arguments (substring str start end) 1 1))
	      (cond ((string-match (concat "\\`\\(" shell-popd-regexp "\\)\\($\\|[ \t]\\)") cmd)
		     (shell-process-popd (substitute-in-file-name arg1)))
		    ((string-match (concat "\\`\\(" shell-pushd-regexp "\\)\\($\\|[ \t]\\)") cmd)
		     (shell-process-pushd (shell-map-home-aliases
					   (shell-replace-stack-dir
					    (substitute-in-file-name arg1))
					   t)))
		    ((string-match (concat "\\`\\(" shell-cd-regexp "\\)\\($\\|[ \t]\\)") cmd)
		     (shell-process-cd (shell-map-home-aliases
					(shell-replace-stack-dir
					 (substitute-in-file-name arg1))
					t))))
	      (if (equal def-dir default-directory)
		  nil
		(setq shell-last-dir def-dir)
		(setq def-dir default-directory))
	      (setq start (progn (string-match "[; \t]*" str end) ; skip again
				 (match-end 0)))))
	    (error (message (format "Could not cd: %s" (car (cdr chdir-failure))))))))

(defun shell-replace-stack-dir (str &optional nomsg defdir)
  "Replace reference to directory stack in given argument by its
value.  Leading parts of STR matching -, =n, =- (for n >= 0) are
replaced by the corresponding directory element (last dir, nth on
stack, last on stack, resp); otherwise STR itself is returned. Strings
of the form +n are not processed since, depending on variables like
'shell-pushd-dextract', these might require special treatment.
Replacement handles the addition of '/', so it is not necessary that
directory elements end in '/'.  If n above is larger than the
directory stack length, a message to that effect is printed, unless
NOMSG is non-nil.  When an ERROR occurs or when a 0 reference into the
stack is given, returns DEFDIR, which defaults to default-directory if
not provided."
  (let* ((def-dir (or defdir default-directory))
	 (stack   (cons def-dir shell-dirstack))
	 (stacklen (length stack))
	 (num (and (stringp str)
		   (or (and (string-match "^=\\([0-9]+\\)\\(/.*\\|\\s \\|$\\)" str)
			    (string-to-number (substring str (match-beginning 1) (match-end 1))))
		       (and (string-match "^=-\\(/.*\\|\\s \\|$\\)" str) (1- stacklen)))))
	 dir-head
	 dir-tail)
    (cond ((and (stringp str) (string-equal "-" str)) shell-last-dir)
	  ((null num) str)
	  ((>= num stacklen)
	   (if (not nomsg)
	       (signal 'error
		       (list (format "Directory stack not that deep (%d)." num))))
	   def-dir)
	  ((string-match "^=\\(\\([0-9]+\\|-\\)/?\\)" str) ;; assumes dirstack entries end in '/'
	   (setq dir-head (nth num stack))
	   (setq dir-tail (substring str (match-end 1)))
	   (concat dir-head (if (not (string-match "/$" dir-head)) "/") dir-tail))
	  (t str))))

; cd [dir]
(defun my-shell-process-cd (arg)
  (let ((new-dir (cond ((zerop (length arg)) (concat comint-file-name-prefix
						     "~/"))
		       ((string-equal "-" arg) shell-last-dir)
		       (t (shell-prefixed-directory-name arg)))))
    (shell-cd new-dir)
    (shell-dirstack-message)))

; pushd [+n | dir]
(defun my-shell-process-pushd (arg)
  (let ((num (shell-extract-num arg)))
    (cond ((zerop (length arg))
	   ;; no arg -- swap pwd and car of stack unless shell-pushd-tohome
	   (cond (shell-pushd-tohome
		  (shell-process-pushd (concat comint-file-name-prefix "~/")))
		 (shell-dirstack
		  (let ((old default-directory))
		    (shell-cd (car shell-dirstack))
		    (setq shell-dirstack
			  (cons old (cdr shell-dirstack)))
		    (shell-dirstack-message)))
		 (t
		  (message "Directory stack empty."))))
	  ((numberp num)
	   ;; pushd +n
	   (cond ((> num (length shell-dirstack))
		  (message "Directory stack not that deep."))
		 ((= num 0)
		  (error (message "Couldn't cd.")))
		 (shell-pushd-dextract
		  (let ((dir (nth (1- num) shell-dirstack)))
		    (shell-process-popd arg)
		    (shell-process-pushd dir)))
		 (t
		  (let* ((ds (cons default-directory shell-dirstack))
			 (dslen (length ds))
			 (front (nthcdr num ds))
			 (back (reverse (nthcdr (- dslen num) (reverse ds))))
			 (new-ds (append front back))
			 (new-dir (car new-ds)))
		    (shell-cd new-dir)
		    (setq shell-dirstack (cdr new-ds))
		    (shell-dirstack-message)))))
	  (t
	   ;; pushd <dir>
	   (let* ((old-wd default-directory)
		 (dir (shell-prefixed-directory-name arg))
		 (ex-dir (expand-directory-name dir)))
	     (if (not (string-match "/$" dir))
		 (setq dir (concat dir "/")))
	     (shell-cd dir)
	     (setq shell-dirstack (cons old-wd shell-dirstack))
	     (if (and shell-pushd-dunique
		      (member ex-dir
			      (mapcar 'expand-directory-name shell-dirstack)))
		 (setq shell-dirstack
		       (remove-matching-elements ex-dir shell-dirstack
						 'equal-dir-name))))
	   (shell-dirstack-message)))))

(defun my-shell-dirstack-message (&optional show)
  "Show the current dirstack on the message line.
If the variable shell-pushd-silent is non-nil, then no message
is shown.  This mimicks the optional behavior of tcsh."
  (if (and shell-pushd-silent (null show))
      nil
    (let* ((msg "")
	   (ds (cons default-directory shell-dirstack))
	   (home (expand-file-name (concat comint-file-name-prefix "~/")))
	   (homelen (length home)))
      (while ds
	(let ((dir (car ds)))
	  (and (>= (length dir) homelen) (string= home (substring dir 0 homelen))
	       (setq dir (concat "~/" (substring dir homelen))))
	  ;; Strip off comint-file-name-prefix if present.
	  (and comint-file-name-prefix
	       (>= (length dir) (length comint-file-name-prefix))
	       (string= comint-file-name-prefix
			(substring dir 0 (length comint-file-name-prefix)))
	       (setq dir (substring dir (length comint-file-name-prefix)))
	       (setcar ds dir))
	  (setq msg (concat msg (directory-file-name dir) " "))
	  (setq ds (cdr ds))))
      (message "%s" msg))))

(defun my-shell-replace-by-expanded-directory ()
  "Expand directory stack reference before point.
Directory stack references are of the form \"=digit\" or \"=-\".
See `default-directory' and `shell-dirstack'.

Now handles trailing / correctly.

Returns t if successful."
  (interactive)
  (if (comint-match-partial-filename)
      (save-excursion
	(goto-char (match-beginning 0))
	(let ((stack (cons default-directory shell-dirstack))
	      (index (cond ((looking-at "=-/?")
			    (length shell-dirstack))
			   ((looking-at "=\\([0-9]+\\)/?")
			    (string-to-number
			     (buffer-substring
			      (match-beginning 1) (match-end 1)))))))
	  (cond ((null index)
		 nil)
		((>= index (length stack))
		 (error "Directory stack not that deep."))
		(t
		 (replace-match (file-name-as-directory (nth index stack)) t t)
		 (message "Directory item: %d" index)
		 t))))))

;; Configuration

(setq-default shell-file-name "/bin/tcsh")

(defun my-shell-mode-hook ()
  ;; (require 'my-shell-ext)
  (local-set-key "\C-c\C-q" 'shell-toggle-autoexpand)
  (local-set-key "\C-a" 'my-comint-bol)
  (setq shell-pushd-regexp "\\(pd\\|cd\\|pushd\\)")
  (setq shell-cd-regexp    "chdir")
  (setq shell-popd-regexp  "\\(pf\\|popd\\)")
  (setq shell-prompt-pattern "^[^#%$>]*[#$%>]>? *" )
  (setq shell-pushd-dextract t)
  (setq shell-pushd-dunique t)
  (setq shell-pushd-silent nil)
  (setq shell-pushd-tohome t)
  (setq comint-process-echoes t)
  (setq comint-input-autoexpand 'input)
  (setq shell-input-autoexpand 'input)
  (setq shell-backup-input-autoexpand (cons nil shell-input-autoexpand))
  ;;
  ;; Check that starting directory is correct on startup
  ;;
  (setq default-directory (shell-map-home-aliases default-directory))
  ;;
  ;; Include cwd on the mode line
  ;;
  (make-local-variable 'mode-line-format)
  (setq mode-line-format
        (reverse (append
                  (list " -%-" 'default-directory "-- ")
                  (cdr (reverse (default-value 'mode-line-format))))))
  ;;
  ;; Configure Output Filter 
  ;;
  (add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt nil t)
  ;;
  ;; Set-up replacements for shell.el functions, mine given below
  ;;
  (use-my-shell-ext)
  )

(add-hook 'shell-mode-hook 'my-shell-mode-hook)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                           ;;
;;(@* "Navigation and Action Modes")                         ;;;;;;
;;                                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Dired-mode

;; Added Functionality 
;;
;; * Copy or Move file/dir from elsewhere to current subdirectory:
;;
;;   Dired standard functionality makes it easy to copy or move
;;   files *from* the directory represented by the buffer, but
;;   to copy files *to* the current dired buffer is not as easy.
;;   One would have to open another dired buffer and then do
;;   the copy/move. The commands `my-dired-move-file-from-elsewhere'
;;   and `my-dired-copy-file-from-elsewhere' are used to do that.
;;   They move/copy files or directories (possibly recursively)
;;   to the directory containing point in the current dired buffer.
;;   The names are read from the minibuffer with completion and
;;   (hopefully) reasonable defaults. See the documentation.
;;
;; * Open file, or directory in same dired buffer, on return:
;;     
;;   Ordinary behavior opens a new dired buffer for subdirectories
;;   but this is often inconvenient, especially because my
;;   navigation setup makes it very quick to move within a buffer.
;;   Command `my-dired-find-file-or-subdir' acts like `dired-find-file'
;;   but opens directories in the same buffer. This also allows
;;   an option for listing switches to be modified for the
;;   subdirectory listed in this buffer.
;;
;; * Create new directory or new empty file in current subdirectory:
;;
;;   If a sub-directory is listed in the same buffer, opening a new
;;   file in that directory is annoying because find-file gives
;;   the parent as a default location, requiring extra typing.
;;   The commands `my-dired-create-directory-or-file' and
;;   `my-dired-create-file-or-directory' do exactly this.
;;   The two commands are the same except for the preference
;;   given to file or directory. The former creates a subdirectory
;;   without a prefix arg and an empty file with a prefix arg.
;;   The latter does the opposite. Point is left on the new file
;;   making it easier to immediately edit the new entry. This
;;   is surprisingly useful!
;;
;; * Mark files by regexp matching against the full path name
;;
;;   In directories with many files and subdirectories, it is
;;   often easier to mark the files you want quickly by including
;;   information from the full path. (This is especially true
;;   when multiple subdirectories are represented in one dired
;;   buffer.) The standard `dired-mark-files-regexp' does not
;;   do this, so I've included `my-dired-mark-files-path-regexp'
;;   which does. This function does remove the prefix common
;;   to all sub-directories represented in the buffer as that
;;   prefix has no discriminating power. As such without
;;   any subdirectories this is the same.  Currently bind this
;;   to %p below but consider replacing the current mark
;;   function.
;;
;; * Move cyclically among subdirectories:
;; 
;;   The standard subdir movement commands in dired give an error
;;   at the first or last subdir of the buffer. The alternate commands
;;   `my-dired-next-subdir' and `my-dired-prev-subdir' instead
;;   wrap-around to the last or first subdir. They can be used
;;   as direct replacements for the standard commands.
;;  
;; * Move to nth file in subdir with wraparound (negative) indexing:
;;
;;   Commands `my-dired-move-to-file-at-index' and `my-dired-move-to-last-file'
;;   provide easy indexed movement among the files within a dired
;;   sub-directory. Without prefix args, these commands move to the
;;   first and last file in the directory, respectively.
;;   
;; * Find file read only:
;;     `my-dired-find-file-read-only'
;;
;; * Find file in other window or frame (depending on prefix arg)
;;     `my-dired-find-file-other-window-or-frame'
;;     
;; * Invoke open on file (for Mac OS X):
;;     `my-dired-mac-open'
;;     
;; * Change listing switches without needing prefix arg
;;     `my-dired-change-listing-switches'
;;

(defun my-dired-next-subdir (arg &optional no-skip)
  "Go to next subdirectory, regardless of level. If at the end (arg > 0)
or beginning (arg < 0), cycle to the beginning or end respectively."
  ;; Use 0 arg to go to this directory's header line.
  ;; NO-SKIP prevents moving to end of header line, returning whatever
  ;; position was found in dired-subdir-alist.
  (interactive "p")
  (let ((this-dir (dired-current-directory))
	pos index)
    ;; nth with negative arg does not return nil but the first element
    (setq index (mod (- (dired-subdir-index this-dir) arg)
                     (length dired-subdir-alist)))
    (setq pos (dired-get-subdir-min (nth index dired-subdir-alist)))
    (goto-char pos)
    (or no-skip (skip-chars-forward "^\n\r"))
    (point)))

(defun my-dired-prev-subdir (arg &optional no-skip)
  "Go to previous subdirectory, regardless of level. If at the
beginning (arg > 0) or end (arg < 0), cycle to the end or
beginning respectively. When called interactively and not on a
subdir line, go to this subdir's line."
  ;;(interactive "p")
  (interactive
   (list (if current-prefix-arg
	     (prefix-numeric-value current-prefix-arg)
	   ;; if on subdir start already, don't stay there!
	   (if (dired-get-subdir) 1 0))))
  (my-dired-next-subdir (- arg) no-skip))

(defun my-dired-move-to-file-at-index (&optional fileindex)
  "Without numeric prefix arg FILEINDEX, move to first file in
this subdir. Otherwise, move to FILEINDEX'th file in this subdir.
If FILEINDEX is negative, index from the last file. (That is, -1
is the last file; -2 the second to last, and so forth.) No
attempt is made to ensure that fileindex is in range."
  (interactive "p")
  (cond
   ((> fileindex 0)
    ;; move to first file
    (my-dired-prev-subdir 0)
    (forward-line 1)
    (unless (looking-at "^\\s-*$")
      (forward-line 1)
      (dired-move-to-filename)
      (dired-next-line (- fileindex 1))))
   ((< fileindex 0)
    (if (dired-next-subdir 1 t)
        (progn
          (forward-line -2)
          (dired-move-to-filename))
      (goto-char (point-max))
      (forward-line -1)
      (dired-move-to-filename))
    (dired-previous-line (- -1 fileindex)))
   (t
    (my-dired-prev-subdir 0))))

(defun my-dired-move-to-last-file (&optional fileindex)
  "Without numeric prefix arg FILEINDEX, move to last file in
this subdir. Otherwise, move to FILEINDEX'th from last file in this subdir.
If FILEINDEX is negative, index from the first file. (That is, -1
is the first file; -2 the second, and so forth.) No
attempt is made to ensure that fileindex is in range."
  (interactive "p")
  (my-dired-move-to-file-at-index (- fileindex)))


(defun my-dired-find-file-read-only ()
  "In dired, visit the file or directory named on this line in read-only state."
  (interactive)
  (let ((file-name (file-name-sans-versions (dired-get-filename) t)))
    (if (file-exists-p file-name)
        (find-file-read-only file-name)
      (if (file-symlink-p file-name)
          (error "File is a symlink to a nonexistent target")
        (error "File no longer exists; type `g' to update Dired buffer")))))

(defun my-dired-find-file (&optional read-only-or-alternate)
  "In Dired, visit the file or directory named on this line.
With a prefix argument (C-u or 4), make the visit read-only,
as with `my-dired-find-file-read-only'. With two prefix
arguments (C-u C-u or 16), visit the file or directory instead of
the dired buffer, as with `dired-find-alternate-file'."
  (interactive "p")
  (case read-only-or-alternate
    (1  (call-interactively #'dired-find-file))
    (4  (call-interactively #'my-dired-find-file-read-only))
    (16 (call-interactively #'dired-find-alternate-file))
    (t  (call-interactively #'dired-find-file))))

(defun my-dired-mac-open (&optional arg)
  "In dired, run open command on given file or on each file in list"
  (interactive "P")
  (dired-do-shell-command
   "open" arg (dired-get-marked-files t arg)))

(defun my-dired-find-file-other-window-or-frame (&optional arg)
  "In Dired, visit this file or directory in another window,
when arg is nil, or in another frame otherwise."
  (interactive "P")
  (call-interactively (if arg 
                          #'diredp-find-file-other-frame
                        #'dired-find-file-other-window)))

(defun my-dired-mark-files-path-regexp (regexp &optional marker-char)
  "Mark all files whose filename including path matches REGEXP,
for use in later commands. The prefix path common to all subdirectories
in this dired buffer is first removed as it has no discriminating power
among these files. A prefix argument means to unmark them instead.
Directories `.' and `..' are never marked.

Note the distinction between this and `dired-mark-files-regexp'.
The latter uses only the filename, so distinctions among files in
different subdirectories in the same buffer are not possible. The
former (this function) can distinguish among files in different
subdirectories but may also match patterns in the path
component (excluding the part of the path common to all files in
this buffer, which is removed.)

REGEXP is an Emacs regexp, not a shell wildcard.  Thus, use `\\.o$' for
object files--just `.o' will mark more than you might think."
  (interactive
   (list (dired-read-regexp (concat (if current-prefix-arg "Unmark" "Mark")
				    " files (regexp): "))
	 (if current-prefix-arg ?\040)))
  (let ((dired-marker-char (or marker-char dired-marker-char)))
    (dired-mark-if
     (and (not (looking-at dired-re-dot))
	  (not (eolp))			; empty line
	  (let ((fn (dired-get-filename nil t)))
	    (and fn (string-match regexp fn))))
     "matching file"))) ; ATTN: still need to remove common prefix path! (e.g., buffer-file-name or dired-subdir-alist)

(defun my-dired-change-listing-switches ()
  "In dired, read and set the listing switches. A shortcut
for dired-sort-toggle-or-edit with a prefix arg."
  (interactive)
  (dired-sort-toggle-or-edit 1))

(defun my-dired-create-directory-or-file (&optional file?)
  "Create a new directory or file in the subdirectory containing
point. Without prefix arg, create a directory; with prefix arg
create an file, if the filename does not exist. The dired buffer
is updated with the new directory or file and point is moved
to the corresponding filename."
  (interactive "P")
  (if (null file?)
      (call-interactively 'dired-create-directory)
    (let* ((dir (dired-current-directory))
           (name (read-file-name "Create File: " dir))
           (expanded (expand-file-name name dir)))
      (if (file-exists-p expanded)
          (progn
            (message "File %s already exists in this sub directory" name)
            (dired-goto-file expanded))
        (my-aif (find-file-noselect expanded) ; create file
            (with-current-buffer it 
              (set-buffer-modified-p t)
              (save-buffer)))
        (dired-add-file expanded)
        (dired-move-to-filename)))))

(defun my-dired-create-file-or-directory (&optional directory?)
  "Create a new file or directory in the directory containing
point. Without prefix arg, create a file, if the filename does
not exist; with prefix arg create a directory. The dired buffer
is updated with the new directory or file and point is moved to
the corresponding filename. This is based on the command
`my-dired-create-directory-or-file', with the opposite argument
meaning."
  (interactive "P")
  (if (null directory?)
      (my-dired-create-directory-or-file t)
    (my-dired-create-directory-or-file nil)))

(defun my-dired-find-file-or-subdir (&optional use-new-buf-or-switches-for-dirs?)
  "In dired, visit the file or sub-directory named on this line.
Files are visited as normally with `dired-find-file', but
directories is determined by the argument as follows:

  * No prefix:   Insert directory listing into current dired buffer
  * C-u:         Open directory listing in its own dired buffer
  * C-u C-u:     As with no prefix, but offer to change listing switches
  * C-u C-u C-u: Open directory listing in its own dired buffer
  * M--:         As with no prefix, but offer to change listing switches

where C-u represents \\[universal-argument] and M-- represents \\[negative-argument],
the latter of which can be given by C-u - as well. The triple prefix
case is equivalent to one prefix arg and is intended as a convenience
in case one changes one's mind part way through.

When called from lisp, these arguments correspond to the argument
USE-NEW-BUF-OR-SWITCHES-FOR-DIRS? having values of 1, 4, 16, or -1.
"
  (interactive "p")
  (let ((dir  (my-aif (dired-get-filename nil t) (file-name-as-directory it)))
        (use-new-buffer?   (memq use-new-buf-or-switches-for-dirs? '(4 64)))
        (use-new-switches? (memq use-new-buf-or-switches-for-dirs? '(16 -1))))
    (if (null dir)
        (error "No file on this line") ; should we just use message here??
      (if (or use-new-buffer? (not (file-directory-p dir)))
          (dired-find-file)
        (dired-maybe-insert-subdir
         dir
         (if use-new-switches?
             (read-string "Switches for listing: " dired-actual-switches)))
        (dired-goto-next-file)))))

(defun my-dired-parent-directory-at-point (&optional append-separator?)
  "Return parent directory of the directory associated with the
current point of the dired buffer. If APPEND-SEPARTOR? is non-NIL,
then the directory separator (e.g., /) is appended to the name."
  (interactive "P")
  (let* ((this-dir (directory-file-name (expand-file-name (dired-current-directory))))
         (dir this-dir)
         (parent (directory-file-name (file-name-directory dir))))
    (if append-separator? 
        (expand-directory-name parent)
      parent)))

(defun is-dir-and-prefix-of-dir? (name1 name2 &optional name2-is-dir)
  "Return nil unless NAME1 and NAME2 are both directories and
name1 is a path prefix of NAME2. This means that the path
represented by NAME1 is a sub-path of the path represented by
NAME2 (possibly equal). When optional boolean argument NAME2-IS-DIR is
non-nil, the function treats NAME2 as a directory without
checking.
"
  (let ((dir1 (expand-directory-name name1))
        (dir2 (expand-directory-name name2)))
    (if (and dir1 (file-directory-p dir1)
           (or name2-is-dir (file-directory-p dir2))
           (string-match (concat "^" (regexp-quote dir1)) dir2))
        t nil)))

(defun private/my-dired-read-src-for-move-or-copy (cur-file cur-dir action)
  "Read a file (or directory) name from the minibuffer, to be
the source in a move or copy operation into the current dired
buffer.

    The name to be read must represent a readable file or
directory and should be given as an absolute path. If it is a
directory name, it cannot equal an exact prefix of the directory
named in CUR-DIR.

   CUR-FILE is the name of the file or subdirectory currently at point in the
dired buffer. It can be nil if point does not lie on a file line.

   CUR-DIR is the directory containing point in the current dired buffer.

   ACTION is a string used in the prompt to describe the action,
usually `rename' or `copy'.

   Returns the file name. Multiple reads may be required if
the user mis-specifies the input.

   Preconditions: Should only be called from a dired buffer.
"
  (let
      ((default-dir
         (if (and cur-file (file-directory-p cur-file))
             (file-name-as-directory cur-file)
           (my-dired-parent-directory-at-point t)))
       (prompt (format "File to %s: " action))
       from)
    (setq from (read-file-name prompt default-dir "" t nil #'file-readable-p))
    (while (or (null from) (string= from ""))
      (message (format "Input \"%s\" invalid...try again" from))
      (sit-for 1)
      (setq from (read-file-name prompt default-dir "" t nil #'file-readable-p))
      (if (or (is-dir-and-prefix-of-dir? from cur-dir t)
              (not (file-readable-p from)))
          (setq from nil)))
    from))

; This and the previous are different because we may wish to specialize them.
(defun private/my-dired-read-des-for-move-or-copy (src-file cur-dir)
  "Read a name from the minibuffer, to be the destination in a
move or copy operation into the current dired buffer.

    The name represents a file or directory relative to CUR-DIR,
the directory containing point in the current dired buffer. It
should be a valid name for the associated file system, and should
not be an exact prefix of SRC-FILE.

   SRC-FILE is the name of the source file in the move and copy operation
and is used for generating the default in reading this name.
Specifically, the default is the file root (as in `file-name-nondirectory')
of SRC-FILE.

   CUR-DIR is the directory containing point in the current dired buffer.

   Returns the file name as an absolute path. Multiple reads may
be required if the user mis-specifies the input.

   Preconditions: Should only be called from a dired buffer.
"
  (let
      ((default-name
         (if src-file
             (file-name-nondirectory (expand-file-name src-file))
           "untitled"))
       (prompt "New Name: ")
       default-path
       to)
    (setq default-path (concat (expand-directory-name cur-dir) default-name))
    (setq to (read-file-name prompt cur-dir default-path))
    (while (or (null to) (string= to ""))
      (message (format "Input \"%s\" invalid...try again" to))
      (sit-for 1)
      (setq to (read-file-name prompt cur-dir default-path))
      (if (is-dir-and-prefix-of-dir? to src-file t)
          (setq to nil)))
    to))

(defun my-dired-rename-file-from-elsewhere (src des)
  "Move/rename a file from somewhere in the file system into the
directory containing point in this dired buffer.

   SRC should be the name of an existing, readable file or
directory, as an absolute path. If it is a directory is should
not be a prefix of the directory containing point. When the
function is called interactively, SRC is read in the
minibuffer with completion. The default directory is determined
by the name at point: if it is a file, the default location
is the parent directory; if it is a directory, the default location
is within that directory.

   DES, if supplied, should be a valid file or directory name, also
as an absolute path. It should not be a prefix of the 
directory containing point. When the function is called interactively,
DES is read in the minibuffer with completion. The default
value is the file/directory in the current directory that has
the same fileroot (last component in path) as SRC.
" 
  (interactive
   (let*
       ((curfile (dired-get-filename nil t))
        (curdir  (dired-current-directory))
        (from (private/my-dired-read-src-for-move-or-copy curfile curdir "rename"))
        (to   (private/my-dired-read-des-for-move-or-copy from curdir)))
     (list from to)))
  (my-dired-bring-file-elsewhere-to-here src des 'move (called-interactively-p 'any)))

(defun my-dired-copy-file-from-elsewhere (src des)
  "Copy a file from somewhere in the file system into the
directory containing point in this dired buffer.

   SRC should be the name of an existing, readable file or
directory, as an absolute path. If it is a directory is should
not be a prefix of the directory containing point. When the
function is called interactively, SRC is read in the
minibuffer with completion. The default directory is determined
by the name at point: if it is a file, the default location
is the parent directory; if it is a directory, the default location
is within that directory.

   DES, if supplied, should be a valid file or directory name, also
as an absolute path. It should not be a prefix of the 
directory containing point. When the function is called interactively,
DES is read in the minibuffer with completion. The default
value is the file/directory in the current directory that has
the same fileroot (last component in path) as SRC.
" 
  (interactive
   (let*
       ((curfile (dired-get-filename nil t))
        (curdir  (dired-current-directory))
        (from (private/my-dired-read-src-for-move-or-copy curfile curdir "copy"))
        (to   (private/my-dired-read-des-for-move-or-copy from curdir)))
     (list from to)))
  (my-dired-bring-file-elsewhere-to-here src des 'copy (called-interactively-p 'any)))

(defun my-dired-bring-file-elsewhere-to-here (srcname desname how &optional checked)
  "Get a file from somewhere in the file system into the
directory containing point in this dired buffer.

   SRCNAME should be the name of an existing, readable file or
directory, as an absolute path. If it is a directory is should
not be a prefix of the directory containing point.

   DIR is the current dired directory and should match the result
of calling `dired-current-directory'. An error will be raised if
this directory is not writeable by the current process.

   DESNAME, if supplied, should be a valid file or directory
name, also as an absolute path. It should not be a prefix of the
directory containing point. If DESNAME is the name of an existing
file, the user is queried to determine if the file should be
overwritten.

   HOW should be a symbol, either 'move for a move/renaming or
'copy for a copy operation. The variable `dired-recursive-copies'
controls recursive copying behavior for this function as well.

   CHECKED is a boolean that if non-nil, indicates that the names
have already been checked for validity and should not be rechecked.
Use this with care.
"
  (let* ((dir   (dired-current-directory))
         (from  (expand-file-name srcname))
         (to    (expand-file-name desname dir))
         (bring (if (eq how 'move) 'dired-rename-file 'dired-copy-file)))
    (unless checked  ; make sure the names are valid
      (cond
       ((not (file-readable-p from))
        (error (format "Source file %s not readable" from)))
       ((not (is-dir-and-prefix-of-dir? from dir t))
        (error (format "Source directory %s cannot be an exact prefix of the current directory" from)))
       ((not (is-dir-and-prefix-of-dir? to from t))
        (error (format "Destination directory %s cannot be an exact prefix of the source file" from)))))
    (let* ((overwrite (file-exists-p to))
           (overwrite-backup-query nil) ; used in dired-handle-overwrite
           (dired-overwrite-confirmed   ; used in dired-handle-overwrite
            (and overwrite              ; confirm if necessary
                 (yes-or-no-p (format "Overwrite %s? " to)))))
      (condition-case err
          (progn ; do the actual move or copy
            (funcall bring from to dired-overwrite-confirmed)
            (dired-add-file to)
            (revert-buffer)
            (let ((what (if (file-directory-p from) "Directory" "File"))
                  (verb (if (eq how 'move) "renamed" "copied")))
              (message "%s %s %s to %s" what from verb to)))
        (file-error
         (progn
           (message "rename `%s' to `%s' failed:\n%s\n"
                    from to err)
           (dired-log "rename `%s' to `%s' failed:\n%s\n"
                      from to err)
           (dired-log t)))))))

;; Configuration
;;
;; This is a modification and extension of the standard dired
;; keymap that uses the functionality above. I find it faster
;; and more convenient than the standard bindings.
;;
;; As mentioned earlier, one difference in theme relative
;; to the default bindings is C-w versus M-k. This is reflected
;; here in the use of C-k and M-k for killing lines and subdirs
;; and w for ''scrolling'' within a directory. Other notable
;; changes include a remapping of r/R and c/C to allow both
;; kinds of moving and copying described above, and
;; greater accessibility of grep commands.
;; 

(setq-default dired-listing-switches "-lFoh")

(defun my-dired-load-hook ()
  (require-soft 'dired-aux)
  (require-soft 'dired-x)
  (require-soft 'dired+)
  (setq dired-ls-F-marks-symlinks t) ;; Says ls -F marks symlinks with @
  (setq dired-backup-overwrite 'ask)
  (defalias 'dired-advertised-find-file-read-only 'dired-find-file-read-only)
  ;; don't bother checking (featurep 'dired+) but maybe later... 
  (define-key dired-mode-map [?\M-C]   'diredp-capitalize-this-file)
  (define-key dired-mode-map "c"       'dired-do-copy)
  (define-key dired-mode-map "e"       'my-dired-find-file)
  (define-key dired-mode-map "f"       'my-dired-find-file)
  (define-key dired-mode-map "r"       'dired-do-rename)
  (define-key dired-mode-map "w"       'my-dired-move-to-file-at-index)
  (define-key dired-mode-map "C"       'my-dired-copy-file-from-elsewhere)
  (define-key dired-mode-map "F"       'find-dired)
  (define-key dired-mode-map "G"       'find-grep-dired)
  (define-key dired-mode-map "M"       'dired-do-rename)
  (define-key dired-mode-map "N"       'find-name-dired)
  (define-key dired-mode-map "R"       'my-dired-move-file-from-elsewhere)
  (define-key dired-mode-map "V"       'my-dired-move-to-last-file) ; should be "v" but that's life
  (define-key dired-mode-map "W"       'browse-url-of-dired-file)
  (define-key dired-mode-map "%p"      'my-dired-mark-files-path-regexp) ; consider %m instead
  (define-key dired-mode-map "%F"      'find-dired)
  (define-key dired-mode-map "%G"      'find-grep-dired)
  (define-key dired-mode-map "%m"      'dired-mark-files-regexp)
  (define-key dired-mode-map "%M"      'dired-do-rename-regexp)
  (define-key dired-mode-map "%N"      'find-name-dired)
  (define-key dired-mode-map "\M-c"    'dired-copy-filename-as-kill)
  (define-key dired-mode-map "\M-d"    'dired-tree-down)
  (define-key dired-mode-map "\M-g"    'diredp-do-grep)
  (define-key dired-mode-map "\M-i"    'dired-maybe-insert-subdir)
  (define-key dired-mode-map "\M-n"    'my-dired-next-subdir)
  (define-key dired-mode-map "\M-o"    'dired-display-file)
  (define-key dired-mode-map "\M-p"    'my-dired-prev-subdir)
  (define-key dired-mode-map "\M-u"    'dired-tree-up)
  (define-key dired-mode-map "\C-d"    'mark-word)
  (define-key dired-mode-map "\C-m"    'my-dired-find-file-or-subdir)
  (define-key dired-mode-map "\C-o"    'my-dired-find-file-other-window-or-frame)
  (define-key dired-mode-map "\C-k"    'dired-do-kill-lines)
  (define-key dired-mode-map "\M-k"    'dired-kill-subdir)
  (define-key dired-mode-map "\C-\M-g" 'dired-do-chgrp)
  (define-key dired-mode-map "\C-\M-k" 'dired-kill-tree)
  (define-key dired-mode-map "\C-\M-l" 'diredp-downcase-this-file)
  (define-key dired-mode-map "\C-\M-m" 'my-dired-move-file-from-elsewhere)
  (define-key dired-mode-map "\C-\M-n" 'my-dired-create-directory-or-file)
  (define-key dired-mode-map "\C-\M-o" 'my-dired-mac-open)
  (define-key dired-mode-map "\C-\M-p" 'diredp-print-this-file)
  (define-key dired-mode-map "\C-\M-u" 'diredp-upcase-this-file)
  (define-key dired-mode-map "\M-ss"   'dired-do-isearch)
  (define-key dired-mode-map "\M-sr"   'dired-do-isearch-regexp)
  (define-key dired-mode-map "\M-sf"   'dired-isearch-filenames)
  (define-key dired-mode-map "\M-sx"   'dired-isearch-filenames-regexp)
  (define-key dired-mode-map [?\C-\M--] 'my-dired-change-listing-switches)
  (if (not (featurep 'dired+))
      (message "Dired+ is not loaded")
    (set-face-background 'diredp-no-priv     "white")
    (set-face-background 'diredp-write-priv  "tan")
    (set-face-background 'diredp-dir-heading "bisque1")
    (set-face-background 'diredp-dir-priv    "white")
    (set-face-foreground 'diredp-dir-priv    "#FF3445")
    (set-face-foreground 'diredp-file-name   "#3D34FF")
    (set-face-foreground 'diredp-file-suffix "#CF34FF"))
  )

(defun my-dired-mode-hook ()
  (setq dired-recursive-copies  'top)
  (setq dired-recursive-deletes 'ask) ; always ask on delete!
  )

(add-hook 'dired-load-hook 'my-dired-load-hook)
(add-hook 'dired-mode-hook 'my-dired-mode-hook)

(if (featurep 'dired)     ; dired already loaded!
    (my-dired-load-hook)) ; so do the setup now...


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Linkd Mode

(require-soft 'linkd)
(setq linkd-use-icons t)
(setq linkd-icons-directory (concat my-site-lisp-dir "other-modes/linkd-icons"))
(setq linkd-wiki-directory "~/.emacs.d/linkd-wiki")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Icicles

;; Added Functionality
;;
;; * Show sort order label in completions buffer:
;;
;;   Changing the sort order of the completions causes a brief
;;   echo that is easily missed. The command
;;   `my-icicle-show-sort-order' changes the sort order and
;;   labels the sort order in the completion buffer. It is
;;   configured by variable `my-icicle-describe-sort-in-completions'.
;; 
;; * Move to beginning or end of completions list from minibuffer:
;;
;;   It is convenient when completion lists are long to be able
;;   to move to the beginning or end of the completions list from
;;   the minibuffer. The commands `icicle-beginning-of-Completions'
;;   and `icicle-end-of-Completions' do this.
;;
;; * Fixes double-default problem that sometimes occurs.
;; 
;;   See `icicle-suppress-default-pattern' and rewrite of
;;   icicle function `icicle-read-from-minibuffer'.
;;

(defvar my-icicle-describe-sort-in-completions (and t (featurep 'icicles))
  "If non-nil, show the current sort order in *Completions* buffer.")

(defun my-icicle-show-sort-order ()
  "Show current sort order in completions list.
   Add this to completion-setup-hook to achieve
   the effect."
  (when (and my-icicle-describe-sort-in-completions icicle-mode)
    (with-current-buffer standard-output
      (let* ((sort-order (icicle-current-sort-order nil))
             (sort-string (concat ", sorting "
                                  (if (string-equal sort-order "alphabetical")
                                      "alphabetically" sort-order))))
        (goto-char (point-min))
        (when (re-search-forward "Possible completions are" nil t)
          (put-text-property 0 (length sort-string) 'face 'icicle-Completions-instruction-1 sort-string)
          (insert sort-string)))))
  )

(defun icicle-beginning-of-Completions ()
  "Scroll to the beginning of *Completions* window."
  (interactive)
  (let ((window (get-buffer-window "*Completions*" 0)))
  (when window
    (save-selected-window
      (select-window window)
      (unless (= (window-start) (point-min))
        (goto-char (icicle-start-of-candidates-in-Completions)))
      ))))

(defun icicle-end-of-Completions ()
  "Scroll to the end of the *Completions* window, showing
   as many of the completions as possible."
  (interactive)
  (let ((window (get-buffer-window "*Completions*" 0)))
  (when window
    (save-selected-window
      (select-window window)
      (unless (= (window-end) (point-max))
        (goto-char (point-max))
        (scroll-down (1- (/ (window-height) 2)))
        (beginning-of-line))
      ))))

;; Configuration
;;
;; Besides incorporating the above functionality, the
;; configuration below adapts the default icicles keybindings
;; to my theme as described in the introduction. In particular,
;; C-w/C-v instead of C-v/M-v for scrolling, and M-w/M-v for
;; beginning and end of buffer. These are reflected in
;; the corresponding minibuffer commands below. In addition,
;; I add C-n/C-p as keys for moving among completions,
;; which is more convenient than up/down and is easy on
;; the mac keyboard as well.
;; 

(when (featurep 'icicles)
  (add-to-list 'icicle-modal-cycle-up-keys              "\C-p")
  (add-to-list 'icicle-modal-cycle-up-action-keys       "\C-\M-p")
  (add-to-list 'icicle-modal-cycle-up-alt-action-keys   [?\C-\S-p])
  (add-to-list 'icicle-modal-cycle-up-help-keys         [?\C-\M-\S-p])
  (add-to-list 'icicle-modal-cycle-down-keys            "\C-n")
  (add-to-list 'icicle-modal-cycle-down-action-keys     "\C-\M-n")
  (add-to-list 'icicle-modal-cycle-down-alt-action-keys [?\C-\S-n])
  (add-to-list 'icicle-modal-cycle-down-help-keys       [?\C-\M-\S-n])
  (add-hook 'completion-list-mode-hook
            (lambda () (define-key completion-list-mode-map [\C-return] 'icicle-insert-completion)))
  ;;
  ;; The remaining code in this form fixes a problem where defaults are
  ;; sometimes given twice in the minibuffer prompt. It creates a new
  ;; icicles variable `icicle-suppress-default-pattern' and modifies
  ;; an existing function `icicle-read-from-minibuffer'.
  ;;
  (setq icicle-default-value t)
  (setq icicle-suppress-default-pattern-1 "\\(?:(.+)\\|\\[.+\\]\\|{.+}\\)")  ; internal delimiter match ok
  (defvar icicle-suppress-default-pattern 
    (concat "\\(?::\\s-*" icicle-suppress-default-pattern-1 "\\|" icicle-suppress-default-pattern-1 "\\s-*:\\)\\s-*$")
    "Regular expression used to determine if a default value should
be added to the minibuffer prompt. If the variable
`icicle-default-value' equals t and this pattern matches the
existing prompt, no default is added.

This is intended for cases, such as help-command, when the
minibuffer already contains a default, so icicles would add a
second copy when `icicle-default-value' is t. Setting
`icicle-default-value' to nil does not solve the problem as one
might want a default in the prompt, just not too. The default
value of this pattern is designed to catch the common case where
the end of the existing prompt is a string delimited by parens,
braces, or brackets that precedes or follows a colon and optional
whitespace.")
  ;; The following function is a modified from that given in 
  ;; icicles-fn.el update # 11922 version 22.0.
  (defun icicle-read-from-minibuffer (prompt &optional initial-contents keymap read
                                             hist-m@%=!$+&^*z default-value inherit-input-method)
    "Read a string from the minibuffer, prompting with string PROMPT.
The optional second arg INITIAL-CONTENTS is an alternative to
  DEFAULT-VALUE.  Vanilla Emacs considers it to be obsolete, but
  Icicles does not.  It is discussed in more detail below.
Third arg KEYMAP is a keymap to use while reading;
  if omitted or nil, the default is `minibuffer-local-map'.
If fourth arg READ is non-nil, then interpret the result as a Lisp object
  and return that object:
  in other words, do `(car (read-from-string INPUT-STRING))'
Fifth arg HIST, if non-nil, specifies a history list and optionally
  the initial position in the list.  It can be a symbol, which is the
  history list variable to use, or it can be a cons cell
  (HISTVAR . HISTPOS).  In that case, HISTVAR is the history list variable
  to use, and HISTPOS is the initial position for use by the minibuffer
  history commands.  For consistency, you should also specify that
  element of the history as the value of INITIAL-CONTENTS.  Positions
  are counted starting from 1 at the beginning of the list.
Sixth arg DEFAULT-VALUE is the default value.  If non-nil, it is available
  for history commands; but, unless READ is non-nil, `read-from-minibuffer'
  does NOT return DEFAULT-VALUE if the user enters empty input!  It returns
  the empty string.  DEFAULT-VALUE can be a string or a list of strings.
  These  become the minibuffer's future history, available using `M-n'.
Seventh arg INHERIT-INPUT-METHOD, if non-nil, means the minibuffer inherits
 the current input method and the setting of `enable-multibyte-characters'.
Eighth arg KEEP-ALL, if non-nil, says to put all inputs in the history list,
 even empty or duplicate inputs.  This is available starting with Emacs 22.
If the variable `minibuffer-allow-text-properties' is non-nil,
 then the string which is returned includes whatever text properties
 were present in the minibuffer.  Otherwise the value has no text properties.

Option `icicle-default-value' controls how the default value,
DEFAULT-VALUE, is treated.

The remainder of this documentation string describes the
INITIAL-CONTENTS argument in more detail.  If non-nil,
INITIAL-CONTENTS is a string to be inserted into the minibuffer before
reading input.  Normally, point is put at the end of that string.
However, if INITIAL-CONTENTS is (STRING . POSITION), the initial input
is STRING, but point is placed at one-indexed position POSITION in the
minibuffer.  Any integer value less than or equal to one puts point at
the beginning of the string.  *Note* that this behavior differs from
the way such arguments are used in `completing-read' and some related
functions, which use zero-indexing for POSITION."
    (unless initial-contents (setq initial-contents  ""))

    ;; Filter DEFAULT-VALUE using `icicle-filter-wo-input'.
    (when default-value
      (setq default-value
            (if (atom default-value)
                (icicle-filter-wo-input default-value)
              (delq nil (mapcar #'icicle-filter-wo-input default-value))))) ; Emacs 23 accepts a list.
    ;; Save new default value for caller (e.g. `icicle-lisp-vanilla-completing-read'.
    (setq icicle-filtered-default-value  default-value)

    ;; If a list of strings, use the first one for prompt etc.
    (let ((def-value  (if (consp default-value) (car default-value) default-value)))
      ;; Maybe use DEFAULT-VALUE for INITIAL-CONTENTS also.
      (when (and icicle-default-value  (not (eq icicle-default-value t))
                 def-value  (stringp initial-contents)  (string= "" initial-contents))
        (setq initial-contents  (if (integerp def-value) ; Character
                                    (char-to-string def-value)
                                  def-value)))
      (when (and def-value (eq icicle-default-value t)) ; Add DEFAULT-VALUE to PROMPT.
        (unless (and icicle-suppress-default-pattern
                     (string-match icicle-suppress-default-pattern prompt))
          (when (icicle-file-name-input-p) (setq def-value  (file-name-nondirectory def-value)))
          (setq prompt  (if (string-match "\\(.*\\)\\(: *\\)$" prompt)
                            (concat (substring prompt (match-beginning 1) (match-end 1)) " (" def-value
                                    ")" (substring prompt (match-beginning 2) (match-end 2)))
                          (concat prompt def-value))))))
    (old-read-from-minibuffer
     prompt initial-contents keymap read hist-m@%=!$+&^*z default-value inherit-input-method))
  )

(defun my-icicle-mode-hook ()
  (setq read-file-name-completion-ignore-case t) ; Emacs >= 22
  (setq read-buffer-completion-ignore-case t)    ; Emacs >= 23
  (setq icicle-show-Completions-help-flag nil)
  (when (and icicle-mode (lookup-key icicle-mode-map "\C-h"))
    (map-keymap  ; move help keymap to each new help key
     (lambda (event binding)
       (mapcar
        (lambda (base-event)
          (define-key icicle-mode-map
            (vconcat base-event (vector event)) binding))
        my-help-events))
     (lookup-key icicle-mode-map "\C-h"))
    (define-key icicle-mode-map "\C-h" nil))
  (when icicle-mode
    (define-key icicle-mode-map "\C-c/" nil)) ; conflicts with org-sparse-tree in org-mode
  (if icicle-mode
      (add-hook 'completion-setup-hook 'my-icicle-show-sort-order t)
    (remove-hook 'completion-setup-hook 'my-icicle-show-sort-order))
  )

(add-hook 'icicle-mode-hook 'my-icicle-mode-hook)

(defun my-icicle-minibuffer-setup-hook ()
  (define-key minibuffer-local-completion-map "\C-w"     'icicle-scroll-Completions-up)
  (define-key minibuffer-local-completion-map "\C-v"     'icicle-scroll-Completions)
  (define-key minibuffer-local-completion-map "\M-w"     'icicle-beginning-of-Completions)
  (define-key minibuffer-local-completion-map "\M-v"     'icicle-end-of-Completions)
  (define-key minibuffer-local-completion-map "\M-k"     'icicle-kill-region)
  (define-key minibuffer-local-completion-map "\M-e"     'icicle-erase-minibuffer-or-history-element)
  (define-key minibuffer-local-completion-map "\M-c"     'icicle-switch-to-Completions-buf)
  (define-key minibuffer-local-completion-map "\C-\M-c"  'icicle-toggle-case-sensitivity)
  (define-key minibuffer-local-completion-map "\C-x\C-o" 'icicle-switch-to/from-minibuffer)
  ;; process new help key position
  (define-key minibuffer-local-completion-map "\M-y" 'icicle-history)
  (define-key minibuffer-local-completion-map "\M-h" nil)
  ;; the following no longer appear necessary
  ;(define-key minibuffer-local-completion-map "\C-n" 'icicle-next-candidate-per-mode)
  ;(define-key minibuffer-local-completion-map "\C-p" 'icicle-previous-candidate-per-mode)
  )

(add-hook 'icicle-minibuffer-setup-hook 'my-icicle-minibuffer-setup-hook)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Interactive Do Things (ido-mode)

;; Added Functionality
;;
;; * Smart beginning-of-line command
;;
;;   Command `my-ido-begin-line' moves to the
;;   beginning of the line on first invocation,
;;   but successive invocations toggle among
;;   the home-directory and the root-directory.
;;   This is used for quick editing when
;;   the desired file is not in current part
;;   of the tree
;;   

(defvar my-ido-begin-str "~/")

(defvar my-ido-begin-home 'home
  "*Determines the behavior of \\[my-ido-begin-line].
    If equal 'home,  replaces existing
    file text with the home directory. If 'root,
    replaces it with root. And if 'old, replaces
    it with value of \\[my-ido-begin-str]. The
    function toggles among these states.")

(defun my-ido-begin-line ()
  "*Moves to the beginning of the find file line,
    toggling between the home directory and root
    on successive calls."
  (interactive)
  (let
      ;; ATTN: Fix this 12 == length("Find file: ")
      ((contents (buffer-substring-no-properties 12 (minibuffer-prompt-end))))
    (cond
     ((equal my-ido-begin-home 'home)
      (insert "~/")
      (setq my-ido-begin-home 'root)
      ;;(setq my-ido-begin-str contents)
      (kill-new contents) ;;ATTN: for now make old string available to yank
      )
     ((equal my-ido-begin-home 'root)
      (insert "//")
      (setq my-ido-begin-home 'home) ;;'old)
      )
     (t ;; ATTN: for now this isn't working because delete fails so ignore it
      (setq my-ido-begin-home 'home)
      ;;(delete-region 12 (minibuffer-prompt-end))
      ;;(insert my-ido-begin-str)
      )))
  (setq ido-rescan t))

;; Configuration
;;
;; A rebinding of the standard map that I find more memorable.
;; 

(defun my-ido-setup-hook ()
  "Adjust keymaps for ido completion and other customizations."
  (define-key ido-file-completion-map "\C-a" 'my-ido-begin-line)
  (define-key ido-file-completion-map "\C-p" 'ido-prev-match)
  (define-key ido-file-completion-map "\C-n" 'ido-next-match)
  (define-key ido-file-completion-map "\C-xc"    'ido-toggle-case)
  (define-key ido-file-completion-map "\C-x\C-c" 'ido-toggle-case)
  (define-key ido-file-completion-map "\C-x\C-i" 'ido-toggle-ignore)
  (define-key ido-file-completion-map "\C-xi"    'ido-toggle-ignore)
  (define-key ido-file-completion-map "\C-xp"    'ido-toggle-prefix)
  (define-key ido-file-completion-map "\C-x\C-p" 'ido-toggle-prefix)
  (define-key ido-file-completion-map "\C-xr"    'ido-toggle-regexp)
  (define-key ido-file-completion-map "\C-x\C-r" 'ido-toggle-regexp)
  (setq ido-enable-flex-matching t)
  )

(add-hook 'ido-setup-hook 'my-ido-setup-hook)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Anything


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; YASnippet

(require-soft 'yasnippet)

(when (featurep 'yasnippet)
  (setq yas/root-directory "~/.emacs.d/snippets")
  (yas/initialize)
  (yas/load-directory yas/root-directory)
  (setq yas/prompt-functions '(yas/ido-prompt yas/dropdown-prompt yas/completing-prompt yas/x-prompt yas/no-prompt)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Which-Function

(require-soft 'which-func)
(setq which-func-modes 
      '(emacs-lisp-mode c-mode c++-mode python-mode perl-mode cperl-mode makefile-mode sh-mode fortran-mode f90-mode ada-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; View-mode

(defun my-view-mode-hook ()
  (define-key view-mode-map "\C-\M-w" 'View-scroll-half-page-backward)
  (define-key view-mode-map "\C-\M-v" 'View-scroll-half-page-forward)
  (define-key view-mode-map "\M-v"    'end-of-buffer)
  (define-key view-mode-map "k"       'View-kill-and-leave)
  (define-key view-mode-map "l"       'View-leave)
  )

(add-hook 'view-mode-hook 'my-view-mode-hook)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Help-mode

;; Added Functionality 
;;
;; * Easy navigation to and from *Help* buffer/window
;;
;;   I often navigate through links in help information in a single help
;;   buffer, while working in another primary buffer, so it is useful to
;;   be able to move from the given buffer into and out of an existing
;;   help buffer. This is especially true when the window structure in
;;   the frame is complicated. The functions `my-help-return-from-help'
;;   and `my-help-goto-help' provide that service. By default, they are
;;   bound to g in help-mode and <help-char>-g below.
;;
;; * Avoiding view-mode shadowing of help-mode key bindings.
;;
;;   Help buffers use view-minor mode, which has the effect of shadowing
;;   any conflicting keybindings set in help-mode-hook. The help
;;   system uses `minor-mode-overriding-map-alist' to prevent that,
;;   but the way this is done does not allow overriding this map
;;   in the help-mode-hook. (Short reason: `help-make-xrefs' is
;;   called *after* the help mode hook in setting up the buffer,
;;   resetting `minor-mode-overriding-map-alist'.) The functions
;;   `my-help-make-override-map' and `my-help-override-view-map'
;;   and the configuration below prevent this from happening.
;;
;; * Adjusted keybindings in help-mode.
;;
;;   Slightly different from the default, but I find them much nicer.
;;

(defun my-help-return-from-help (&optional bury-help)
  "From *Help* buffer, return to previous buffer.
This intentionally does not delete the help buffer or window, but
the prefix arg (when called interactively) or BURY-HELP can
modify the fate of the help buffer/window. With a single prefix
arg (or BURY-HELP eq 4), the help buffer is buried; with two
prefix args (or BURY-HELP eq 16), the help window is deleted.
This function uses the window given in `view-return-to-alist' if
available but ignores the buffer information in those entries.
See also `View-leave' and `View-kill-and-leave' for
alternatives."
  (interactive "p")
  (let* ((win-info
         (and (boundp 'view-return-to-alist)
              view-return-to-alist
              (assq (selected-window) view-return-to-alist)))
         (old-window (and win-info (car (cdr win-info))))
         (cbuf (current-buffer))
         (cwin (selected-window)))
    (if (and old-window (window-live-p old-window))
        (select-window old-window)
      (pop-to-buffer (other-buffer cbuf t)))
    (cond
     ((and (eq bury-help 16)
           (not (one-window-p t)))
      (delete-window cwin))
     ((eq bury-help 4)
      (with-selected-window cwin (bury-buffer))))))

(defun my-help-goto-help ()
  "If a buffer named *Help* exists, move to it."
  (interactive)
  (let* ((help-buf (get-buffer "*Help*"))
         (help-win (get-buffer-window help-buf)))
    (when help-buf
      (if (and help-win (window-live-p help-win))
          (select-window help-win t)
        (switch-to-buffer-other-window help-buf t)))))

(defun my-help-make-override-map (&optional event-list)
  "Create keymap used for `minor-mode-overriding-map-alist' to
prevent parent mode `view-mode' from shadowing several keys we
need. EVENT-LIST is a list of event specifiers, in either vector
or string form (as for local-set-key), to unshadow. If EVENT-LIST
is nil, `\r' is unshadowed as is done by default in help-mode. If
not EVENT-LIST is not nil, the `\r' is not included. Returns the
constructed keymap."
  (let ((map (make-sparse-keymap)))
    (when (null event-list)
      (setq event-list (list "\r")))
    (set-keymap-parent map view-mode-map)
    (dolist (key event-list)
      (define-key map key nil))
    map))

(defun my-help-override-view-map (unshadow-map)
  "Prevent parent mode `view-mode' from shadowing several keys we
need. Resets `minor-mode-overriding-map-alist', adding an entry
for `view-mode' to do this. UNSHADOW-MAP is a keymap to unshadow,
as constructed by `my-help-make-override-map'."
  (set (make-local-variable 'minor-mode-overriding-map-alist)
       (list (cons 'view-mode unshadow-map))))

;; Configuration
;;
;; The main changes, as mentioned above, are some different keybindings,
;; easy navigation bound to g, and overriding of view-mode shadowing.
;; The latter is, in my view, a design bug in the current help-mode.el
;; and help.el, which will hopefully be rectified. So it is subject
;; to change in the future.
;; 

(defvar my-help-keybindings
  '((","        . rename-uniquely)
    ("a"        . command-apropos)
    ("\M-a"     . apropos-documentation)
    ("b"        . describe-bindings)
    ("c"        . describe-key-briefly)
    ("C"        . describe-coding-system)
    ("d"        . apropos-documentation)
    ("e"        . view-echo-area-messages)
    ("f"        . describe-function)
    ("F"        . view-emacs-FAQ)
    ("g"        . my-help-return-from-help)
    ("G"        . describe-gnu-project)
    ("i"        . info)
    ("I"        . Info-goto-emacs-command-node)
    ("k"        . describe-key)
    ("K"        . Info-goto-emacs-key-command-node)
    ("l"        . help-go-back)   ; like Info-mode, muscle memory
    ("L"        . view-lossage)   ; minor inconsistency with help-map
    ("\M-l"     . locate-library)
    ("m"        . describe-mode)
    ("p"        . finder-by-keyword)
    ("q"        . View-leave)
    ("\M-q"     . View-kill-and-leave)
    ("r"        . info-emacs-manual)
    ("s"        . describe-syntax)
    ("S"        . info-lookup-symbol)
    ("u"        . manual-entry)
    ("v"        . describe-variable)
    ("V"        . describe-language-environment) ; another minor inconsistency
    ("w"        . where-is)
    ("y"        . View-leave)           ; could do bury-buffer instead
    ("\C-c\C-c" . help-follow-symbol)
    ("\C-c\C-b" . help-go-back)
    ("\C-c\C-f" . help-go-forward)
    ([\M-left]  . help-go-back)
    ([\M-right] . help-go-forward))
  "Keybindings used in help-mode as well as keys to override so
that they are not shadowed by view-mode. This changes a few keys
and adds some useful functionality to the default help-mode map.
It is mostly consistent with the help bindings, although some
minor inconsistencies with help-map keys are allowed if they make
commonly used operations easier at the expense of shadowing less
commonly used functions in this mode.")

(defvar my-help-unshadow-map
  (my-help-make-override-map (mapcar #'car my-help-keybindings))
  "Keymap setting keys to be visible despite the bindings of
view-mode, which usually shadow various help keys. This
unshadowing is done by setting `minor-mode-overrriding-map-alist';
see `my-help-override-view-map'.")

(defun my-help-mode-hook ()
    ;(dolist (binding my-help-keybindings)
    ;  (define-key help-mode-map (car binding) (cdr binding)))
    ;(my-help-override-view-map my-help-unshadow-map)
    (ignore "Nothing needed yet given the remaining config."))

;; Uncomment this if something needs to be added to my-help-mode-hook
;(add-hook 'help-mode-hook 'my-help-mode-hook)

;; Bind the keys here instead of in help-mode hook because
;; there are so many of them.  ??
(eval-after-load 'help-mode
  '(dolist (binding my-help-keybindings)
     (define-key help-mode-map (car binding) (cdr binding))))

(eval-after-load 'help
  '(define-key help-map "g" 'my-help-goto-help))

(defadvice help-make-xrefs (after unshadow-view-keys last activate)
  "The variable `minor-mode-overriding-map-alist' is reset in
`help-make-xrefs' which is used for temporary buffer creation
and so undoes any attempt to adjust the map in the help-mode-hook."
  (my-help-override-view-map my-help-unshadow-map))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Doc-View Mode

(defun my-doc-view-mode-hook ()
  (auto-revert-mode 1))

(add-hook 'doc-view-mode-hook 'my-doc-view-mode-hook)

;not necessary once exec-path is properly setup
;(setq doc-view-ghostscript-program "/opt/local/bin/gs")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Hippie Expand

(condition-case nil
    (delete 'try-expand-line hippie-expand-try-functions-list)
   ;(delete 'try-expand-list hippie-expand-try-functions-list)
   (error nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                           ;;
;;(@* "TeX-related Modes")                                   ;;;;;;
;;                                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; AUC TeX support

;; I primarily use genTeX, which is why I use `plain-TeX-mode'
;; as the default AUCTeX mode. But the configuration below supports
;; plain TeX, laTeX, and genTeX. The primary extension here
;; is easy functionality in `TeX-expand-list' to run
;; make on a target, which is a convenient for extended projects.

(setq tex-default-mode 'plain-tex-mode) ;includes non AucTeX case
(setq TeX-default-mode 'plain-TeX-mode)
(setq my-TeX-generic-setup-done nil)

(defun my-generic-TeX-setup ()
  "Configure global settings and interface that affects all 
AucTeX modes. Only performed once across all modes."
  (do-only-once my-TeX-generic-setup-done
    (add-to-list 'TeX-format-list '("TEX" plain-tex-mode "\\input\\b"))
    (add-to-list 'TeX-expand-list '("%a" file "pdf" t))
    (add-to-list 'TeX-expand-list '("%M"
                                    (lambda ()
                                      (let ((targets (my-get-makefile-targets))) 
                                        (completing-read "Target: " targets)) )))
    (delete-if (lambda (item) (string-equal "Makeinfo" (car item))) TeX-command-list)
    (delete-if (lambda (item) (string-equal "Makeinfo HTML" (car item))) TeX-command-list)
    (add-to-list 'TeX-command-list '("Info" "makeinfo %t" TeX-run-compile nil
                                     (texinfo-mode) :help "Run makeinfo with Info output"))
    (add-to-list 'TeX-command-list '("Info HTML" "makeinfo --html %t" TeX-run-compile nil
                                     (texinfo-mode) :help "Run makeinfo with HTML output"))
    (add-to-list 'TeX-command-list '("PDF" "%(o?)dvipdf %d %a " TeX-run-command t t
                                     :help "Generate PDF file"))
    (add-to-list 'TeX-command-list '("Make" "make %M " TeX-run-command t t
                                     :help "Generate with a Makefile"))
    (require 'texmathp)
    (eval-after-load 'texmathp
      (progn
        (setq texmathp-tex-commands '(("equations" env-on)
                                      ("\\halign"  arg-off)))
        (texmathp-compile) ))
    (setq TeX-parse-self t)
    (setq TeX-auto-save  t))
  (local-set-key "\C-c`" 'TeX-next-error)
  (local-set-key "\M-gn" 'TeX-next-error)
  (local-set-key "\M-gp" 'TeX-previous-error))

(defun my-plain-TeX-mode-hook ()
  "Personal configuration for GenTeX and plain TeX."
  (local-set-key "\C-cp" 'TeX-PDF-mode)
  (local-set-key "\C-c[" 'TeX-insert-braces) ; why shift?
  (local-set-key "\C-c\C-n" nil)
  (local-set-key "\C-c\C-m" nil)
  (local-set-key [?\C-c return] 'TeX-insert-macro)
  ;; electric $ balks on embedded $ but try again for now
  ;(local-set-key "$" 'self-insert-command)
  ;; GenTeX specific font specifiers
  (setq TeX-font-list '( (?\C-b "\\textbold{" "}" "\\mathbold{" "}") 
			 (?\C-c "\\smcaps{" "}")
			 (?\C-e "\\emph{" "}")
			 (?\C-i "{\\it " "\\/}")
			 (?\C-r "{\\rm " "}")
			 (?\C-s "\\sans{" "}")
			 (?\C-t "\\tty{" "}")
			 (?\C-u "{" "}")
			 (?\C-d "" "" t)
			 (?\M-p "{\\xlarge " "}")
			 (?\C-p "{\\large " "}")
			 (?\C-m "{\\medium " "}")
			 (?\C-n "{\\small " "}")
			 (?\M-n "{\\xsmall " "}")
			 ))
  (LaTeX-add-environments '("equations" LaTeX-env-label)) 
  (local-set-key "\C-c\C-e" 'LaTeX-environment)
  (setq LaTeX-default-environment "itemize"))
  
(add-hook 'plain-TeX-mode-hook 'highlight-attn-words)
(add-hook 'plain-TeX-mode-hook 'turn-on-flyspell) 
(add-hook 'plain-TeX-mode-hook 'turn-on-tex-parser-ispell)
(add-hook 'plain-TeX-mode-hook 'my-plain-TeX-mode-hook)
(add-hook 'plain-TeX-mode-hook 'my-generic-TeX-setup)

(defun my-LaTeX-mode-hook ()
  "Personal configuration for LaTeX."
  (TeX-PDF-mode 1))

(add-hook 'LaTeX-mode-hook 'highlight-attn-words)
(add-hook 'LaTeX-mode-hook 'turn-on-tex-parser-ispell)
(add-hook 'LaTeX-mode-hook 'turn-on-flyspell) 
(add-hook 'LaTeX-mode-hook 'my-LaTeX-mode-hook)
(add-hook 'LaTeX-mode-hook 'my-generic-TeX-setup)

(autoload 'LaTeX-environment "latex" "Make Environment Using AucTeX commands" t)
(autoload 'LaTeX-add-environments "latex" "Add Environment Info Using AucTeX commands" t)

(add-hook 'AmS-TeX-mode-hook 'my-generic-TeX-setup)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; BibTeX-mode

(setq bibtex-mode-hook
      '((lambda ()
	  (local-set-key "\"" 'self-insert-command)
	  (setq bibtex-include-OPTannote nil)
	  (setq bibtex-include-OPTcrossref nil)
	  )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                           ;;
;;(@* "Other Modes and Features")                            ;;;;;;
;;                                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Calendar Mode

(unless (featurep 'astronomy)
  (require-soft 'astronomy))

(when (featurep 'astronomy)
  (defun astronomy-twilight-time-to-time-string (time-list)
    "Converts a twilight time list as returned by
`solar-astronomical-twilight' from astronomy.el into a time
string. TIME-LIST is of the form (TIME ZONE) where TIME is a
decimal time and ZONE is a string, e.g., EST. The result is
converted into an hh:mm ZONE string. Treats the boundary case of
a time within 30 seconds of midnight by saying \"midnight\"."
    (let* ((hour (floor (car time-list)))
           (min (round (* 60.0 (- (car time-list) hour))))
           (zone (cadr time-list)))
      (format "%s %s"
              (if (and (= min 60) (= hour 23))
                  "midnight"
                (format "%02d:%02d" hour min))
              zone)))

  (defun calendar-astronomical-twilight (&optional event)
    "Local time of astronomical twilight, morning and evening,
for date under cursor. Accurate to a few seconds."
    (interactive (list last-nonmenu-event))
    (or (and calendar-latitude calendar-longitude calendar-time-zone)
        (solar-setup))
    (let* ((date (calendar-cursor-to-date t event))
           (twilight (solar-astronomical-twilight date))
           (morning (car twilight))
           (evening (cadr twilight)))
      (message "Astronomical twilight on %s: %s (morning) and %s (evening)"
               (calendar-date-string date t t)
               (astronomy-twilight-time-to-time-string morning)
               (astronomy-twilight-time-to-time-string evening)))))

(defun my-calendar-mode-hook ()
  (local-set-key "\C-w"    'calendar-scroll-right-three-months)
  (local-set-key "\M-p"    'calendar-backward-month)
  (local-set-key "\M-n"    'calendar-forward-month)
  (local-set-key "\M-w"    'calendar-beginning-of-year)
  (local-set-key "\M-v"    'calendar-end-of-year)
  (local-set-key "\C-\M-p" 'calendar-backward-year)
  (local-set-key "\C-\M-n" 'calendar-forward-year)
  (local-set-key "L"       'lunar-phases)
  (when (featurep 'astronomy)
    (local-set-key "T" 'calendar-astronomical-twilight))
  )

(add-hook 'calendar-mode-hook 'my-calendar-mode-hook)

(setq calendar-date-display-form
     '((when dayname (concat (substring dayname 0 3) " "))
       (format "%02d" (string-to-number day)) " " (substring monthname 0 3) " " year))

(add-hook 'calendar-today-visible-hook 'calendar-mark-today)

(setq calendar-week-start-day 1)  ; Start week on monday

(setq calendar-font-lock-keywords ; Change how weekends are highlighted 
      (subst 'font-lock-keyword-face 'font-lock-comment-face calendar-font-lock-keywords))

; (setq calendar-font-lock-keywords
;       (mapcar (lambda (seq)
;                 (let
;                     ((sub (substitute 'font-lock-keyword-face
;                                       'font-lock-comment-face
;                                       (list (car seq) (cdr seq)))))
;                   (cons (car sub) (cadr sub))))
;               calendar-font-lock-keywords)) ; Change how weekends are highlighted 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Formats

(defvar my-date-format "%Y %b %d %a"
  "My favorite date format; see format-time-string for details.")
(setq header-date-format "%Y %b %d %a %H:%M:%S")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; IBuffer

(defun ibuffer-list-buffers-select (&optional noselect)
  "Display a list of buffers, in another window.
If optional argument FILES-ONLY is non-nil, then add a filter for
buffers which are visiting a file."
  (interactive "P")
  (let ((starting-buffer (buffer-name (current-buffer))))
    (ibuffer t nil nil noselect)
    (if (and (null noselect)
             ibuffer-start-at-most-recent
             (eq major-mode 'ibuffer-mode))  ; only do this in the ibuffer window
        (ibuffer-jump-to-buffer starting-buffer))))

(defun ibuffer-match-my-files (label &optional subdir pattern)
  "Create ibuffer filter groups associated with specified file or
subdirectory names in my home directory. Files in the group are
those that match the path, which can be only partially specified.
LABEL is the ibuffer name associated with the filter group.
SUBDIR is a subdirectory path that restricts the files selected
if non-nil. It should not start with / in general.
PATTERN is a regexp that limits the files considered within
the subdirectory.
   The return value of the function is a list containing the
list entry specifying the filter group, so that it can be included
in the filter group list with ,@ and will not affect the resulting
list if no such files exist."
  (let* ((subdir (or subdir ""))
         (nodes  (mapcar (lambda (x) `(filename . ,x))
                         (directory-files (concat my-home-dir subdir) t pattern))))
    (if (= (length nodes) 0) nil
      (list (cons label (if (= (length nodes) 1) nodes (list (cons 'or nodes))))))))

(defvar ibuffer-start-at-most-recent nil
  "If non-nil, \\[ibuffer-list-buffers-select] will, if selecting the
ibuffer listing window, put the cursor on the line corresponding to the
most recent buffer. Otherwise, the cursor is positioned on the top
buffer line in the ibuffer listing.")

(defvar my-ibuffer-loaded nil
  "Variable used to control loading of ibuffer customized setup.
Should always be defined as NIL initially.")

(defun my-ibuffer-mode-hook () 
  ;; Setup definitions and filter groups.
  ;; To avoid worrying about requires, handle the setup
  ;; in the hook but only on the first time called.
  (do-only-once my-ibuffer-loaded
    (require 'ibuf-ext) ;; Only required as need and when ibuffer exists
    (define-ibuffer-filter mode-nostar
      "Toggle current view to buffers with major mode QUALIFIER
     and no star at beginning of the buffer name."
      (:description "major mode with no star in name"
                    :reader
                    (intern
                     (completing-read "Filter by major mode: " obarray
                                      #'(lambda (e)
                                          (string-match "-mode$"
                                                        (symbol-name e)))
                                      t
                                      (let ((buf (ibuffer-current-buffer)))
                                        (if (and buf (buffer-live-p buf))
                                            (with-current-buffer buf
                                              (symbol-name major-mode))
                                          "")))))
      (and
       (not (string-match "^\\s-*\\*" (buffer-name buf)))
       (eq qualifier (with-current-buffer buf major-mode))))
    (setq ibuffer-saved-filter-groups
          `(("default"
             ,@(when (file-directory-p (concat my-home-dir "class"))
                 (ibuffer-match-my-files "Teaching" "class/" "^s"))
             ("Documents" (or (mode . plain-tex-mode)
                              (mode . latex-mode)
                              (mode . bibtex-mode)
                              (mode . amstex-mode)
                              (mode-nostar . org-mode)
                              (mode-nostar . fundamental-mode)
                              (mode . nxml-mode)
                              (mode . nxhtml-mode)
                              (mode . css-mode)))
             ,@(when (file-directory-p (concat my-home-dir "Documents"))
                 (ibuffer-match-my-files "Android" "Documents/" "^Eclipse"))
             ,@(when (file-directory-p (concat my-home-dir "Programming"))
                 (ibuffer-match-my-files "Arduino" "Programming/" "^Arduino"))
             ("Code" (or (mode . c-mode)
                         (mode . python-mode)
                         (mode . java-mode)
                         (mode . ess-mode)
                         (mode . ruby-mode)
                         (mode . clojure-mode)
                         (mode . lisp-mode)
                         (mode . js2-mode)
                         (mode . haskell-mode)
                         (mode . perl-mode)
                         (mode . cperl-mode)
                         (mode . tuareg-mode)
                         (mode . c++-mode)
                         (mode . sh-mode)
                         (mode . php-mode)
                         (mode . objc-mode)
                         (mode . processing-mode)
                         (mode . arduino-mode)
                         (mode . ps-mode)
                         (mode . R-mode)
                         (mode . r-mode)
                         (mode . R-transcript-mode)
                         (mode . r-transcript-mode)
                         (filename . "[Mm]akefile")))
             ("Emacs" (mode-nostar . emacs-lisp-mode))
             ("Shells" (or (mode . shell-mode)
                           (mode . comint-mode)
                           (mode . inferior-ess-mode)
                           (mode . eshell-mode)
                           (mode . lisp-interaction-mode)
                           (name . "^\\*[iI][pP]ython\\*")
                           (mode . slime-repl-mode)))
             ("Dirs"  (mode . dired-mode))
             ("Help"  (or (name . "^\\*Help\\*")
                          (name . "^\\*help\\[R\\]")
                          (name . "^\\*Man\\s-")
                          (name . "^\\*Apropos\\*")
                          (mode . help-mode)
                          (mode . Man-mode)
                          (mode . Info-mode)))
             ("Utility" (name . "^\\*.*\\*\\(<[0-9]+>\\)?$"))
             ("Other" (name . "^\\S-")))
                  ("all"
                   ("All" (name . ".")))
                  )))
  ;; Normal operational settings.
  (setq ibuffer-start-at-most-recent t)
  (ibuffer-auto-mode 1)
  (local-set-key "\M-ss"   'ibuffer-do-isearch)
  (local-set-key "\M-sr"   'ibuffer-do-isearch-regexp)
  (local-set-key "\M-\C-f" 'ibuffer-forward-filter-group)
  (local-set-key "\M-\C-b" 'ibuffer-backward-filter-group)
  (local-set-key "/F"      (lambda () (interactive) (ibuffer-filter-by-filename ".*")))
  (setq ibuffer-show-empty-filter-groups nil)
  (ibuffer-switch-to-saved-filter-groups "default"))

(add-hook 'ibuffer-mode-hook 'my-ibuffer-mode-hook)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Intelligent Copy and Kill

  ;; ATTN: not done yet

(defvar my-noregion-copykill-thing-default-order
  '((url . nearest)
    (word . nearest)
    (line . at)
    (page . at)
    (sentence . at)
    (paragraph . at)
    (sexp . nearest)
    (defun . nearest))
  "A list defining the order and scope of things
that are captured by smart copy and kill operations.
Each element of the list is a cons cell of the form
(THING . SCOPE) where THING is a symbol that corresponds
to a capturable thing in thingatpt/thingatpoint+ and
SCOPE is either the symbol at or nearest, corresponding
to the functions thing-at-point or thing-nearest-point.
The nearest scope only works if thingatpt+ is available,
otherwise it is transformed to at. The default")

 
(defvar my-noregion-copykill-thing-order-alist
  '((emacs-lisp-mode .
      ((sexp . nearest)
       (defun . nearest)
       (line . at)
       (page . at)
       (word . nearest)
       (sentence . at))))
  "")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                           ;;
;;(@* "ESS Mode for R and S Code")                           ;;;;;;
;;                                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ESS-mode for S 


;;
;; Formatting Style 
;;

(setq ESS-crg-style '(CRG (ess-indent-level . 4)
                          (ess-continued-statement-offset . 4)
                          (ess-continued-brace-offset . -4 )
                          (ess-brace-offset . 0)
                          (ess-brace-imaginary-offset . 0)
                          (ess-close-brace-offset . 0)
                          (ess-else-offset . 0)
                          (ess-expression-offset . 0)
                          (ess-arg-function-offset . 0)
                          (ess-fancy-comments . nil)
                          (ess-auto-newline . t)
                          (ess-tab-always-indent . t)
                   ))

;;
;; Basic Configuration
;;

(defun my-ess-mode-load-hook ()
  (if (not (assoc 'CRG ess-style-alist))
      (setq ess-style-alist (cons ESS-crg-style ess-style-alist)))
  (setq ess-default-style 'CRG)
  (setq ess-style 'CRG)
  (setq ess-auto-newline t)
  (setq-default ess-loop-timeout '1000000)
  (setq-default inferior-ess-help-command "help(\"%s\")\n")
  (setq ess-use-toolbar nil)
  (setq ess-fancy-comments nil)
  )

(defun my-inferior-ess-mode-hook ()
  ;; Key bindings
  (local-set-key "\C-d" 'delete-char)
  (local-set-key "\C-a" 'my-comint-bol)
  (local-set-key "\C-c\M-p" 'comint-backward-matching-input)
  (local-set-key "\C-c\M-n" 'comint-forward-matching-input)
  ;; Comint configuration
  (add-hook 'comint-input-filter-functions 'ess-search-path-tracker nil t)
  (setq comint-move-point-for-output 'all) ; follow output after eval or entry
  (setq input-ring-size '1024)
  ;; Style Configuration (these three lines needed?)
  (setq ess-style 'CRG) 
  (setq ess-auto-newline t)
  (ess-set-style)
  ;; General ESS config
  (setq inferior-ess-help-command "help(\"%s\")\n")
  )

(defun my-ess-pre-run-hook ()
  (progn
    (remove-hook 'ess-mode-hook             'turn-on-font-lock)
    (remove-hook 'ess-transcript-mode-hoonk 'turn-on-font-lock)
    (remove-hook 'inferior-ess-mode-hook    'turn-on-font-lock)
    )
  )

(defun my-ess-mode-hook ()
  (setq ess-style 'CRG)
  (setq ess-auto-newline t)
  (ess-set-style)
  )

(defun ess-transcript-mode-hook ()
  (setq ess-style 'CRG)
  (setq ess-auto-newline t)
  (ess-set-style)
  )

;;
;; Enable the hooks
;;

(add-hook 'ess-mode-load-hook       'my-ess-mode-load-hook)
(add-hook 'inferior-ess-mode-hook   'my-inferior-ess-mode-hook)
(add-hook 'ess-pre-run-hook         'my-ess-pre-run-hook)
(add-hook 'ess-mode-hook            'my-ess-mode-hook)
(add-hook 'ess-transcript-mode-hook 'my-ess-transcript-mode-hook)
(require-soft 'ess-site)

(if (not (featurep 'ess))
    (message "Cannot load ESS."))

;; (setq ess-style-alist (cons ESS-crg-style ess-style-alist)) ;;  needed?



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                           ;;
;;(@* "Global Key Bindings")                                 ;;;;;;
;;                                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Auxilliary Global Keymaps
;;
    ;; String Replacement Commands

(define-prefix-command 'my-string-replace-prefix 'my-string-replace-map "String Replacement")
(define-key my-string-replace-map "q" 'query-replace)
(define-key my-string-replace-map "x" 'query-replace-regexp)
(define-key my-string-replace-map "s" 'replace-string)
(define-key my-string-replace-map "r" 'replace-regexp)
(define-key my-string-replace-map "t" 'tags-query-replace)

(when (featurep 'icicles)
  (define-key my-string-replace-map [escape] (make-sparse-keymap)) ; to allow icicle-complete-keys
  (add-to-list 'icicle-keymaps-for-key-completion 'my-string-replace-map t))

    ;; Transpose Commands

(define-prefix-command 'my-transpose-prefix 'my-transpose-map "Transpose Commands")
(define-key my-transpose-map "c"   'transpose-chars)
(define-key my-transpose-map "l"   'transpose-lines)
(define-key my-transpose-map "p"   'transpose-paragraphs)
(define-key my-transpose-map "s"   'transpose-sentences)
(define-key my-transpose-map "w"   'transpose-words)
(define-key my-transpose-map "x"   'transpose-sexps)

    ;; Tag Management Commands

(define-prefix-command 'my-tags-prefix 'my-tags-map "Tag Management")
(define-key my-tags-map "l"   'list-tags)
(define-key my-tags-map "."   'find-tag)
(define-key my-tags-map "f"   'find-tag)
(define-key my-tags-map "r"   'find-tag-regexp)
(define-key my-tags-map "4"   'find-tag-other-window)
(define-key my-tags-map "5"   'find-tag-other-frame)
(define-key my-tags-map "n"   'find-tag-noselect)
(define-key my-tags-map "c"   'tags-loop-continue)
(define-key my-tags-map "s"   'tags-search)
(define-key my-tags-map "\C-i" 'complete-tag)
(define-key my-tags-map "t"   'select-tags-table)
(define-key my-tags-map "v"   'visit-tags-table)
(define-key my-tags-map "z"   'tags-reset-tags-table)
(define-key my-tags-map "a"   'tags-apropos)
(define-key my-tags-map "q"   'tags-query-replace)

(when (featurep 'icicles)
  (define-key my-tags-map [escape] (make-sparse-keymap)) ; to allow icicle-complete-keys
  (add-to-list 'icicle-keymaps-for-key-completion 'my-tags-map t))

    ;; Marks and so forth (e.g., Bookmarks)

(defvar my-mark-map (copy-keymap bookmark-map) "Mark/Bookmark Management")
(fset 'my-mark-prefix my-mark-map)

(when (featurep 'icicles)
  (define-key my-mark-map [escape] (make-sparse-keymap)) ; to allow icicle-complete-keys
  (add-to-list 'icicle-keymaps-for-key-completion 'my-mark-map t))

    ;; Go Map  (Goto's and Jumps of All Kinds)

(define-prefix-command 'my-go-prefix 'my-go-map "Go")
(define-key my-go-map "g"      'goto-line)
(define-key my-go-map "\M-g"   'goto-line)
(define-key my-go-map "h"      'help-command)
(when (featurep 'linkd)
  (define-key my-go-map "l"    'linkd-follow-at-point))
(define-key my-go-map "n"      'next-error)
(define-key my-go-map "\M-n"   'next-error)
(when (featurep 'org-install)
  (define-key my-go-map "o"    'org-open-at-point))
(define-key my-go-map "p"      'previous-error)
(define-key my-go-map "\M-p"   'previous-error)
(define-key my-go-map "q"      'keyboard-quit)  
(define-key my-go-map "t"      'top-level)
(define-key my-go-map "u"      'browse-url)
(define-key my-go-map "x"      'exit-recursive-edit)
(define-key my-go-map [escape] 'keyboard-escape-quit)

(when (featurep 'icicles)
  (define-key my-go-map [escape] (make-sparse-keymap)) ; to allow icicle-complete-keys
  (add-to-list 'icicle-keymaps-for-key-completion 'my-go-map t))

(when (featurep 'semantic)
  (define-prefix-command 'my-go-semantic-prefix 'my-go-semantic-map "Go Places with Semantic")
  (when (featurep 'senator)
    (define-key my-go-semantic-map "j" 'senator-jump)
    (define-key my-go-semantic-map "n" 'senator-next-tag)
    (define-key my-go-semantic-map "p" 'senator-previous-tag)
    (define-key my-go-semantic-map "u" 'senator-go-to-up-reference))
  (define-key my-go-semantic-map "B" 'semantic-mrub-switch-tags)
  (define-key my-go-semantic-map "c" 'semantic-complete-jump)
  (define-key my-go-semantic-map "f" 'semantic-ia-fast-jump)
  (define-key my-go-semantic-map "h" 'semantic-analyze-proto-impl-toggle)
  (define-key my-go-semantic-map "r" 'semantic-symref)
  (define-key my-go-semantic-map "s" 'semantic-symref-symbol)
  (define-key my-go-semantic-map "x" 'semantic-symref-regexp)
  (define-key my-go-map "s" 'my-go-semantic-prefix))

(if (featurep 'bookmark+)
    (progn
      (define-key my-go-map "b"    'bmkp-jump-map)
      (define-key my-go-map "B"    'bmkp-jump-other-window-map))
  (define-key my-go-map "b"    'bookmark-jump)
  (define-key my-go-map "B"    'bookmark-jump-other-window))


    ;; Abbrevs

(define-prefix-command 'my-abbrev-prefix 'my-abbrev-map "Abbreviations")
(define-key my-abbrev-map "c"      'dabbrev-completion)
(define-key my-abbrev-map "d"      'define-global-abbrev)
(define-key my-abbrev-map "\C-d"   'define-mode-abbrev)
(define-key my-abbrev-map "e"      'expand-abbrev)
(define-key my-abbrev-map "g"      'add-global-abbrev)
(define-key my-abbrev-map "\C-g"   'inverse-add-global-abbrev)
(define-key my-abbrev-map "k"      'kill-all-abbrevs)
(define-key my-abbrev-map "l"      'list-abbrevs)
(define-key my-abbrev-map "m"      'add-mode-abbrev)
(define-key my-abbrev-map "\C-m"   'inverse-add-mode-abbrev)
(define-key my-abbrev-map "r"      'expand-region-abbrevs)
(define-key my-abbrev-map "u"      'unexpand-abbrev)
(define-key my-abbrev-map "v"      'edit-abbrevs)
(define-key my-abbrev-map "x"      'dabbrev-expand)
(define-key my-abbrev-map " "      'set-abbrev-mark)

(when (featurep 'icicles)
  (define-key my-abbrev-map [escape] (make-sparse-keymap)) ; to allow icicle-complete-keys
  (add-to-list 'icicle-keymaps-for-key-completion 'my-abbrev-map t))

    ;; Org-Mode

(define-prefix-command 'my-org-prefix 'my-org-map "Org Mode")
(when (featurep 'org-install)
  (define-key my-org-map "a" 'org-agenda)
  (define-key my-org-map "c" 'org-capture)
  (define-key my-org-map "l" 'org-store-link)
  (define-key my-org-map "A" 'org-attach)
  (define-key my-org-map "3" 'calendar))

(when (featurep 'icicles)
  (define-key my-org-map [escape] (make-sparse-keymap)) ; to allow icicle-complete-keys
  (add-to-list 'icicle-keymaps-for-key-completion 'my-org-map t))

    ;; Icicles

(define-prefix-command 'my-icicles-prefix 'my-icicles-map "Icicles")
(when (featurep 'icicles)
  (define-key my-icicles-map "b" 'icicle-search-buffer)
  (define-key my-icicles-map "c" 'icicle-imenu-command)
  (define-key my-icicles-map "f" 'icicle-search-file)
  (define-key my-icicles-map "g" 'icicle-search-generic)
  (define-key my-icicles-map "o" 'icicle-occur)
  (define-key my-icicles-map "m" 'icicle-search-bookmark)
  (define-key my-icicles-map "M" 'icicle-search-bookmarks-together)
  (define-key my-icicles-map "s" 'icicle-search)
  (define-key my-icicles-map "t" 'icicle-complete-thesaurus-entry) ; from "\C-c/" 
  (define-key my-icicles-map "$" 'icicle-search-word)
  (define-key my-icicles-map "^" 'icicle-search-keywords)
  (define-key my-icicles-map "`" 'icicle-compilation-search)
  (define-key my-icicles-map "=" 'icicle-imenu)
  (define-key my-icicles-map [?\"] 'icicle-search-char-property) 
  (define-key my-icicles-map [tab] 'icicle-comint-command)
  (define-key my-icicles-map [escape] (make-sparse-keymap)) ; to allow icicle-complete-keys
  (add-to-list 'icicle-keymaps-for-key-completion 'my-icicles-map t))

    ;; Anything

(define-prefix-command 'my-anything-prefix 'my-anything-map "Anything Interface")
(when (featurep 'anything)
  (define-key my-anything-map "a" 'anything)
  (define-key my-anything-map "b" 'anything-bookmarks)
  (define-key my-anything-map "c" 'anything-browse-code))

(when (featurep 'icicles)
  (define-key my-anything-map [escape] (make-sparse-keymap)) ; to allow icicle-complete-keys
  (add-to-list 'icicle-keymaps-for-key-completion 'my-anything-map t))

    ;; Links

(define-prefix-command 'my-links-prefix 'my-links-map "Links")
(when (featurep 'org)
  (define-key my-links-map "o" 'org-store-link))
(when (featurep 'linkd)
  (define-key my-links-map "b" 'linkd-block-around-point)
  (define-key my-links-map "e" 'linkd-edit-link-at-point)
  (define-key my-links-map "i" 'linkd-insert-link)
  (define-key my-links-map "p" 'linkd-process-block)
  (define-key my-links-map "s" 'linkd-send-block-to-shell)
  (define-key my-links-map [return] 'linkd-follow-at-point)
  (define-key my-links-map "\C-n" 'linkd-next-link)
  (define-key my-links-map "\C-p" 'linkd-previous-link))

(when (featurep 'icicles)
  (define-key my-links-map [escape] (make-sparse-keymap)) ; to allow icicle-complete-keys
  (add-to-list 'icicle-keymaps-for-key-completion 'my-links-map t))


;;
;; Keybinding Setters
;;

(defun my-set-global-sysdep-keybindings ()
  "Sets global keybindings that are system
   dependent (operating system or emacs version)."
   nil
  )

(defun my-set-global-keybindings ()
  "Adjusts default global keybindings to
   conform to my preferences."
  ;;
  ;; Movement
  ;;
    (global-set-key "\C-a"        'my-move-beginning-of-line)
    (global-set-key "\C-w"        'scroll-down-or-beg)
    (global-set-key "\C-v"        'scroll-up-or-end)
    (global-set-key "\M-v"        'end-of-buffer)
    (global-set-key "\M-w"        'beginning-of-buffer)
    (global-set-key "\M-\C-v"     'scroll-other-window)
    (global-set-key "\M-\C-w"     'scroll-other-window-down)
    (global-set-key "\M-p"        'backward-up-list)
    (global-set-key "\M-n"        'up-list)
    (global-set-key "\M-\C-f"     'forward-sexp-or-char)
    (global-set-key "\M-\C-b"     'backward-sexp-or-char)
    (global-set-key [?\A-a]       'back-to-indentation)
  ;;
  ;; Basic Functions
  ;;
    (global-set-key "\C-x\C-m"    'execute-extended-command)
    (global-set-key "\C-c\C-m"    'execute-extended-command)
    (when (featurep 'lacarte)
      (global-set-key "\C-x\M-m"  'lacarte-execute-command)
      (global-set-key "\C-c\M-m"  'lacarte-execute-command))
    (global-set-key [?\C-0]       'universal-argument)
  ;;
  ;; Help
  ;;   I've moved the help command to enable more efficient
  ;;   access to some editing commands. But this necessitates
  ;;   a bit of extra work to ensure consistency elswhere,
  ;;   especially isearch and icicles (see below for the former)
  ;;   and (@> "File and Navigation Modes") for the latter.
  ;;
    (define-key help-map "\C-c"   'describe-key-briefly)
    (define-key help-map "\M-c"   'describe-copying)
    (define-key help-map "\C-f"   'describe-face)
    (define-key help-map "F"      'view-emacs-FAQ)
    (define-key help-map "\C-i"   'Info-goto-emacs-command-node) ; might be easier in practice
    (define-key help-map "I"      'Info-goto-emacs-command-node) ; for help-mode consistency
    (define-key help-map "\M-i"   'describe-input-method)        ; not often needed
    (define-key help-map "T"      'describe-text-properties) 
    (dolist (event my-help-events)
      (global-set-key event 'help-command)) 
  ;;
  ;; Searching
  ;;
    (global-set-key "\M-s"    'isearch-forward-regexp)
    (global-set-key "\M-r"    'isearch-backward-regexp)
    (global-set-key "\C-\M-s" 'query-replace)
    (global-set-key "\C-\M-r" 'query-replace-regexp)
    (global-set-key [?\C-5]   'query-replace)
    (global-set-key [?\C-6]   'query-replace-regexp)
    (defalias 'qr  'query-replace)
    (defalias 'qrr 'query-replace-regexp)
    ;; To end a search with a specially defined key, like a keypad key
    ;; and to have the search not automatically continue with a single
    ;; C-s, do the following for the key.  For example, this makes
    ;; kp-2 act like a proper search ender.
    ;; (define-key isearch-mode-map [kp-2] 'isearch-other-control-char)
    (define-key isearch-mode-map "\M-w" 'isearch-yank-word-or-char)
    (define-key isearch-mode-map "\C-w" 'isearch-other-control-char)
    (define-key isearch-mode-map [(control return)] 'isearch-exit)
    (define-key isearch-mode-map (car my-help-events) (lookup-key isearch-mode-map "\C-h"))
    (define-key isearch-mode-map "\C-h" 'isearch-delete-char)
    (define-key minibuffer-local-isearch-map "\M-s" 'isearch-forward-exit-minibuffer)
    (define-key minibuffer-local-isearch-map "\M-r" 'isearch-reverse-exit-minibuffer)
    (define-key minibuffer-local-isearch-map [(control meta tab)] 'isearch-complete-edit)
    (define-key minibuffer-local-isearch-map [(meta control ?i)]  nil)
  ;;
  ;; Kills and Copies
  ;;
    (global-set-key "\C-k"            'kill-line)
    (global-set-key "\M-k"            'kill-region)
    (global-set-key "\M-\C-k"         'kill-sexp)
    (global-set-key [?\S-\C-k]        'kill-sentence)
    (global-set-key [?\S-\M-\C-k]     'kill-paragraph)
    (global-set-key [?\A-k]           'kill-whole-line)
    (global-set-key [?\M-B]           'backward-kill-word)
    (global-set-key [?\A-b]           'backward-kill-word)
    (global-set-key [\M-backspace]    'backward-kill-word)
    (global-set-key [?\A-K]           'backward-kill-sentence)
    (global-set-key [?\S-\C-\A-k]     'backward-kill-paragraph)
    (global-set-key [?\C-\A-k]        'backward-kill-sexp)
    (global-set-key [\C-\S-backspace] 'backward-kill-sentence)
    (global-set-key [\C-\M-backspace] 'backward-kill-sexp)
    (global-set-key "\M-c"            'my-kill-ring-save) ;alternate: my-copy-region-as-kill
    (global-set-key "\M-\C-y"         'append-next-kill)
    (global-set-key "\C-z"            'zap-to-char)
    (global-set-key "\M-z"            'undo)
    (global-set-key "\C-h"            'delete-backward-char) ; Move help command below
    (global-set-key "\M-h"            'backward-kill-word)
  ;;
  ;; Marking
  ;;
    (global-set-key [?\M- ]       'mark-sexp)
    (global-set-key [?\C-\M- ]    'mark-defun)
    (global-set-key "\M-m"        'mark-paragraph)
    (global-set-key "\C-\M-m"     'mark-end-of-sentence)
    (global-set-key [?\S-\C-\M-m] 'mark-page)
    (global-set-key [?\A-m]       'mark-whole-buffer)
    (global-set-key [?\A-M]       'mark-word)
  ;;
  ;; Buffers, Files, Windows, and Frames
  ;;
    (global-set-key "\C-x\C-b"    'ibuffer-list-buffers-select)
    (global-set-key [?\C-x ?\C-\S-b] 'ibuffer-list-buffers) ; no select and files arg
    (global-set-key "\C-xy"       'bury-buffer)
    (global-set-key "\C-xo"       'other-window)         ;; also bound to C-x C-o
    (global-set-key "\C-x\C-o"    'other-window)         ;; same to avoid collisions
    (global-set-key "\C-x\M-w"    'append-to-file)
    (global-set-key "\C-x\M-\C-f" 'find-file-at-point)
    (global-set-key "\C-x:"       'view-url-in-buffer)
    (global-set-key "\C-\M-z"     'iconify-or-deiconify-frame)
  ;;
  ;; Positioning
  ;;
    (global-set-key "\M-l"        'reposition-window)
    (global-set-key "\M-\C-l"     'move-to-window-line)
    (global-set-key "\C-xL"       'count-lines-page) ;; ATTN replace with something smarter
    (global-set-key "\C-xl"       'what-line)
    (global-set-key "\C-xg"       'goto-line)    ;; ATTN: used to be C-xt
    (global-set-key "\M-g"        'my-go-prefix) ;; replaces similar map
  ;;
  ;; Macros   
  ;;
    (global-set-key "\C-x\M-k"    'kmacro-keymap)
    (global-set-key [?\C-3]       'kmacro-start-macro-or-insert-counter)
    (global-set-key [?\C-4]       'kmacro-end-or-call-macro)
  ;;
  ;; Miscellaneous Operations
  ;;
    (global-set-key [?\C-7]       'align-regexp)  ;; very useful! (mnemnoic: & for align)
    (global-set-key "\C-x7"       'align-regexp)  
    (global-set-key [?\A-q]       'unfill-paragraph)
    (global-set-key [\A-return]   'delete-blank-lines)
    (global-set-key [?\A- ]       'just-one-space)
    (global-set-key [?\C-\A- ]    'delete-trailing-whitespace)
    (global-set-key "\C-\\"       'comment-dwim)
    (global-set-key "\M-/"        'hippie-expand)
  ;;
  ;; Rectangle and Register Commands
  ;;
    (global-set-key "\C-xra"      'append-to-register)
    (global-set-key "\C-xrc"      'copy-to-register)            
    (global-set-key "\C-xrl"      'list-registers)            
    (global-set-key "\C-xrp"      'prepend-to-register)
    (global-set-key "\C-xrs"      'string-rectangle)
    (global-set-key "\C-xrt"      'register-to-point)
    (global-set-key "\C-xrv"      'view-register)
    (global-set-key "\C-xrx"      'clear-rectangle)
    (global-set-key "\C-xr "      'point-to-register)
  ;;
  ;; Attach additional keymaps and related rebindings
  ;;
    (define-key ctl-x-map "t"  'my-tags-prefix)  ;; ATTN: used to be C-xg (see goto-line above)
    (define-key ctl-x-map "%"  'my-string-replace-prefix)
    (define-key ctl-x-map "m"  'my-mark-prefix)
    (define-key ctl-x-map "a"  'my-abbrev-prefix)
    (global-set-key [?\M-T]    'my-transpose-prefix)
    (global-set-key [?\A-t]    'my-transpose-prefix)
  ;;
  ;; Modified Arrow Keys get added functionality
  ;;
    (global-set-key [C-left]      'back-to-indentation)
    (global-set-key [C-right]     'end-of-line)
    (global-set-key [M-left]      'backward-word)
    (global-set-key [M-right]     'forward-word)
    (global-set-key [C-M-left]    'backward-list)
    (global-set-key [C-M-right]   'forward-list)
    (global-set-key [C-up]        'scroll-down-or-beg)
    (global-set-key [C-down]      'scroll-up-or-end)
    (global-set-key [M-up]        'backward-paragraph)
    (global-set-key [M-down]      'forward-paragraph)
    (global-set-key [C-M-up]      'backward-page)
    (global-set-key [C-M-down]    'forward-page)
  ;;
  ;; System Dependent Global Bindings
  ;;
    (my-set-global-sysdep-keybindings)
  ;;
  ;; Third-Party Mode Global Access Keys
  ;;
    (global-set-key "\C-ca"       'my-anything-prefix)
    (global-set-key "\C-ci"       'my-icicles-prefix)
    (global-set-key "\C-cl"       'my-links-prefix)
    (global-set-key "\C-co"       'my-org-prefix)
    (when (featurep 'icicles)
      (global-set-key "\C-x\M-\C-o" 'icicle-switch-to/from-minibuffer))
    (when (featurep 'lacarte)
      (global-set-key [?\e ?\M-x]   'lacarte-execute-command)
      (global-set-key [?\M-`]       'lacarte-execute-command))
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                           ;;
;;(@* "Settings and Initialization")                         ;;
;;                                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Start-up configuration

(setq inhibit-startup-message 't)
(setq initial-major-mode 'lisp-interaction-mode) ; formerly fundamental-mode
(setq initial-frame-alist my-frame-alist)        ; see (@> "Environment Configuration")

  ; load custom-file in (@> "Initial State")
(setq custom-file (expand-file-name "~/.emacs.d/emacs-custom.el")) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Enable Disabled Commands

(put 'eval-expression  'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region  'disabled nil)
(put 'upcase-region    'disabled nil)
(put 'set-goal-column  'disabled nil)
(put 'scroll-left      'disabled nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Basic Operation

(defun my-enable-cua-rectangles ()
  "Turn on (only) the rectangle feature of cua mode.
Also, take care of conflicting keybindings with icicles. Should
be called before `my-set-completion-mode' in
`my-set-operating-state' so that icicles properly sets up its
bindings. But if called manually, then icy-mode should be cycled
on and off afterwards."
  (require-soft 'cua-rect)
  (when (featurep 'cua-rect)
    (setq cua-enable-cua-keys nil)
    (cua-mode t)
    (when (featurep 'icicles)
      (defun my-ctrl-return ()
        "When in minibuffer use `icicle-candidate-action',
       otherwise use `cua-set-rectangle-mark'."
        (interactive)
        (if (or (window-minibuffer-p (selected-window))
                (not (boundp 'cua-mode))
                (null cua-mode))
            (call-interactively 'icicle-candidate-action)
          (call-interactively 'cua-set-rectangle-mark)))
      (add-to-list 'icicle-top-level-key-bindings '(cua-set-rectangle-mark my-ctrl-return t)))))

(defun my-set-completion-mode (sym)
  "Choose among various intelligent completion methods
   according to symbol. Current choices are 'icicles, 'ido,
   'iswitchb, 'none."
   (cond
    ((equal sym 'icicles)
     (if (not (featurep 'icicles))
         (require-soft 'icicles))
     (if (featurep 'icicles)
         (icicle-mode 1)
       (message "Cannot load icicles")))
    ((equal sym 'ido)
     (ido-mode '1)
     (setq ido-case-fold t))
    ((equal sym 'iswitchb) ; Use smart buffer switching
     (iswitchb-mode '1)                      
     (setq iswitchb-case t))))
    
(defun my-set-operating-state ()
  "Set Emacs to preferred modes, maps, and more."
  (setq-default case-fold-search nil)
  (setq-default indent-tabs-mode nil)
  (setq-default next-line-add-newlines nil)
    
  (setq-default fill-column '72)
  (setq paragraph-start paragraph-separate)            ; Use blank lines to separate paragraphs by default
  (setq adaptive-fill-regexp "[ \t]*\\([>*%#]+ +\\)?") ; Fill around comment beginnings and yanked-messages
  (setq sentence-end-double-space nil)                 ; Allow frenchspacing
  (setq page-delimiter "^\\(\f\\|\n\n+\\)")            ; FF or 2+ consecutive blank lines

  (setq history-length 256)
  (setq print-length 1024)                 ; Give more information about objects in help
  (setq print-level  8)
  (setq eval-expression-print-length 1024) ; ...and in *elisp*
  (setq eval-expression-print-level  8)

  (if (>= emacs-major-version 23)
      (setq-default major-mode 'org-mode)
    (setq default-major-mode 'org-mode))   ; back to fundamental-mode if org-mode causes strange behavior??
  (setq my-keep-scratch-buf "*elisp*")     ; if nil, delete; if string, new name; otherwise leave alone.
  
  (setq display-time-24hr-format t)
  (display-time)                           ; Time on Mode Line
    
  (transient-mark-mode '1)                 ; Highlight region
  (setq kill-read-only-ok t)               ; OK to use kill to copy text in read-only buffer
  (if (fboundp 'menu-bar-mode)
      (menu-bar-mode  1))                  ; Make menu bar available, just in case
  (if (fboundp 'tool-bar-mode)
      (tool-bar-mode  -1))                 ; Disable tool bar
  (setq-default scroll-bar-mode 'right)    ; Put scroll bars on the right
  (toggle-scroll-bar  '1)                  ; Use scroll bars (mostly for visual sense of buffer size)
  (auto-compression-mode '1)               ; Auto view compressed files
  (turn-on-global-show-paren-mode)         ; show matching parens
  (setq indicate-empty-lines t)            ; show empty lines at end of file

  (setq browse-url-browser-function 'browse-url-firefox) ; use new tabs in firefox
  (setq browse-url-firefox-new-window-is-tab t)
  (setq browse-url-new-window-flag  t)

  (my-set-completion-mode 'icicles)
  (my-set-global-keybindings)
  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                           ;;
;;(@* "System Dependent Settings")                           ;;
;;                                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst my-platform
  (cond ((or (string-equal system-type "darwin")
             (eq window-system 'ns)
             (eq window-system 'mac))
	 'macosx)
	((or (string-equal system-type "ms-dos")
             (string-equal system-type "windows-nt")
	     (string-equal system-type "cygwin"))
	 'windows)
        (t
         'linux))
  "The platform on which we are currently running, derived from
the variables `system-type' and `window-system'. Value is a
symbol. For the moment, all *nix variants are converted to
`linux', though this can be generalized later if needed")

;; ATTN This is a bit of a mess below.
;; Put the settings in their own functions or settings
;; and use my-platform

;; Operating-system-specific settings

(let* ((ostype (getenv "OSTYPE"))
       (myos (and ostype (downcase (getenv "OSTYPE")))))
  (when myos
    (cond
     ((string-equal myos "linux")
      (setq list-directory-verbose-switches "-HlrtF")
      (setq list-directory-brief-switches   "-CHvF"))
     (t
      (setq list-directory-verbose-switches "-lrtF")
      (setq list-directory-brief-switches   "-CF"))
     )))


;; Non-terminal/Windowing System Settings

(when window-system
  (setq frame-title-format '("" invocation-name " <%b>"))
  (tooltip-mode -1)
  (mouse-wheel-mode t)
  (blink-cursor-mode t)
  (setq visible-bell 't)           
  )


;; Linux-specific settings

(when (eq window-system 'x)
  ;; Faces
  (set-face-background 'highlight "tan")   ;;; "red" is nice also
  ;; Command Defaults
  (setq dired-guess-shell-alist-user
        (list
         (list "\\.py\\'"    "python")
         (list "\\.pl\\'"    "perl")
         (list "\\.rb\\'"    "ruby")
         (list "\\.r\\'"     "R --no-save --no-readline --slave --file=?")
         ))
  )


;; Emacs Version Specific Settings

(when (equal emacs-major-version 23)
  (set-cursor-color "gray40")
  )


;; Mac OSX Specific Settings (see ~/.emacs.d/my-env.el for more)

(when (or (eq window-system 'ns)   ; Gnu Emacs 23+ (Cocoa/NextStep)
          (eq window-system 'mac)  ; Carbon Emacs 22
          (string-equal system-type "darwin"))
  (when window-system
    (if (eq window-system 'mac)
        (message "MAC CARBON SYSTEM CHECK")
      (message "MAC COCOA SYSTEM CHECK"))
    (setq default-frame-alist           ; new frames after initial one
          '((top . 48) (left . 24)   
            (width . 180) (height . 56)
            (cursor-color . "gray40")
            (menu-bar-lines . 1) (tool-bar-lines . 0))))

  ;;
  ;; Modifier Keys
  ;;
  (setq mac-command-modifier  'meta) ;;; Emacs 23 defaults are different
  (setq mac-option-modifier   'alt)
  (setq mac-function-modifier 'super)
  (setq mac-pass-command-to-system nil)  ; not sure about the last two
  (setq mac-pass-control-to-system nil)
  ;;
  ;; Mouse Emulation
  ;;
  ;; With Cocoa, use super (fn) for mouse 2 and alt (option) for mouse 3,
  ;; using shift with all combinations to indicate the down-mouse variety.
  ;; It's not yet obvious how to get the analogous drag versions as these
  ;; are produced from event pairs.
  ;;
  (setq mac-emulate-three-button-mouse 'reverse)  ; carbon only
  (define-key key-translation-map     [s-mouse-1]          [mouse-2])
  (define-key key-translation-map   [C-s-mouse-1]        [C-mouse-2])
  (define-key key-translation-map   [M-s-mouse-1]      [M-C-mouse-2])

  (define-key key-translation-map   [s-S-mouse-1]     [down-mouse-2])
  (define-key key-translation-map [C-s-S-mouse-1]   [C-down-mouse-2])
  (define-key key-translation-map [M-s-S-mouse-1] [M-C-down-mouse-2])

  (define-key key-translation-map     [A-mouse-1]          [mouse-3])
  (define-key key-translation-map   [C-A-mouse-1]        [C-mouse-3])
  (define-key key-translation-map   [M-A-mouse-1]      [M-C-mouse-3])

  (define-key key-translation-map   [A-S-mouse-1]     [down-mouse-3])
  (define-key key-translation-map [C-A-S-mouse-1]   [C-down-mouse-3])
  (define-key key-translation-map [M-A-S-mouse-1] [M-C-down-mouse-3])
  ;;
  ;; Command Defaults
  ;;
  (setq dired-guess-shell-alist-user
        (list
         (list "\\.pdf\\'"   "open")
         (list "\\.gif\\'"   "open")
         (list "\\.jpe?g\\'" "open")
         (list "\\.png\\'"   "open")
         (list "\\.tiff\\'"  "open")
         (list "\\.py\\'"    "python")
         (list "\\.pl\\'"    "perl")
         (list "\\.rb\\'"    "ruby")
         (list "\\.r\\'"     "R --no-save --no-readline --slave --file=?")
         ))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                           ;;
;;(@* "Faces")                                               ;;;;;;
;;                                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when window-system
    (progn
      (set-face-background 'region "lightgoldenrod2")  
      (set-face-background 'secondary-selection "gray")
      ;(when (facep 'calendar-today-face)
      ;    (set-face-foreground 'calendary-today-face "firebrick")
      ;    (set-face-bold-p 'calendar-today-face t)
      ;    (set-face-underline-p 'calendar-today-face nil))
      ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                           ;;
;;(@* "Initial State")                                       ;;
;;                                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load-file custom-file)   ;; holds customization-only customizations
(my-set-operating-state)

;; handle scratch buffer if requested
(my-aif (get-buffer "*scratch*")  
    (cond
     ((stringp my-keep-scratch-buf)
      (with-current-buffer it
        (rename-buffer my-keep-scratch-buf)
        (funcall initial-major-mode)))
     ((null my-keep-scratch-buf)
      (kill-buffer it))))

;; load additional environment variables, if any
;; Note: Mac OS X uses ~/.MacOSX/environment.plist
;; Might also want some emacs-specific environment settings.
(let ((envir (concat my-home-lisp-dir "my-env.el")))
  (if (file-exists-p envir)
    (load-file envir)))  

;; start in the shell
;;   use *unix* as the buffer name, rather than *shell*,
;;   to avoid completion conflicts with "*Shell Command Output*"
(let* ((shell-buf (generate-new-buffer-name "*unix*")))
  (shell shell-buf)
  (with-selected-window (get-buffer-window shell-buf)
    (delete-other-windows)))
