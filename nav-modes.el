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

