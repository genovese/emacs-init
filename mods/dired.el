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
;;   the copy/move. The commands `my/dired-move-file-from-elsewhere'
;;   and `my/dired-copy-file-from-elsewhere' are used to do that.
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
;;   Command `my/dired-find-file-or-subdir' acts like `dired-find-file'
;;   but opens directories in the same buffer. This also allows
;;   an option for listing switches to be modified for the
;;   subdirectory listed in this buffer.
;;
;; * Create new directory or new empty file in current subdirectory:
;;
;;   If a sub-directory is listed in the same buffer, opening a new
;;   file in that directory is annoying because find-file gives
;;   the parent as a default location, requiring extra typing.
;;   The commands `my/dired-create-directory-or-file' and
;;   `my/dired-create-file-or-directory' do exactly this.
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
;;   do this, so I've included `my/dired-mark-files-path-regexp'
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
;;   `my/dired-next-subdir' and `my/dired-prev-subdir' instead
;;   wrap-around to the last or first subdir. They can be used
;;   as direct replacements for the standard commands.
;;  
;; * Move to nth file in subdir with wraparound (negative) indexing:
;;
;;   Commands `my/dired-move-to-file-at-index' and `my/dired-move-to-last-file'
;;   provide easy indexed movement among the files within a dired
;;   sub-directory. Without prefix args, these commands move to the
;;   first and last file in the directory, respectively.
;;   
;; * Find file read only:
;;     `my/dired-find-file-read-only'
;;
;; * Find file in other window or frame (depending on prefix arg)
;;     `my/dired-find-file-other-window-or-frame'
;;     
;; * Invoke open on file (for Mac OS X):
;;     `my/dired-mac-open'
;;     
;; * Change listing switches without needing prefix arg
;;     `my/dired-change-listing-switches'
;;

(defun my/dired-next-subdir (arg &optional no-skip)
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

(defun my/dired-prev-subdir (arg &optional no-skip)
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
  (my/dired-next-subdir (- arg) no-skip))

(defun my/dired-move-to-file-at-index (&optional fileindex)
  "Without numeric prefix arg FILEINDEX, move to first file in
this subdir. Otherwise, move to FILEINDEX'th file in this subdir.
If FILEINDEX is negative, index from the last file. (That is, -1
is the last file; -2 the second to last, and so forth.) No
attempt is made to ensure that fileindex is in range."
  (interactive "p")
  (cond
   ((> fileindex 0)
    ;; move to first file
    (my/dired-prev-subdir 0)
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
    (my/dired-prev-subdir 0))))

(defun my/dired-move-to-last-file (&optional fileindex)
  "Without numeric prefix arg FILEINDEX, move to last file in
this subdir. Otherwise, move to FILEINDEX'th from last file in this subdir.
If FILEINDEX is negative, index from the first file. (That is, -1
is the first file; -2 the second, and so forth.) No
attempt is made to ensure that fileindex is in range."
  (interactive "p")
  (my/dired-move-to-file-at-index (- fileindex)))


(defun my/dired-find-file-read-only ()
  "In dired, visit the file or directory named on this line in read-only state."
  (interactive)
  (let ((file-name (file-name-sans-versions (dired-get-filename) t)))
    (if (file-exists-p file-name)
        (find-file-read-only file-name)
      (if (file-symlink-p file-name)
          (error "File is a symlink to a nonexistent target")
        (error "File no longer exists; type `g' to update Dired buffer")))))

(defun my/dired-find-file (&optional read-only-or-alternate)
  "In Dired, visit the file or directory named on this line.
With a prefix argument (C-u or 4), make the visit read-only,
as with `my/dired-find-file-read-only'. With two prefix
arguments (C-u C-u or 16), visit the file or directory instead of
the dired buffer, as with `dired-find-alternate-file'."
  (interactive "p")
  (case read-only-or-alternate
    (1  (call-interactively #'dired-find-file))
    (4  (call-interactively #'my/dired-find-file-read-only))
    (16 (call-interactively #'dired-find-alternate-file))
    (t  (call-interactively #'dired-find-file))))

(defun my/dired-mac-open (&optional arg)
  "In dired, run open command on given file or on each file in list"
  (interactive "P")
  (dired-do-shell-command
   "open" arg (dired-get-marked-files t arg)))

(defun my/dired-find-file-other-window-or-frame (&optional arg)
  "In Dired, visit this file or directory in another window,
when arg is nil, or in another frame otherwise."
  (interactive "P")
  (call-interactively (if arg 
                          #'diredp-find-file-other-frame
                        #'dired-find-file-other-window)))

(defun my/dired-mark-files-path-regexp (regexp &optional marker-char)
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

(defun my/dired-change-listing-switches ()
  "In dired, read and set the listing switches. A shortcut
for dired-sort-toggle-or-edit with a prefix arg."
  (interactive)
  (dired-sort-toggle-or-edit 1))

(defun my/dired-create-directory-or-file (&optional file?)
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
        (-if-let (buf (find-file-noselect expanded)) ; create file
            (with-current-buffer buf
              (set-buffer-modified-p t)
              (save-buffer)))
        (dired-add-file expanded)
        (dired-move-to-filename)))))

(defun my/dired-create-file-or-directory (&optional directory?)
  "Create a new file or directory in the directory containing
point. Without prefix arg, create a file, if the filename does
not exist; with prefix arg create a directory. The dired buffer
is updated with the new directory or file and point is moved to
the corresponding filename. This is based on the command
`my/dired-create-directory-or-file', with the opposite argument
meaning."
  (interactive "P")
  (if (null directory?)
      (my/dired-create-directory-or-file t)
    (my/dired-create-directory-or-file nil)))

(defun my/dired-find-file-or-subdir (&optional use-new-buf-or-switches-for-dirs?)
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
  (let ((dir  (-if-let (d (dired-get-filename nil t)) (file-name-as-directory d)))
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

(defun my/dired-parent-directory-at-point (&optional append-separator?)
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
           (my/dired-parent-directory-at-point t)))
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


;; ATTN: DES when not supplied uses the path instead; seemingly an error.
(defun my/dired-rename-file-from-elsewhere (src des)
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
  (my/dired-bring-file-elsewhere-to-here src des 'move (called-interactively-p 'any)))

(defun my/dired-copy-file-from-elsewhere (src des)
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
  (my/dired-bring-file-elsewhere-to-here src des 'copy (called-interactively-p 'any)))

(defun my/dired-bring-file-elsewhere-to-here (srcname desname how &optional checked)
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

(defun my/dired-keybindings ()
  "Mnemonic and efficient keybindings for dired, consistent with my theme."
  (interactive)
  (define-key dired-mode-map [?\M-C]   'diredp-capitalize-this-file)
  (define-key dired-mode-map "c"       'dired-do-copy)
  (define-key dired-mode-map "e"       'my/dired-find-file)
  (define-key dired-mode-map "f"       'my/dired-find-file)
  (define-key dired-mode-map "r"       'dired-do-rename)
  (define-key dired-mode-map "w"       'my/dired-move-to-file-at-index)
  (define-key dired-mode-map "C"       'my/dired-copy-file-from-elsewhere)
  (define-key dired-mode-map "F"       'find-dired)
  (define-key dired-mode-map "G"       'find-grep-dired)
  (define-key dired-mode-map "M"       'dired-do-rename)
  (define-key dired-mode-map "N"       'find-name-dired)
  (define-key dired-mode-map "R"       'my/dired-rename-file-from-elsewhere)
  (define-key dired-mode-map "V"       'my/dired-move-to-last-file) ; should be "v" but that's life
  (define-key dired-mode-map "W"       'browse-url-of-dired-file)
  (define-key dired-mode-map "%p"      'my/dired-mark-files-path-regexp) ; consider %m instead
  (define-key dired-mode-map "%F"      'find-dired)
  (define-key dired-mode-map "%G"      'find-grep-dired)
  (define-key dired-mode-map "%m"      'dired-mark-files-regexp)
  (define-key dired-mode-map "%M"      'dired-do-rename-regexp)
  (define-key dired-mode-map "%N"      'find-name-dired)
  (define-key dired-mode-map "/P"      'dired-filter-pop-all)
  (define-key dired-mode-map "//"      'dired-filter-by-directory)
  (define-key dired-mode-map "\M-c"    'dired-copy-filename-as-kill)
  (define-key dired-mode-map "\M-d"    'dired-tree-down)
  (define-key dired-mode-map "\A-g"    'diredp-do-grep) ; save M-g for go map
  (define-key dired-mode-map "\M-i"    'dired-maybe-insert-subdir)
  (define-key dired-mode-map "\M-n"    'my/dired-next-subdir)
  (define-key dired-mode-map "\M-o"    'dired-display-file)
  (define-key dired-mode-map "\M-p"    'my/dired-prev-subdir)
  (define-key dired-mode-map "\M-u"    'dired-tree-up)
  (define-key dired-mode-map "\C-d"    'mark-word)
  (define-key dired-mode-map "\C-m"    'my/dired-find-file-or-subdir)
  (define-key dired-mode-map "\C-o"    'my/dired-find-file-other-window-or-frame)
  (define-key dired-mode-map "\C-k"    'dired-do-kill-lines)
  (define-key dired-mode-map "\M-k"    'dired-kill-subdir)
  (define-key dired-mode-map "\C-\M-g" 'dired-do-chgrp)
  (define-key dired-mode-map "\C-\M-k" 'dired-kill-tree)
  (define-key dired-mode-map "\C-\M-l" 'diredp-downcase-this-file)
  (define-key dired-mode-map "\C-\M-m" 'my/dired-move-file-from-elsewhere)
  (define-key dired-mode-map "\C-\M-n" 'my/dired-create-directory-or-file)
  (define-key dired-mode-map "\C-\M-o" 'my/dired-mac-open)
  (define-key dired-mode-map "\C-\M-p" 'diredp-print-this-file)
  (define-key dired-mode-map "\C-\M-u" 'diredp-upcase-this-file)
  (define-key dired-mode-map "\M-ss"   'dired-do-isearch)
  (define-key dired-mode-map "\M-sr"   'dired-do-isearch-regexp)
  (define-key dired-mode-map "\M-sf"   'dired-isearch-filenames)
  (define-key dired-mode-map "\M-sx"   'dired-isearch-filenames-regexp)
  (define-key dired-mode-map [?\C-\M--] 'my/dired-change-listing-switches)
  (define-key dired-mode-map "\C-c\C-f" 'find-file-in-project))

(defun my/diredp-default-faces ()
  "Dired-Plus faces for standard Emacs theme."
  (interactive)
  (set-face-background 'diredp-no-priv     "white")
  (set-face-background 'diredp-write-priv  "tan")
  (set-face-background 'diredp-dir-heading "bisque1")
  (set-face-background 'diredp-dir-priv    "white")
  (set-face-foreground 'diredp-dir-priv    "#FF3445")
  (set-face-foreground 'diredp-file-name   "#3D34FF")
  (set-face-foreground 'diredp-file-suffix "#CF34FF"))

(defun my/diredp-zenburn-faces ()
  "Dired-Plus faces for my zenburn Emacs theme"
  (interactive)
  (set-face-foreground 'diredp-dir-priv    "#33cc33") ; was "magenta3"
  (set-face-background 'diredp-dir-priv    nil)
  (set-face-foreground 'diredp-file-suffix "cornflower blue")
  (set-face-foreground 'diredp-file-name   "#E0CF9F")
  (set-face-foreground 'diredp-number      "gray60")
  (set-face-foreground 'diredp-dir-heading "Blue")
  (set-face-background 'diredp-dir-heading "bisque1")
  (set-face-background 'diredp-no-priv     "black")
  (set-face-foreground 'diredp-date-time   "#74749A9AF7F7"))

(setq-default dired-listing-switches "-lFoh")

(add-my-hook dired-load-hook
  (setq dired-ls-F-marks-symlinks t) ;; Says ls -F marks symlinks with @
  (setq dired-backup-overwrite 'ask)
  (defalias 'dired-advertised-find-file-read-only 'dired-find-file-read-only)
  (my/dired-keybindings)
  (when (featurep 'dired+)
    (if (eq my/theme-func 'my/zenburn)
        (my/diredp-zenburn-faces)
      (my/diredp-default-faces))))

(add-my-hook dired-mode-hook
  (setq dired-recursive-copies  'top)
  (setq dired-recursive-deletes 'ask))

(when (featurep 'dired)     ; dired already loaded!
    (my/dired-load-hook)) ; so do the setup now...

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
       (list "\\.r\\'"     "R --no-save --no-readline --slave --file=?")))

