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

