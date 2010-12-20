;; (@* "File and Directory Names")
;;
;; Operations on file and directory (and other) names
;; 
;; Functions defined here:
;; `expand-directory-name', `directory-name-and-sep', `equal-file-name',
;; `equal-dir-name', `remove-prefix'
;; 

(defun expand-directory-name (dir)
  "Expands a directory name as in expand-file-name but ensures
that the name ends with a '/'.  This can be used to convert
a directory name, even in relative or ~ form, to a canonical
form, for comparison, concatenation, or other purposes."
  (file-name-as-directory (expand-file-name dir)))
; working but probably obsolete>:
;  (save-excursion
;    (let ((dir (and (stringp dir) (expand-file-name dir))))
;      (if (and dir (not (string-match "/$" dir)))
;	  (concat dir "/")
;	dir))) )

(defun directory-name-and-sep (dir)
  "If directory name does not end in '/', append it to DIR
and return the new name; otherwise return DIR itself. No
other expansion is done; but see `expand-directory-name'."
  (or (and dir (not (string-match "/$" dir)) (concat dir "/"))
      dir))

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

