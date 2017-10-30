;;; utils.el -- elisp utilities that've proven useful -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2017 Christopher R. Genovese, all rights reserved.
;; Author: Christopher Genovese <genovese@cmu.edu>
;; Version: 1.2.0

;;; Commentary:
;;  These utilities are only for daily use, not for use within distributed
;;  packages. Hence, some of the functions are unprefixed, some replace
;;  existing functions, and some use the `my/' prefix if its seems appropriate.


;;; Code:

(require 'f)

(defalias 'foreach 'dolist)

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


(defun require-soft (feature &optional file)
  "Try to require FEATURE, but don't signal an error if `require' fails."
  (require feature file 'noerror))

(defun require-warn (feature &optional file)
  "Try to require FEATURE, but don't signal an error if `require' fails."
  (unless (require feature file 'noerror)
    (message "Library %s is not available" feature)))


(defun call-if-bound (func &rest args)
  "Call function FUNC passing ARGS if it is bound as a function."
  (when (fboundp func)
    (apply 'funcall func args)))

(defun package-available-p (package &optional min-version)
  "PACKAGE is installed (with at least MIN-VERSION) or corresponding feature is loaded."
  (or (package-installed-p package min-version)
      (and (not min-version) (featurep package))))

(defun expand-directory-name (dir)
  "Expands a directory name as in expand-file-name but ensures
that the name ends with a '/'.  This can be used to convert
a directory name, even in relative or ~ form, to a canonical
form, for comparison, concatenation, or other purposes."
  (file-name-as-directory (expand-file-name dir)))


(defun bfwalk-subtree (dir action &optional filter must-match files-seen)
  "Within directory DIR, do ACTION on subdirectories by breadth first traversal.
DIR itself is *not* processed, and subdirectories containing a
file `.nosearch' are skipped, as are special directories `.' and
`..'. ACTION is a function of no arguments. It is called in a
dynamic environment where `default-directory' is set to the given
directory, and its return valueis ignored. If FILTER is non-nil,
it should be a function of one argument, the relative directory
name, that returns non-nil if a directory should be processed. If
MATCH is non-nil, it is a regex that the non-absolute file names
must match. FILES-SEEN is a list of file specifiers (inodes on
Unix-like systems and canonical file-names on Windows systems)
that have already been processed and for which processing is
skipped."
  (let ((attrs nil)
	(pending (list dir)))
    ;; This loop does a breadth-first tree walk on DIR's subtree,
    ;; putting each subdir into DIRS as its contents are examined.
    (while pending
      (let* ((this-dir (pop pending))
	     (contents (directory-files this-dir nil must-match))
	     (default-directory this-dir)
             (special-dirs '("." ".."))
	     (canonicalized (if (fboundp 'untranslated-canonical-name)
				(untranslated-canonical-name this-dir))))
	;; The Windows version doesn't report meaningful inode numbers, so
	;; use the canonicalized absolute file name of the directory instead.
	(setq attrs (or canonicalized
			(nthcdr 10 (file-attributes this-dir))))
	(unless (member attrs files-seen)
	  (push attrs files-seen)
	  (dolist (subdir contents)
	    (and (if filter (funcall filter subdir) t)
                 (not (member subdir special-dirs))
		 (file-directory-p subdir)
		 (let* ((expanded (expand-file-name subdir))
                        (default-directory expanded))
		   (unless (file-exists-p
                            (expand-file-name ".nosearch" expanded))
                     (setq pending (nconc pending (list expanded)))
                     (funcall action))))))))))

(defun prepend-subtree-to-load-path (dir)
  "Prepend subdirectories of DIR, including DIR itself to load path.
Direct children of DIR are at the beginning, then DIR itself, then
deeper subdirectories"
  (setq load-path
        (append
         (let* ((default-directory dir)
                (load-path (list (directory-file-name
                                  (expand-file-name default-directory))))
                (subdir-loader (expand-file-name "subdirs.el"))
                (new-path (progn
                            (when (file-exists-p subdir-loader)
                              (load-file subdir-loader))
                            (append (cdr load-path) (list (car load-path)))))
                (process
                 (lambda ()
                   (load-file "subdirs.el")))
                (has-subdir?
                 (lambda (d)
                   (and
                    (not (or (member d '("RCS" "CVS" "cvs" "rcs"))
                             (string-match "\\.elc?\\'" d)))
                    (file-exists-p (f-join d "subdirs.el")))))
                (alnum? "\\`[[:alnum:]]"))
           (setq load-path new-path)    
           (bfwalk-subtree dir process has-subdir? alnum?)
           load-path)
         load-path)))

;;; utils.el ends here

