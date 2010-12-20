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

