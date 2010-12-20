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

