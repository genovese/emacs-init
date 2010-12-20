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

