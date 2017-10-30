;;; macros.el --- init-file macros -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2017 Christopher R. Genovese, all rights reserved.
;; Author: Christopher Genovese <genovese@cmu.edu>
;; Version: 1.2.0


;;; Code:

(require 'cl-lib)
(require 'ht)

(defun macro-body-with-kw-options (macro-body &optional stop?)
  "Decomposes macro rest arg into docstring, option settings and body. 
Returns a list of three values (options-map docstring true-body).
This is intended for use in macro definitions. Argument
MACRO-BODY is the list of forms given to the macro. It can
consist of an optional docstring followed by zero or more
keyword-value pairs and then a sequence of forms. If supplied,
STOP? should be a predicate that returns a truthy value if
option-processing should cease. All initial keyword-value pairs
up until STOP? returns true are removed in the true body that
results. "
  (let* ((docstring (if (stringp (car macro-body)) (car macro-body) nil))
         (macro-body (if docstring (cdr macro-body) macro-body))
         (opt-body   (cl-loop with options = (ht)
                              for body = macro-body then (cddr body)
                              for key = (car body)
                              for val = (cadr body)
                              do
                              (when (or (not (keywordp key)) (not body)
                                        (and stop? (funcall stop? key)))
                                (cl-return (list options body)))
                              (ht-set! options key val))))
    (list (car opt-body) (or docstring "") (cadr opt-body))))

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

;;; Just use the dash.el versions to keep compatibility simpler (e.g., cider)
;;(defmacro if-let (binding then &rest else)
;;  "Like `if' but if test is true, evaluates then with binding-symbol bound 
;;to the value of test; if not, yields else"
;;  (declare (indent 1))
;;  (cl-destructuring-bind (var test)
;;      binding
;;    (let ((test-val (cl-gensym)))
;;      `(let ((,test-val ,test))
;;         (if ,test-val
;;             (let ((,var ,test-val)) ,then)
;;           ,@else)))))
;; 
;;(defmacro when-let (binding &rest forms)
;;  "Like `when', but if test is true, evaluates then with binding-symbol bound 
;;to the value of test."
;;  (declare (indent 1))
;;  (cl-destructuring-bind (var test)
;;      binding
;;    (let ((test-val (cl-gensym)))
;;      `(let ((,test-val ,test))
;;         (when ,test-val
;;             (let ((,var ,test-val)) ,@forms))))))

(defmacro as-> (expr name &rest forms)
  "Binds name to expr, evaluates the first form given that binding,
then binds name to that result, and so on for each successive
form. However, in the special case where a form is a single
symbol S, it is replaced by (S NAME), as it would with `->'. 
If a constant value is actually desired, use (identity VAL) for
the corresponding form."
  ;; This is based on clojure's as-> macro, but it adds
  ;; a `->'-style behavior for convenience in a common case.
  ;; To get the clojure standard behavior replace the ,@ entry below with
  ;;     (-partition 2 (cons name (-interpose name forms)))
  (declare (indent 2))
  `(let* ((,name ,expr)
          ,@(->> forms
              (-map (lambda (x) (if (symbolp x) (list x name) x)))
              (-interpose name)
              (cons name)
              (-partition 2)))
     ,name))

(defmacro add-my-hook (hook &rest body-with-options)
  "Creates a custom hook function and adds it to an existing hook.
HOOK is a symbol representing a standard hook variable.
BODY-WITH-OPTIONS consists of an optional docstring, an optional
sequence of key-value pairs representing options `:append' and
`:local' corresponding to the append and local arguments to
`add-hook', and a sequence of forms representing the body of the
hook. The first function created for a HOOK named <name> is named
my/<name>; subsequent calls with the same value of HOOK append a
unique number via `cl-gensym'."
  (declare (indent 1))
  (let* ((my-name (concat "my/" (symbol-name hook)))
         (my-base (cl-gensym "my-base-sym"))
         (my-hook (cl-gensym "my-hook-sym")))
    (cl-destructuring-bind (options docstring body)
        (macro-body-with-kw-options body-with-options)
      `(let* ((,my-base (intern ,my-name))
              (,my-hook (if (and (boundp ',hook)
                                 (memq ,my-base ,hook))
                            (intern (symbol-name (cl-gensym ,my-name)))
                          ,my-base)))
         (defalias ,my-hook
           (function (lambda  ()
                       ,docstring
                       ,@body)))
         (add-hook ',hook ,my-hook
                   ,(ht-get options :append nil)
                   ,(ht-get options :local nil))))))

(defmacro my/table-set (table &rest kv-pairs)
  "Add one or more key-value pairs to a hash-table"
  (declare (indent 1))
  (let ((x (cl-gensym)))
    `(cl-loop for ,x = (list ,@kv-pairs) then (cddr ,x)
              while ,x do
              (ht-set! ,table (car ,x) (cadr ,x) ))))


;;; macros.el ends here
