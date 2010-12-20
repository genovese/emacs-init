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

