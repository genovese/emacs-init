(defmacro with-ess-syntax (&rest body)
  "Executes BODY with the ess syntax parsing variables set up, saving state."
  `(save-mark-and-excursion
     (with-ess-syntax*
      ,@body)))

(defmacro with-ess-syntax* (&rest body)
  "Executes BODY with the ess syntax parsing variables set up."
  `(let* ((indent-point (point))
          (state (syntax-ppss))
          (containing-sexp (cadr state))
          (prev-containing-sexp (car (last (butlast (nth 9 state))))))
     ,@body))


;; Replacement of existing ESS function that fixes a bug
;; that causes the end function (this with arg -1) to go
;; too far. See https://github.com/emacs-ess/ESS/issues/822.
(defun ess-goto-beginning-of-function-or-para (&optional arg)
  "If inside a function go to the beginning of it.
Otherwise go to the beginning of paragraph. If ARG is negative,
go to the end of function or paragraph."
  (interactive)
  (let ((beg-point (point)))
    (setq arg (or arg 1))
    (or (condition-case nil (if (> arg 0)
                                (beginning-of-defun)
                              (end-of-defun)
                              (> (point) beg-point))
          ;; end-of-defun can error in R mode
          (error (forward-paragraph)))
        (if (> arg 0)
            (backward-paragraph)
          (forward-paragraph)))
    (when (eql beg-point (point))
      ;; Ensure we move
      (if (> arg 0)
          (backward-paragraph)
        (forward-paragraph))))
  (point))
