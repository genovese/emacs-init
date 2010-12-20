;; (@* "Scrolling and Cursor Movement")
;;
;; Improved scrolling and cursor movement functions as alternatives
;; to the standard offerings. One theme here is to avoid errors
;; on boundary conditions by dwim. In addition, context dependent
;; movement (e.g., at the beginning of the line is often useful).
;; For instance, `my-move-beginning-of-line' alternates moving
;; to the true beginning and the first non-whitespace 
;; character (see also `my-comint-bol' later).
;;
;; Functions defined here:
;; `scroll-down-or-beg', `scroll-up-or-end', `forward-sexp-or-char',
;; `backward-sexp-or-char', `my-move-beginning-of-line',
;;

(defun scroll-down-or-beg ()
  "Smart scroll down, if too close to the beginning of the buffer,
it moves you there without error, in contrast to scroll-down."
  (interactive)
  (condition-case nil (scroll-down) (error (goto-char (point-min)))))

(defun scroll-up-or-end ()
  "Smart scroll up, if too close to the end of the buffer,
it moves you there without error, in contrast to scroll-up."
  (interactive)
  (condition-case nil (scroll-up) (error (goto-char (point-max)))))

(defun forward-sexp-or-char (&optional arg)
  "Move forward across one balanced expression (sexp) or forward
one character if none. With ARG, do it that many times.  Negative
arg -N means move backward across N balanced expressions. In
comparison to backward-sexp, this allows you to continue moving
across expressions without having to change keys."
  (interactive "p")
  (or arg (setq arg 1))
  (condition-case nil
      (forward-sexp arg)
    (error
     (let ((at-sexp-boundary? ; do we see a sexp delimiter in proper direction?
            (if (< arg 0)
                (if (char-equal (char-syntax (char-before (point))) ?\() t nil)
              (if (char-equal (char-syntax (char-after (point))) ?\)) t nil))))
       (if at-sexp-boundary?
           (forward-char arg))))))

(defun backward-sexp-or-char (&optional arg)
  "Move backward across one balanced expression (sexp) or
backward one character if at the boundary.  With ARG, do it that
many times.  Negative arg -N means move forward across N balanced
expressions.  In comparison to backward-sexp, this allows you to
continue moving across expressions without having to change
keys."
  (interactive "p")
  (or arg (setq arg 1))
  (forward-sexp-or-char (- arg)))

(defun my-move-beginning-of-line (&optional n)
  "If not at the beginning of a line, move point to the beginning
of the line, moving forward n - 1 lines first if n is not nil or
1. However, if at the beginning of a line, move back to the first
non-ws character on the line."
  (interactive "p")
  (if (looking-at "^")
      (back-to-indentation)
    (move-beginning-of-line n)))

