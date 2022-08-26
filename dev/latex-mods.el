(add-hook 'LaTeX-mode-hook
          (lambda ()
            (set (make-variable-buffer-local 'TeX-electric-math)
                 (cons "\\(" "\\)"))))

(defun LaTeX-insert-dollar (&optional arg)
  "Insert inline equation, upgrade to display math, or exit inline math.

ATTN:DOC  $->\(_\), $$ -> \[_\], $xx$ -> \(xx\)_

If current math mode was not entered with a dollar, refuse to
insert one.  Show matching dollar sign if this dollar sign ends
the TeX math mode and `blink-matching-paren' is non-nil.

When outside math mode, the behavior is controlled by the variable
`TeX-electric-math'.

With raw \\[universal-argument] prefix, insert exactly one dollar
sign.  With optional ARG, insert that many dollar signs."
  (interactive "P")
  (cond
   ((and arg (listp arg))
    ;; C-u always inserts one
    (insert "$"))
   (arg
    ;; Numerical arg inserts that many
    (insert (make-string (prefix-numeric-value arg) ?\$)))
   ((or (TeX-escaped-p) (TeX-verbatim-p))
    ;; Point is escaped with `\' or is in a verbatim-like construct, so just
    ;; insert one $.
    (insert "$"))
   ((texmathp)
    ;; We are inside math mode
    (cond
     ((and TeX-electric-math
           (looking-back "\\\\(")
           (looking-at "\\\\)"))
      ;; Point is between an empty "\(\)" and `TeX-electric-math' is non-nil;
      ;; upgrade to \[ \]
      (delete-char -1)
      (insert "[")
      (delete-char 2)
      (insert "\\]")
      (backward-char 2))
     ((and (stringp (car texmathp-why))
           (string-equal (car texmathp-why) "\\("))
      (let ((end-pos (save-excursion (search-forward "\\)" nil t))))
        (when end-pos (goto-char end-pos))))
     (t
      (message "Math mode started with `%s', no action taken."
               (car texmathp-why)))))
   (t
    ;; Just somewhere in the text.
    (cond
     ((and TeX-electric-math (TeX-active-mark))
      (if (> (point) (mark))
          (exchange-point-and-mark))
      (cond
       ;; $...$ to $$...$$
       ((and (eq last-command #'TeX-insert-dollar)
             (re-search-forward "\\=\\$\\([^$][^z-a]*[^$]\\)\\$" (mark) t))
        (replace-match "$$\\1$$")
        (set-mark (match-beginning 0)))
       ;; \(...\) to \[...\]
       ((and (eq last-command #'TeX-insert-dollar)
             (re-search-forward "\\=\\\\(\\([^z-a]*\\)\\\\)" (mark) t))
        (replace-match "\\\\[\\1\\\\]")
        (set-mark (match-beginning 0)))
       ;; Strip \[...\] or $$...$$
       ((and (eq last-command #'TeX-insert-dollar)
             (or (re-search-forward "\\=\\\\\\[\\([^z-a]*\\)\\\\\\]" (mark) t)
                 (re-search-forward "\\=\\$\\$\\([^z-a]*\\)\\$\\$" (mark) t)))
        (replace-match "\\1")
        (set-mark (match-beginning 0)))
       (t
        ;; We use `save-excursion' because point must be situated before opening
        ;; symbol.
        (save-excursion (insert (car TeX-electric-math)))
        (exchange-point-and-mark)
        (insert (cdr TeX-electric-math))))
      ;; Keep the region active.
      (TeX-activate-region))
     (TeX-electric-math
      (insert (car TeX-electric-math))
      (save-excursion (insert (cdr TeX-electric-math)))
      (if blink-matching-paren
          (progn
            (backward-char)
            (sit-for blink-matching-delay)
            (forward-char))))
     ;; In any other case just insert a single $.
     ((insert "$")))))
  (TeX-math-input-method-off))
