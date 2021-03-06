;;; A recasting of open line for global and org-mode used
;;; This reverses the ordinary valence, so by default opens below
;;; instead of above, and it indents appropriately where applicable.
;;; The main advantage of this is that one can start a new line indented
;;; from anywhere in the current line, without having to do C-M-j at the end.
;;; Accepts a prefix arg to switch valence but also maps the mostly unused
;;; M-o to the above valence as well.
;;; 
;;; An alternative to consider is calling comment-line-break-function or
;;; similar when comment syntax is defined, making it easy to continue
;;; comments when comments. Trying this here, but put back original
;;; text if not.
(defun smart-open-line-below ()
  "Insert an empty line below the current line.
Position the cursor at its beginning, according to the current mode."
  (interactive)
  (move-end-of-line nil)
  ;; originally: (newline-and-indent)
  (default-indent-new-line))

(defun smart-open-line-above ()
  "Insert an empty line above the current line.
Position the cursor at it's beginning, according to the current mode."
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))

(defun smart-open-line (&optional arg)
  (interactive "P")
  (if arg (smart-open-line-above) (smart-open-line-below)))

(defun org-smart-open-line (&optional arg)
  "Insert a new row in tables, call `open-line' elsewhere.
If `org-special-ctrl-o' is nil, just call `open-line' everywhere.
As a special case, when a document starts with a table, allow to
call `open-line' on the very first character."
  (interactive "*P")
  (cond
   ((and org-special-ctrl-o (/= (point) 1) (org-at-table-p))
    (org-table-insert-row arg))
   (arg
    (smart-open-line-above))
   (t
    (smart-open-line-below))))

(bind-key "C-o" #'smart-open-line)
(bind-key "M-o" #'smart-open-line-above)
(bind-key "C-o" #'org-smart-open-line 'org-mode-map)


