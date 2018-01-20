;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Org Mode

;(setq org-todo-keywords
;      '((sequence "TODO(t)" "WAIT(w@)" "DONE(d)")
;        (sequence "READING" "REVIEWING" "RESPONDED")
;        (sequence "PREPARED" "DISPATCHED" "PROCESSED" "FINISHED")
;        (sequence "FOUND" "TESTING" "FIXING" "FIXED")))

(defun org-table-mark-field (&optional prefix)
  (interactive "P")
  (org-table-check-inside-data-field)
  (unless (or prefix (looking-back "| ?" (point-at-bol 0)))
    (org-table-beginning-of-field 0))
  (push-mark
   (save-excursion
     (let ((org-table-automatic-realign nil))
       (org-table-end-of-field 1))
     (point)))
  (activate-mark))

(defun my-org-table-extensions ()
  (local-set-key [(super ?\ )] 'org-table-mark-field))

(add-hook 'org-mode-hook 'my-org-table-extensions)
(setq org-ditaa-jar-path "~/bin/ditaa.jar")
