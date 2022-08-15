;; jsx-fixes: improvements for jsx handling in rjsx-mode

(require 'sgml-mode)
(require 'rjsx-mode)

(defun rjsx-kill-tag (arg)
  "Delete tag on or after cursor and the matching closing or opening tag.
Child nodes are not affected.
With prefix argument ARG, repeat this ARG times."
  (interactive "p")
  (with-syntax-table sgml-tag-syntax-table
    (sgml-delete-tag arg)))

(bind-key "C-c C-h" #'js2-mode-toggle-hide-functions  'rjsx-mode-map)
(bind-key "C-c C-f" #'sgml-skip-tag-forward           'rjsx-mode-map)
(bind-key "C-c C-b" #'sgml-skip-tag-backward          'rjsx-mode-map)
(bind-key "C-c C-d" #'rjsx-kill-tag                   'rjsx-mode-map)



(defun rjsx-jsx-indent ()
  (interactive)
  (let ((indent-region-function #'indent-region-line-by-line))
    (call-interactively #'indent-region)))

;; This is a temporary binding for now
(bind-key "C-c <tab>" #'rjsx-jsx-indent                 'rjsx-mode-map)

;; Next we want to adapt `sgml-electric-tag-pair-mode`, a minor mode
;; for editing sgml style keys to work in rjsx mode. It seems to right
;; now, but we have to check if it is general.
;; One key approach is to make the before-change function used test
;; if we are in a jsx segment.  (This could slow things down too much, but
;; is worth experimenting with.)
;;
;; For now, just use it with M-x sgml-electric-tag-pair-mode and see.
;; Later, can make an rjsx-electric-tag-pair-mode if needed or put it
;; in the rjsx-mode-hook

(add-hook 'rjsx-mode-hook #'sgml-electric-tag-pair-mode)



;; This is a copy from sgml-mode for study. Ultimately want to reindent the
;; jsx after the tags are killed in `rjsx-kill-tag`.
;; JSX indentation is not done in `lsp-format-region` which is the indent-region-function
;; in lsp-mode.
;;
;; To do this, we can mark the region deleted (on the last one) or just do the
;; reindentation etc.  Alternatively, can mark the beginning and end with save-excursions
;; within rjsx-kill-tag

(defun sgml-delete-tag (arg)
  ;; FIXME: Should be called sgml-kill-tag or should not touch the kill-ring.
  "Delete tag on or after cursor, and matching closing or opening tag.
With prefix argument ARG, repeat this ARG times."
  (interactive "p")
  (while (>= arg 1)
    (save-excursion
      (let* (close open)
	(if (looking-at "[ \t\n]*<")
	    ;; just before tag
	    (if (eq (char-after (match-end 0)) ?/)
		;; closing tag
		(progn
		  (setq close (point))
		  (goto-char (match-end 0))))
	  ;; on tag?
	  (or (save-excursion (setq close (sgml-beginning-of-tag)
				    close (and (stringp close)
					       (eq (aref close 0) ?/)
					       (point))))
	      ;; not on closing tag
	      (let ((point (point)))
		(sgml-skip-tag-backward 1)
		(if (or (not (eq (following-char) ?<))
			(save-excursion
			  (forward-list 1)
			  (<= (point) point)))
		    (error "Not on or before tag")))))
	(if close
	    (progn
	      (sgml-skip-tag-backward 1)
	      (setq open (point))
	      (goto-char close)
	      (kill-sexp 1))
	  (setq open (point))
	  (when (and (sgml-skip-tag-forward 1)
		     (not (sgml-looking-back-at "/>")))
	    (kill-sexp -1)))
	;; Delete any resulting empty line.  If we didn't kill-sexp,
	;; this *should* do nothing, because we're right after the tag.
	(if (progn (forward-line 0) (looking-at "\\(?:[ \t]*$\\)\n?"))
	    (delete-region (match-beginning 0) (match-end 0)))
	(goto-char open)
	(kill-sexp 1)
	(if (progn (forward-line 0) (looking-at "\\(?:[ \t]*$\\)\n?"))
	    (delete-region (match-beginning 0) (match-end 0)))))
    (setq arg (1- arg))))
