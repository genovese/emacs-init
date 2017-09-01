;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; comint-mode 

;; Added Functionality
;;
;; * Beginning of line that alternates between true beginnng
;;   and prompt. Command `my/comint-bol'.
;;
;; * DWIM search of input history or text, using comint
;;   functionality and a prefix arg to determine which.
;;   See `my/comint-dwim-isearch-backward'.
;;   

(defun my/comint-bol ()
  "Goes to the beginning of line, then skips past the prompt, if any.
However, if already at the prompt, the prompt is not skipped.
This has the side-effect of skipping the prompt if you start at the
beginning of a line with a prompt.  This oscillation between the two
possible beginnings is useful.

The prompt skip is done by skipping text matching the regular expression
`comint-prompt-regexp', a buffer local variable."
  (interactive)
  (let ((now (point))
	(eol (save-excursion (end-of-line) (point))))
    ;; (beginning-of-line)  ;; ATTN: this got confused by "field boundaries" in later versions of emacs
    (forward-line 0)
    (if (looking-at comint-prompt-regexp)
	(if (and (not (= (match-end 0) now))
		 (<= (match-end 0) eol))
	    (goto-char (match-end 0))))))

(defun my/comint-dwim-isearch-backward (&optional use-hist-regexp?)
  "Search incrementally backward for a string or regexp, using
either the input history or the buffer text. If point is at
or after the final prompt line (i.e., the process mark),
then search the input history; otherwise, do a regular isearch
backward in the buffer text. When searching history, the prefix
argument (or regular argument noninteractively) determines whether
a string or regexp search is used. Nil prefix arg gives a
regular string search; non-nil gives a regexp search. When
searching the buffer contents, regexp search is always used."
  (interactive)
  (let ((on-prompt-line? (comint-after-pmark-p))
        (comint-history-isearch 'dwim))
    (if (or (not on-prompt-line?) use-hist-regexp?)
        (isearch-backward-regexp)
      (isearch-backward))))

;; Configuration

(add-my-hook comint-mode-hook
  "Generic comint setup, primarily for shell use"
  (setq comint-input-ring-size '1024)
  (setq comint-input-ignoredups t)
  ;(setq comint-process-echoes t)  ;; ATTN: this should be in specific sub-mode not here! Causes ielm problems
  (local-set-key "\C-a"         'my/comint-bol)
  (local-set-key "\M-r"         'my/comint-dwim-isearch-backward) 
  (local-set-key [?\C-c return] 'comint-copy-old-input)
  (local-set-key "\C-c\C-m"     nil)  ;; used globally for extended command
  (add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)
  (ansi-color-for-comint-mode-on))



