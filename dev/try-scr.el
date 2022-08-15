(defmacro comment (&rest body)
  "Ignore all forms in BODY."
  (declare (indent 0))
  `,@(list))

(setq ivy-sort-matches-functions-alist '((t)
 (ivy-completion-in-region . ivy--shorter-matches-first)))
(setq cider-preferred-build-tool 'shadow-cljs)
(global-set-key (kbd "C-M-;") #'origami-prefix)
(add-to-list 'origami-parser-alist '(clojurescript-mode . origami-clj-parser))

;;(define-key cider-repl-mode-map (kbd "C-c C-d C-p") #'cider-start-shadow-node-client)
;; (let ((name "test-rpc")
;;       (buff (get-buffer-create "elpy-rpc-test"))
;;       (full-python-command (executable-find elpy-rpc-python-command)))
;;   (start-process name
;;                  buff
;;                  full-python-command
;;                  "-W" "ignore"
;;                  "-m" "elpy.__main__"))

;; (defun org-comment-line-break-function (&optional soft)
;;   "Break line at point and indent, continuing comment if within one.
;; The inserted newline is marked hard if variable
;; `use-hard-newlines' is true, unless optional argument SOFT is
;; non-nil."
;;   (if soft (insert-and-inherit ?\n) (newline 1))
;;   (save-excursion (forward-char -1) (delete-horizontal-space))
;;   (delete-horizontal-space)
;;   (indent-to-left-margin)
;;   (insert-before-markers-and-inherit fill-prefix))

(defun org-comment-line-break-function (&optional soft)
  "Break line at point and indent, continuing comment if within one.
The inserted newline is marked hard if variable
`use-hard-newlines' is true, unless optional argument SOFT is
non-nil."
  (if soft (insert-and-inherit ?\n) (newline 1))
  (save-excursion (forward-char -1) (delete-horizontal-space))
  (delete-horizontal-space)
  (indent-to-left-margin)
  (if fill-prefix
      (insert-before-markers-and-inherit fill-prefix)
    (insert-before-markers-and-inherit)))

(defun org-babel-demarcate-block (&optional arg)
  "Wrap or split the code in the region or on the point.
When called from inside of a code block the current block is
split.  When called from outside of a code block a new code block
is created.  In both cases if the region is demarcated and if the
region is not active then the point is demarcated."
  (interactive "P")
  (let* ((info (org-babel-get-src-block-info 'light))
	 (start (org-babel-where-is-src-block-head))
	 (block (and start (match-string 0)))
	 (headers (and start (match-string 4)))
	 (stars (concat (make-string (or (org-current-level) 1) ?*) " "))
	 (upper-case-p (and block
			    (let (case-fold-search)
			      (string-match-p "#\\+BEGIN_SRC" block)))))
    (if info
        (mapc
         (lambda (place)
           (save-excursion
             (goto-char place)
             (let ((lang (nth 0 info))
                   (indent (make-string (current-indentation) ?\s)))
	       (when (string-match "^[[:space:]]*$"
				   (buffer-substring (point-at-bol)
						     (point-at-eol)))
		 (delete-region (point-at-bol) (point-at-eol)))
               (insert (concat
			(if (looking-at "^") "" "\n")
			indent (if upper-case-p "#+END_SRC\n" "#+end_src\n")
			(if arg stars indent) "\n"
			indent (if upper-case-p "#+BEGIN_SRC " "#+begin_src ")
			lang
			(if (> (length headers) 1)
			    (concat " " headers) headers)
			(if (looking-at "[\n\r]")
			    ""
			  (concat "\n" (make-string (current-column) ? )))))))
	   (move-end-of-line 2))
         (sort (if (org-region-active-p) (list (mark) (point)) (list (point))) #'>))
      (let ((start (point))
	    (lang (completing-read
		   "Lang: "
		   (mapcar #'symbol-name
			   (delete-dups
			    (append (mapcar #'car org-babel-load-languages)
				    (mapcar (lambda (el) (intern (car el)))
					    org-src-lang-modes))))))
	    (body (delete-and-extract-region
		   (if (org-region-active-p) (mark) (point)) (point)))
            (new-start (progn
                         (forward-line 0)
                         (unless (looking-at "^[:blank:]*$")
                           (end-of-line)
                           (insert "\n"))
                         (point))))
	(insert (concat (if arg (concat stars "\n") "")
			(if upper-case-p "#+BEGIN_SRC " "#+begin_src ")
			lang "\n" body
			(cond
                         ((= (length body) 0)
                          "\n")  ; (concat (make-string (+ (length stars) 2) ? ) "\n")
                         ((or (string-suffix-p "\n" body)
			      (string-suffix-p "\r" body))
			  "")
			 (t
                          "\n"))
			(if upper-case-p "#+END_SRC\n" "#+end_src\n")))
	(let ((end (point)))
          (goto-char new-start)
          (save-excursion
            (while (< (point) end)
             (org-indent-line)
             (forward-line 1))))
        (forward-line 1)
        (skip-chars-forward " \t")))))


(defhydra origami-hydra (:hint nil)
  "
           ^Move^        ^Open/Close^          ^Other^
      ---------------------------------------------------------------
      [_C-n_] Next Line  [_o_]   Open          [_f_] Forward Fold
      [_C-p_] Prev Line  [_O_]   Open All      [_b_] Backward Fold
      [_C-w_] Scroll Dn  [_C-o_] Open recurs   [_n_] Next Fold
      [_C-v_] Scroll Up  [_c_]   Close         [_p_] Prev Fold
      [_M-w_] Beg Buff   [_C_]   Close All     [_F_] F same level
      [_M-v_] End Buf    [_C-c_] Close recurs  [_,_] F toggle
      [_l_]   Repos      [_s_]   Show Open     [_t_] Toggle
      [_L_]   Recenter   [_SPC_] Toggle recurs [_q_] Quit
      ...                Arrows move f/b and toggle nodes
     "
  ("c" origami-close-node)
  ("C" origami-close-all-nodes)
  ("C-c" origami-close-node-recursively)
  ("o" origami-open-node)
  ("O" origami-open-all-nodes)
  ("C-o" origami-open-node-recursively)
  ("t" origami-toggle-node)
  ("SPC" origami-recursively-toggle-node)
  ("s" origami-show-only-node)
  ("f" origami-forward-fold)
  ("F" origami-forward-fold-same-level)
  ("," origami-forward-toggle-node)
  ("b" origami-backward-fold-same-level)
  ("n" origami-next-fold)
  ("p" origami-previous-fold)
  ("<left>" origami-backward-fold-same-level)
  ("<right>" origami-forward-fold-same-level)
  ("<up>" origami-toggle-node)
  ("<down>" origami-recursively-toggle-node)
  ("C-n" next-line)
  ("C-p" previous-line)
  ("C-a" my/move-beginning-of-line)
  ("C-e" move-end-of-line)
  ("C-f" forward-char)
  ("C-b" backward-char)
  ("M-f" forward-word)
  ("M-b" backward-word)
  ("M-C-f" forward-sexp-or-char)
  ("M-C-b" backward-sexp-or-char)
  ("M-w" beginning-of-buffer)
  ("M-v" end-of-buffer)
  ("C-w" cua-scroll-down)
  ("C-v" cua-scroll-up)
  ("l" reposition-window)
  ("L" recenter-top-bottom)
  ("q" nil))

(progn
  (define-prefix-command 'my/origami-prefix 
    "Origami mode keys")
  (define-key my/origami-prefix "c" 'origami-close-node)
  (define-key my/origami-prefix "C" 'origami-close-all-nodes)
  (define-key my/origami-prefix "o" 'origami-open-node)
  (define-key my/origami-prefix "O" 'origami-open-all-nodes)
  (define-key my/origami-prefix "t" 'origami-toggle-node)
  (define-key my/origami-prefix " " 'origami-recursively-toggle-node)
  (define-key my/origami-prefix "f" 'origami-forward-fold)
  (define-key my/origami-prefix "F" 'origami-forward-toggle-node)
  (define-key my/origami-prefix "b" 'origami-backward-fold-same-level)
  (define-key my/origami-prefix "n" 'origami-next-fold)
  (define-key my/origami-prefix "p" 'origami-previous-fold)
  (define-key my/origami-prefix ";" 'origami-hydra/body)
  (define-key my/origami-prefix "RET" 'origami-hydra/body)
  (global-set-key (kbd "C-M-;") 'my/origami-prefix))


;; bound to M-g M-o  and C-M-; RET  and C-M-; M-o
;; Others put on C-M-; with these keys
(defhydra origami-hydra (:hint nil)
  "
           ^Move  ^      ^Open/Close^          ^Other^
      ---------------------------------------------------------------
      [_C-n_] Next Line  [_o_]   Open          [_f_] Forward Fold
      [_C-p_] Prev Line  [_O_]   Open All      [_b_] Backward Fold
      [_C-w_] Scroll Dn  [_c_]   Close         [_n_] Next Fold
      [_C-v_] Scroll Up  [_C_]   Close All     [_p_] Prev Fold
      [_M-w_] Beg Buff   [_s_]   Show Open     [_F_] F same level
      [_M-v_] End Buf    [_C-o_] Open recurs   [_,_] F toggle
      [_l_]   Repos      [_C-c_] Close recurs  [_t_] Toggle
      [_L_]   Recenter   [_ _]   Toggle recurs [_q_] Quit
     "
  ("c" origami-close-node)
  ("C" origami-close-all-nodes)
  ("o" origami-open-node)
  ("O" origami-open-all-nodes)
  ("t" origami-toggle-node)
  (" " origami-recursively-toggle-node)
  ("f" origami-forward-fold)
  ("F" origami-forward-toggle-node)
  ("b" origami-backward-fold-same-level)
  ("n" origami-next-fold)
  ("p" origami-previous-fold)
  ("q" nil))

