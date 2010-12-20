;; (@* "Tools")
;;
;; Useful tools for both interactive and programmatic use.
;;
;; Functions defined here:
;; `new-buffer', `unfill-paragraph', `eval-and-replace-sexp',
;; `view-url-in-buffer', and `my-get-makefile-targets'.
;; 

(defun new-buffer (&optional noselect)
  "Create a new buffer named *Untitled*<n> for the n that makes it unique.
If NOSELECT is nil, switch to that buffer; otherwise just display it."
  (interactive "P")
  (let ((buf (get-buffer-create (generate-new-buffer-name "*Untitled*"))))
        (if (null noselect)
            (switch-to-buffer buf)
          (display-buffer buf))))

;(defun unfill-paragraph ()
;  "Convert a paragraph a single line of text."
;  (interactive)
;  (let ((fill-column (point-max)))
;    (fill-paragraph nil)))

(defun with-infinite-fill (func)
  "Set effectively infinite fill column during function FUNC.
FUNC should be a symbol with non-void, interactive function
definition. Inspired by `ourcomments-util' and Sean Burke."
  (let ((fill-column (1+ (point-max))))
    (call-interactively func)))

(defun unfill-paragraph ()
  "Convert a paragraph a single line of text."
  (interactive)
  (with-infinite-fill 'fill-paragraph))

(defun unfill-region ()
  "Convert all paragraphs in the region into
single lines of text."
  (interactive)
  (with-infinite-fill 'fill-region))

(defun unfill-individual-paragraphs ()
  "Convert each paragraph in the region into
single lines of text, respecting each individual
paragraph's indentation and (apparent) fill prefix."
  (interactive)
  (with-infinite-fill 'fill-individual-paragraphs))

(defun eval-and-replace-sexp ()
  "Replace the preceding emac-lisp sexp in current buffer
   with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(defun view-url-in-buffer ()
  "Open a new buffer containing the contents of URL. If thingatpt+
   is loaded, tries to find URL nearest point as default; otherwise,
   no default is given."
  (interactive)
  (let* ((default (if (featurep 'thingatpt+) (thing-nearest-point 'url) "")) 
         (url (read-from-minibuffer "URL: " default))
         (urlbuf (url-retrieve-synchronously url)))
    (when urlbuf
      (switch-to-buffer urlbuf)
      (rename-buffer url t)
      (goto-char (point-min))
      ;; For an http request, header is included in the buffer,
      ;; so remove up to and including first blank line
      (when (and (looking-at "HTTP")
                 (re-search-forward "^\\s-*$" nil t))
        (condition-case nil (forward-char 1) (error nil)) ; move past blank line
        (delete-region (point-min) (point)))
      (setq case-fold-search t) ; buffer local
      ;; This should use nxml-mode or nxhtml-mode automatically
      (cond ((looking-at "<?xml") (xml-mode))
            ((looking-at "<\\(!DOCTYPE\\s-+\\)html") (html-mode))))))

(defun my-get-makefile-targets ()
  "Scan the contents of all files makefiles in the current
directory and return a list of targets.  The files scanned match
the glob [mM]akefile* within .  All targets are returned except
those containing $ (for variable substitution) or % for pattern
matching."
  (with-temp-buffer
    (let ((makefiles (nreverse (file-expand-wildcards "[mM]akefile*")))
          targets)
      (dolist (file makefiles)
        (insert-file-contents (concat default-directory file)))
      (goto-char (point-min))
      ; Simply exclude $ and %. Can add more generality later if needed.
      (while (re-search-forward "^\\s-*\\([^$%: 	\n]+?\\)\\s-*:" nil t)
        (setq targets (cons (match-string 1) targets)))
      (nreverse (cons "" targets)))))

