;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                           ;;
;;(@* "Shell-based Modes")                                   ;;;;;;
;;                                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ipython


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; comint-mode 

;; Added Functionality
;;
;; * Beginning of line that alternates between true beginnng
;;   and prompt. Command `my-comint-bol'.
;;
;; * DWIM search of input history or text, using comint
;;   functionality and a prefix arg to determine which.
;;   See `my-comint-dwim-isearch-backward'.
;;   

(defun my-comint-bol ()
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

(defun my-comint-dwim-isearch-backward (&optional use-hist-regexp?)
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

(defun my-comint-mode-hook ()
  (setq comint-input-ring-size '1024)
  (setq comint-input-ignoredups t)
  (local-set-key "\C-a"         'my-comint-bol)
  (local-set-key "\M-r"         'my-comint-dwim-isearch-backward) 
  (local-set-key [?\C-c return] 'comint-copy-old-input)
  (local-set-key "\C-c\C-m"     nil)  ;; used globally for extended command
  (add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)
  )
       
(add-hook 'comint-mode-hook 'my-comint-mode-hook)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; shell-mode

;; Added Functionality (tcsh-style)
;;
;; * tcsh-style directory tracking and stack management
;;
;;   Features:
;;    1. Support =n and =- references to directory stack
;;       (without expansion) as in tcsh
;;    2. Fixed expansion of =n and =- (when instigated)
;;       to prevent missing slash
;;    3. Sets shell-last-dir properly so  cd - works
;;       as in tcsh
;;    4. Fixed bug in shell-resync-dirs in case dir
;;       tracking gets lost
;;    5. Improved handling of aliases for HOME directory.
;;
;;   This is old code -- in fact my first real emacs-lisp code, so it's a
;;   bit clunky. But I've been using it for years without problem, and it
;;   really makes moving around in the shell more pleasant.
;; 
;; * Improved shell history autoexpand
;;
;;   See `shell-toggle-autoexpand'.
;;

(defvar shell-pushd-silent nil
  "*If non-nil, show dirstack in minibuffer with every change
of the directory stack.  This mimicks the optional behavior
of tcsh.")

(defvar shell-backup-input-autoexpand (cons nil 'input)
  "*List of autoexpand states among which the user can toggle with
shell-toggle-autoexpand.")

(defvar shell-home-aliases (if (boundp 'my-home-aliases)
                               my-home-aliases
                             (if (getenv "HOME")
                                 (list (concat (getenv "HOME") "/"))
                               nil))
  "*This is a list of strings indicating possible aliases for the users
HOME directory that emacs might not recognize.  This is most useful with
cross-mounted file systems, and is used by shell-map-home-aliases to 
determine whether or not the directory should be changed to \"~/\".")

(defun use-my-shell-ext ()
  "Set selected shell mode functions to my extended versions."
  (fset 'shell-resync-dirs
        (symbol-function 'my-shell-resync-dirs))
  (fset 'shell-directory-tracker
        (symbol-function 'my-shell-directory-tracker))
  (fset 'shell-process-cd
        (symbol-function 'my-shell-process-cd))
  (fset 'shell-process-pushd
        (symbol-function 'my-shell-process-pushd))
  (fset 'shell-dirstack-message
        (symbol-function 'my-shell-dirstack-message))
  (fset 'shell-replace-by-expanded-directory
        (symbol-function 'my-shell-replace-by-expanded-directory)) )

(defun shell-toggle-autoexpand ()
 "Toggle history auto-expansion to eliminate strange behavior in interactive
  applications run within the shell.  The list `shell-backup-input-autoexpand'
  stores the list of values to be toggled among.  At the moment, don't set
  shell/comint-input-autoexpand without changing this list as well."
  (interactive)
  (let ((nextia  (car shell-backup-input-autoexpand))
        (otheria (cdr shell-backup-input-autoexpand))
        (curia   comint-input-autoexpand))
    (setq comint-input-autoexpand nextia)
    (setq shell-backup-input-autoexpand (cons otheria nextia))
    (if nextia
       (message (format "Setting Input Autoexpand to %s" nextia))
       (message "Turning Off Input Autoexpand"))))

 ; Additions and fixes to shell directory tracking mechanism,
 ; based on the code in shell.el.

(defun shell-map-home-aliases (indir &optional noslash)
  "Check if DIR matches one of the aliases for HOME
and thus should be replaced by \"~/\" for consistency."
  (let ((dir indir))
    (if (not (string-match "/$" dir))
	(setq dir (concat dir "/")))
    (if (and shell-home-aliases (member dir shell-home-aliases))
	"~/"
      (if noslash indir dir)) ))

(defun my-shell-resync-dirs ()
  "Resync the buffer's idea of the current directory stack.
This command queries the shell with the command bound to 
`shell-dirstack-query' (default \"dirs\"), reads the next
line output and parses it to form the new directory stack.
DON'T issue this command unless the buffer is at a shell prompt.
Also, note that if some other subprocess decides to do output
immediately after the query, its output will be taken as the
new directory stack -- you lose. If this happens, just do the
command again."
  (interactive)
  (let* ((proc (get-buffer-process (current-buffer)))
	 (pmark (process-mark proc)))
    (goto-char pmark)
    (sit-for 0) ; force redisplay
    (comint-send-string proc shell-dirstack-query) 
    (comint-send-string proc "\n")
    (set-marker pmark (point))
    (let ((pt (point))) ; wait for 1 line
      ;; This extra newline prevents the user's pending input from spoofing us.
      (insert "\n") (backward-char 1)
      (while (not (looking-at ".+\n"))
	(accept-process-output proc)
	(goto-char pt))
      (if (looking-at (concat shell-dirstack-query "\n"))
	  (accept-process-output proc)))
    (goto-char pmark) (delete-char 1) ; remove the extra newline
    (forward-line -1)
    ;; That's the dirlist. grab it & parse it.
;    (let* ((dl (buffer-substring (match-beginning 0) (1- (match-end 0))))
    (let* ((dl (buffer-substring (point) (save-excursion (end-of-line) (point))))
	   (dl-len (length dl))
	   (ds nil)			; new dir stack
	   (i 0)
	   new-dir)
      (while (< i dl-len)
	;; regexp = optional whitespace, (non-whitespace), optional whitespace
	(string-match "\\s *\\(\\S +\\)\\s *" dl i) ; pick off next dir
	(setq new-dir (concat comint-file-name-prefix
			      (substring dl (match-beginning 1) (match-end 1))))
	(setq i (match-end 0))
	(if (not (string-match "/$" new-dir))
	    (setq new-dir (concat new-dir "/")))
	(setq ds (cons new-dir ds)) )
      (goto-char (process-mark proc))
      (let ((ds (nreverse ds)))
	(condition-case nil
	    (progn (shell-cd (car ds))
		   (setq shell-dirstack (cdr ds))
		   (my-shell-dirstack-message t))
	  (error (message (format "Could not cd to %s." (car ds)))))))))

(defun my-shell-directory-tracker (str)
  "Tracks cd, pushd and popd commands issued to the shell.
This function is called on each input passed to the shell.
It watches for cd, pushd and popd commands and sets the buffer's
default directory to track these commands.

You may toggle this tracking on and off with M-x dirtrack-toggle.
If emacs gets confused, you can resync with the shell with M-x dirs.

See variables `shell-cd-regexp', `shell-pushd-regexp', and `shell-popd-regexp',
while `shell-pushd-tohome', `shell-pushd-dextract' and `shell-pushd-dunique'
control the behavior of the relevant command.

Environment variables are expanded, see function `substitute-in-file-name'."
  (if shell-dirtrackp
      ;; We fail gracefully if we think the command will fail in the shell.
      (condition-case chdir-failure
	  (let ((start (progn (string-match "^[; \t]*" str) ; skip whitespace
			      (match-end 0)))
		(def-dir  default-directory)
		end cmd arg1)
	    (while (string-match shell-command-regexp str start)
	      (setq end (match-end 0)
		    cmd (comint-arguments (substring str start end) 0 0)
		    arg1 (comint-arguments (substring str start end) 1 1))
	      (cond ((string-match (concat "\\`\\(" shell-popd-regexp "\\)\\($\\|[ \t]\\)") cmd)
		     (shell-process-popd (substitute-in-file-name arg1)))
		    ((string-match (concat "\\`\\(" shell-pushd-regexp "\\)\\($\\|[ \t]\\)") cmd)
		     (shell-process-pushd (shell-map-home-aliases
					   (shell-replace-stack-dir
					    (substitute-in-file-name arg1))
					   t)))
		    ((string-match (concat "\\`\\(" shell-cd-regexp "\\)\\($\\|[ \t]\\)") cmd)
		     (shell-process-cd (shell-map-home-aliases
					(shell-replace-stack-dir
					 (substitute-in-file-name arg1))
					t))))
	      (if (equal def-dir default-directory)
		  nil
		(setq shell-last-dir def-dir)
		(setq def-dir default-directory))
	      (setq start (progn (string-match "[; \t]*" str end) ; skip again
				 (match-end 0)))))
	    (error (message (format "Could not cd: %s" (car (cdr chdir-failure))))))))

(defun shell-replace-stack-dir (str &optional nomsg defdir)
  "Replace reference to directory stack in given argument by its
value.  Leading parts of STR matching -, =n, =- (for n >= 0) are
replaced by the corresponding directory element (last dir, nth on
stack, last on stack, resp); otherwise STR itself is returned. Strings
of the form +n are not processed since, depending on variables like
'shell-pushd-dextract', these might require special treatment.
Replacement handles the addition of '/', so it is not necessary that
directory elements end in '/'.  If n above is larger than the
directory stack length, a message to that effect is printed, unless
NOMSG is non-nil.  When an ERROR occurs or when a 0 reference into the
stack is given, returns DEFDIR, which defaults to default-directory if
not provided."
  (let* ((def-dir (or defdir default-directory))
	 (stack   (cons def-dir shell-dirstack))
	 (stacklen (length stack))
	 (num (and (stringp str)
		   (or (and (string-match "^=\\([0-9]+\\)\\(/.*\\|\\s \\|$\\)" str)
			    (string-to-number (substring str (match-beginning 1) (match-end 1))))
		       (and (string-match "^=-\\(/.*\\|\\s \\|$\\)" str) (1- stacklen)))))
	 dir-head
	 dir-tail)
    (cond ((and (stringp str) (string-equal "-" str)) shell-last-dir)
	  ((null num) str)
	  ((>= num stacklen)
	   (if (not nomsg)
	       (signal 'error
		       (list (format "Directory stack not that deep (%d)." num))))
	   def-dir)
	  ((string-match "^=\\(\\([0-9]+\\|-\\)/?\\)" str) ;; assumes dirstack entries end in '/'
	   (setq dir-head (nth num stack))
	   (setq dir-tail (substring str (match-end 1)))
	   (concat dir-head (if (not (string-match "/$" dir-head)) "/") dir-tail))
	  (t str))))

; cd [dir]
(defun my-shell-process-cd (arg)
  (let ((new-dir (cond ((zerop (length arg)) (concat comint-file-name-prefix
						     "~/"))
		       ((string-equal "-" arg) shell-last-dir)
		       (t (shell-prefixed-directory-name arg)))))
    (shell-cd new-dir)
    (shell-dirstack-message)))

; pushd [+n | dir]
(defun my-shell-process-pushd (arg)
  (let ((num (shell-extract-num arg)))
    (cond ((zerop (length arg))
	   ;; no arg -- swap pwd and car of stack unless shell-pushd-tohome
	   (cond (shell-pushd-tohome
		  (shell-process-pushd (concat comint-file-name-prefix "~/")))
		 (shell-dirstack
		  (let ((old default-directory))
		    (shell-cd (car shell-dirstack))
		    (setq shell-dirstack
			  (cons old (cdr shell-dirstack)))
		    (shell-dirstack-message)))
		 (t
		  (message "Directory stack empty."))))
	  ((numberp num)
	   ;; pushd +n
	   (cond ((> num (length shell-dirstack))
		  (message "Directory stack not that deep."))
		 ((= num 0)
		  (error (message "Couldn't cd.")))
		 (shell-pushd-dextract
		  (let ((dir (nth (1- num) shell-dirstack)))
		    (shell-process-popd arg)
		    (shell-process-pushd dir)))
		 (t
		  (let* ((ds (cons default-directory shell-dirstack))
			 (dslen (length ds))
			 (front (nthcdr num ds))
			 (back (reverse (nthcdr (- dslen num) (reverse ds))))
			 (new-ds (append front back))
			 (new-dir (car new-ds)))
		    (shell-cd new-dir)
		    (setq shell-dirstack (cdr new-ds))
		    (shell-dirstack-message)))))
	  (t
	   ;; pushd <dir>
	   (let* ((old-wd default-directory)
		 (dir (shell-prefixed-directory-name arg))
		 (ex-dir (expand-directory-name dir)))
	     (if (not (string-match "/$" dir))
		 (setq dir (concat dir "/")))
	     (shell-cd dir)
	     (setq shell-dirstack (cons old-wd shell-dirstack))
	     (if (and shell-pushd-dunique
		      (member ex-dir
			      (mapcar 'expand-directory-name shell-dirstack)))
		 (setq shell-dirstack
		       (remove-matching-elements ex-dir shell-dirstack
						 'equal-dir-name))))
	   (shell-dirstack-message)))))

(defun my-shell-dirstack-message (&optional show)
  "Show the current dirstack on the message line.
If the variable shell-pushd-silent is non-nil, then no message
is shown.  This mimicks the optional behavior of tcsh."
  (if (and shell-pushd-silent (null show))
      nil
    (let* ((msg "")
	   (ds (cons default-directory shell-dirstack))
	   (home (expand-file-name (concat comint-file-name-prefix "~/")))
	   (homelen (length home)))
      (while ds
	(let ((dir (car ds)))
	  (and (>= (length dir) homelen) (string= home (substring dir 0 homelen))
	       (setq dir (concat "~/" (substring dir homelen))))
	  ;; Strip off comint-file-name-prefix if present.
	  (and comint-file-name-prefix
	       (>= (length dir) (length comint-file-name-prefix))
	       (string= comint-file-name-prefix
			(substring dir 0 (length comint-file-name-prefix)))
	       (setq dir (substring dir (length comint-file-name-prefix)))
	       (setcar ds dir))
	  (setq msg (concat msg (directory-file-name dir) " "))
	  (setq ds (cdr ds))))
      (message "%s" msg))))

(defun my-shell-replace-by-expanded-directory ()
  "Expand directory stack reference before point.
Directory stack references are of the form \"=digit\" or \"=-\".
See `default-directory' and `shell-dirstack'.

Now handles trailing / correctly.

Returns t if successful."
  (interactive)
  (if (comint-match-partial-filename)
      (save-excursion
	(goto-char (match-beginning 0))
	(let ((stack (cons default-directory shell-dirstack))
	      (index (cond ((looking-at "=-/?")
			    (length shell-dirstack))
			   ((looking-at "=\\([0-9]+\\)/?")
			    (string-to-number
			     (buffer-substring
			      (match-beginning 1) (match-end 1)))))))
	  (cond ((null index)
		 nil)
		((>= index (length stack))
		 (error "Directory stack not that deep."))
		(t
		 (replace-match (file-name-as-directory (nth index stack)) t t)
		 (message "Directory item: %d" index)
		 t))))))

;; Configuration

(setq-default shell-file-name "/bin/tcsh")

(defun my-shell-mode-hook ()
  ;; (require 'my-shell-ext)
  (local-set-key "\C-c\C-q" 'shell-toggle-autoexpand)
  (local-set-key "\C-a" 'my-comint-bol)
  (setq shell-pushd-regexp "\\(pd\\|cd\\|pushd\\)")
  (setq shell-cd-regexp    "chdir")
  (setq shell-popd-regexp  "\\(pf\\|popd\\)")
  (setq shell-prompt-pattern "^[^#%$>]*[#$%>]>? *" )
  (setq shell-pushd-dextract t)
  (setq shell-pushd-dunique t)
  (setq shell-pushd-silent nil)
  (setq shell-pushd-tohome t)
  (setq comint-process-echoes t)
  (setq comint-input-autoexpand 'input)
  (setq shell-input-autoexpand 'input)
  (setq shell-backup-input-autoexpand (cons nil shell-input-autoexpand))
  ;;
  ;; Check that starting directory is correct on startup
  ;;
  (setq default-directory (shell-map-home-aliases default-directory))
  ;;
  ;; Include cwd on the mode line
  ;;
  (make-local-variable 'mode-line-format)
  (setq mode-line-format
        (reverse (append
                  (list " -%-" 'default-directory "-- ")
                  (cdr (reverse (default-value 'mode-line-format))))))
  ;;
  ;; Configure Output Filter 
  ;;
  (add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt nil t)
  ;;
  ;; Set-up replacements for shell.el functions, mine given below
  ;;
  (use-my-shell-ext)
  )

(add-hook 'shell-mode-hook 'my-shell-mode-hook)


