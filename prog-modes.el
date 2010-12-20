;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                           ;;
;;(@* "Programming Modes")                                   ;;;;;;
;;                                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Generic Programming Modes

(defun my-generic-prog-modes-hook ()
  "Sets up environment useful for a variety of programming modes.
   Add this to the corresponding hooks."
  (highlight-attn-words)
  (turn-on-local-comment-auto-fill))

(when (featurep 'header2)
  (setq header-copyright-notice "Copyright (C) 2010, Christopher R. Genovese, all rights reserved.\n")
  (setq header-date-format "%Y %b %d %a %H:%M:%S"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; IDE Configuration CEDET/ECB/JDEE  (necessary files loaded in imports.el)

(defun my-semantic-init-hook ()
  (global-semantic-idle-scheduler-mode '1)
  (semantic-load-enable-excessive-code-helpers)
  (require 'semantic-ia))

(add-hook 'semantic-init-hook 'my-semantic-init-hook)

(eval-after-load 'senator   ;; alternatively could put this in a hook
  '(progn
     (define-key senator-mode-map "\C-c,c"    'semantic-ia-describe-class)
     (define-key senator-mode-map "\C-c,d"    'semantic-ia-show-doc)
     (define-key senator-mode-map "\C-c,D"    'semantic-ia-show-summary)
     (define-key senator-mode-map "\C-c,e"    'eassist-list-methods)
     (define-key senator-mode-map "\C-c,F"    'semantic-ia-fast-jump)
     (define-key senator-mode-map "\C-c,l"    'semantic-complete-jump-local-members)
     (define-key senator-mode-map "\C-c,m"    'semantic-ia-complete-symbol)
     (define-key senator-mode-map "\C-c,M"    'semantic-ia-complete-symbol-menu)
     (define-key senator-mode-map "\C-c,s"    'semantic-symref)
     (define-key senator-mode-map "\C-c,S"    'semantic-symref-symbol)
     (define-key senator-mode-map "\C-c,x"    'semantic-symref-regexp)
     (define-key senator-mode-map "\C-c,y"    'semantic-ia-show-summary)
     (define-key senator-mode-map "\C-c,\C-k" 'senator-kill-tag)
     (define-key senator-mode-map "\C-c,\M-c" 'senator-copy-tag)
     (define-key senator-mode-map "\C-c,\C-w" nil)
     (define-key senator-mode-map "\C-c,\M-w" nil)
     (define-key senator-mode-map [?\C-c ?, return] 'semantic-mrub-switch-tags)
     ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Emacs-Lisp Mode

(defun my-emacs-lisp-mode-hook ()
  (local-set-key "\C-c\C-e" 'eval-defun)
  (local-set-key "\C-c\C-o" 'outline-minor-mode)
  (local-set-key "\C-c\C-s" 'eval-last-sexp)
  (defun emacs-lisp-outline-minor-setup ()
    (setq outline-regexp ";;; \\|;; \\|(....")
    (local-set-key "\C-c\C-m" outline-mode-prefix-map)
    (when (featurep 'org)
      (define-key outline-minor-mode-map [(control tab)] 'org-cycle)
      (define-key outline-minor-mode-map [(shift tab)]   'org-global-cycle))) 
  (make-local-variable 'outline-minor-mode-hook)
  (add-hook 'outline-minor-mode-hook 'emacs-lisp-outline-minor-setup))

(add-hook 'emacs-lisp-mode-hook 'highlight-attn-words)
(add-hook 'emacs-lisp-mode-hook 'my-emacs-lisp-mode-hook)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Lisp Interaction Mode

;; (This is for Emacs Lisp, see SLIME below.)
;; 

(defun my-lisp-interaction-mode-hook ()
  (local-set-key "\C-j"     'newline-and-indent)
  (local-set-key [C-return] 'eval-print-last-sexp)
  (local-set-key "\C-c\C-c" 'eval-print-last-sexp))

(add-hook 'lisp-interaction-mode-hook 'my-lisp-interaction-mode-hook)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Python Mode

(defun my-python-mode-hook ()
  (local-set-key "\M-\C-a"  'beginning-of-python-def-or-class)
  )

(add-hook 'python-mode-hook 'my-python-mode-hook)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Common C Modes (cc-mode) (C, C++, Java)

(autoload 'c-mode    "cc-mode" "C Editing Mode" t)    ;; Use cc-mode for all C editing
(autoload 'c++-mode  "cc-mode" "C++ Editing Mode" t)
(autoload 'java-mode "cc-mode" "Java Editing Mode" t)

(defun my-c-mode-common-hook ()
  (if (not (assoc "CRG" c-style-alist))
      (c-add-style "CRG" (cdr c-crg-style)))
  (c-set-style "CRG")
  (if (assq 'c-auto-hungry-string minor-mode-alist)   ;; Inhibit minor mode notation
      (setq minor-mode-alist
	    (cons '(c-auto-hungry-string "") (remove-matching-elements 'c-auto-hungry-string minor-mode-alist 'eq-car))))
  (local-set-key "\C-c\C-a" 'c-beginning-of-statement)
  (local-set-key "\C-c\C-e" 'c-end-of-statement)
  (local-set-key "\C-c\C-b" 'c-backward-conditional)
  (local-set-key "\C-c\C-f" 'c-forward-conditional)
  (local-set-key "\C-c\C-c" 'comment-region)
  (local-set-key "\M-q"     'c-fill-paragraph)
  (local-set-key "\C-c\C-d" 'c-down-conditional)
  (local-set-key "\C-c\C-u" 'c-up-conditional)
  (local-set-key "\C-c\C-i" 'c-indent-exp)
  (local-set-key "\C-c\C-q" 'c-indent-defun)
  (local-set-key "\C-c\C-m" 'compile);; m for make
  (local-set-key "\C-c\C-n" 'up-list)
  (local-set-key "\C-c\C-p" 'c-up-list-neg)
  (local-set-key "\C-c\C-o" 'outline-minor-mode)
  (local-set-key "\C-c\C-t" 'c-set-offset)
  (local-set-key "\C-c\C-x" 'c-macro-expand)
  (local-set-key [f4]       'c-fill-paragraph)
  )
  
(add-hook 'c-mode-common-hook 'highlight-attn-words)
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

(defvar c-crg-style
  '("CRG"
    (c-basic-offset . 4)
    (c-comment-only-line-offset . (0 . 0))
    (c-offsets-alist . ((statement-block-intro . +)
			(substatement-open . 0)
			(label . 0)
			(case-label . *)
			(access-label . /)
			(statement-case-intro . *)
			(statement-cont . +)
			(cpp-macro . 0)
                        (inline-open . 0)
			(inher-intro . c-lineup-inher-intro)
			))
    (c-auto-newline . t)
    (c-hanging-braces-alist . ((brace-list-open)
			       (brace-list-close)
			       (substatement-open before after)
			       (block-close before after)))
    (c-hanging-comment-ender-p . nil)
    (c-cleanup-list . '(scope-operator list-close-comma))
    )
  "My personal formatting style for editing C/C++/Java source.")

(defun c-up-list-neg (arg)
  "Like up-list but with a negated arg"
  (interactive "P")
  (if (not arg)
      (up-list -1)
    (up-list arg)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; C-mode (old)

(defun my-c-mode-hook ()
  (setq indent-tabs-mode nil)
  (local-set-key "\C-c\C-z" 'compilation-close-compilation)
  )

(add-hook 'c-mode-hook 'my-c-mode-hook)

(defvar boc-mode-hook                    
  '((lambda ()
      (setq c-indent-level '4)
      (setq c-continued-statement-offset '4)
      (setq c-continued-brace-offset '-4)
      (setq c-brace-offset '0)
      (setq c-brace-imaginary-offset '0)
      (setq indent-tabs-mode 'nil) 
      (setq c-label-offset '-2)
      (setq c-auto-newline t)
      ))
  "Style hook for \"Boring Old C-mode\" in case
it is needed to run an earlier version of emacs.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; C++-mode

(defun my-c++-mode-hook ()
  (setq indent-tabs-mode nil)
  (local-set-key "\C-c\C-z" 'compilation-close-compilation)
  )

(add-hook 'c++-mode-hook 'my-c++-mode-hook)

(defun c-lineup-inher-intro (langelem)
  "Line up inheritence intro : two characters past class keyword"
  (+ c-basic-offset (/ c-basic-offset 2))) 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; java-mode

(defun my-java-mode-hook ()
  (setq indent-tabs-mode nil)
  )

(add-hook 'java-mode-hook 'my-java-mode-hook)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; compilation-mode

(defun my-compilation-mode-hook ()
  (local-set-key "\C-c\C-z" 'compilation-close-compilation)
  )

(add-hook 'compilation-mode-hook 'my-compilation-mode-hook)

(defun compilation-close-compilation (arg)
  "Iconifies compilation pop-up frame.  With prefix arg,
kills *compilation* buffer and thus its dedicated frame,
if the latter exists."
  (interactive "P")
  (let* (err-msg buf)
    (setq buf (condition-case err-msg
		  (setq buf (compilation-find-buffer))
		(error (and (message (car (cdr err-msg))) nil))))
    (if (and buf (not arg))
	(let ((win (get-buffer-window buf t)))
	  (if win
	      (iconify-frame (window-frame win))))
      (if (and buf arg)
	  (kill-buffer buf))
      )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; perl-mode


(defun my-perl-mode-hook ()
  (setq perl-indent-level '4)
  (setq perl-continued-statement-offset '4)
  (setq perl-continued-brace-offset '-4)
  (setq perl-brace-offset '0)
  (setq perl-brace-imaginary-offset '0)
  (setq indent-tabs-mode 'nil) 
  (setq perl-label-offset '-2)
  )

(add-hook 'perl-mode-hook 'highlight-attn-words)
(add-hook 'perl-mode-hook 'my-perl-mode-hook)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; OCaml tuareg-mode

    ;; no hook yet...


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SLIME

;; Added Functionality
;;
;; SLIME works with a variety of common lisp implementations, and more
;; recently with clojure. However, the interface to clojure -- in
;; particular, how the process is started or stopped -- is different
;; than for the common lisp implementations. The interface defined below
;; works for both.
;;
;; Use `slime/register-a-lisp' to register an implementation as avaiable
;; and indicate any processing that needs to be done to start and stop
;; the process. (See the documentation for this function for discussion
;; of the nontrivial options.) Then, `slime/start' and `slime/end' do
;; the necessary work: making connections, starting and stopping the
;; processes, managing the various buffers.
;;
;; The configuration variable `slime/default-lisp' determines which
;; implementation is used by default. When this is nil, the most
;; recently registered implementation is used, unless `slime/start'
;; is invoked with a prefix argument, in which case the user is
;; prompted for the implementation.
;;
;; Also, see `slime-clojure' for the function that handles setup for the
;; clojure implementation.
;;

(defvar slime/default-lisp nil
  "Default lisp to use when invoking slime
through the new interface. Value should be
either a symbol in `slime/registered-lisps'
or nil, in which case the first registered
lisp will be used.")

(defvar slime/registered-lisps nil
  "List of symbols representing registered
lisp implementations available through this
interface.")

(defvar slime/running nil
  "List of symbols representing lisp implementations
that appear to be currently running. Any actions
taken on the basis of this list should deal gracefully
with the possiblility that the user might have killed
the corresponding processes another way.")

(defun* slime/register-a-lisp (lisp-sym
                               &key (name (symbol-name lisp-sym))
                               &key (command (list name))
                               &key call
                               &key (body (cond
                                           (call
                                            `((,call)))
                                           (command
                                            `((slime ',lisp-sym)))
                                           (t
                                            '((message "Slime invocation missing."))))
                                          body-given?)
                               &key quit)
  "Create and configure a new SLIME lisp implementation and
create the necessary callbacks for running it.

LISP-SYM is a symbol to be associated with the registered lisp.
Unless the :CALL or :BODY keywords are supplied, LISP-SYM is used
as the key in the alist `slime-lisp-implementations'.

:NAME determines the name of the function to start the given lisp
in slime. If this keyword is supplied, its value should be a
string. The invocation function is called `slime/start-NAME',
where NAME is the associated value. By default, NAME is the
symbol-name of LISP-SYM.

:COMMAND should be a list of strings representing a shell command
to invoke in order to begin the associated lisp process. By
default it is a singleton list consisting of NAME. Unless
the :CALL or :BODY keywords are supplied in the call, this
command list will be entered as the value associated with
LISP-SYM in `slime-lisp-implementations.

:CALL is a shortcut for defining a function that invokes the
given lisp when a command in `slime-lisp-implementations' is
insufficient. If this keyword is supplied, its value should be a
symbol with a non-void function definition. This function will be
called when the given lisp is invoked. When missing, the :BODY
keyword defines this function, and more generally.

:BODY defines a function to invoke the given lisp when a command
in `slime-lisp-implementations' is insufficient. If this keyword
is supplied, its value should be a list of S-expressions that
will comprise the body of the function slime/start-NAME. If :BODY
is not supplied, then the :CALL keyword takes precedence in
defining the function. If both :BODY and :CALL are missing, then
the function invokes `slime' with the symbol LISP-SYM.

:QUIT defines the body of a function to invoke when the given
lisp is stopped. If this keyword is supplied, its value should
have the same structure as that of the :BODY keyword, namely a
list of S-expressions. These will be put in a function
`slime/stop-NAME'.

Note that any function namde `slime/start-NAME' will have its
function definition overridden.
"
  (when (not (symbolp lisp-sym))
    (error "add-slime-lisp argument LISP-SYM must be a symbol"))

  ; Create functions to start and end the given lisp
  (let ((ssym (intern (concat "slime/start-" name)))
        (esym (intern (concat "slime/stop-"  name))))
    (fset ssym
          `(lambda ()
             (interactive)
             ,@body
             (add-to-list 'slime/running ',lisp-sym))) 
    (fset esym
          `(lambda (&optional kill-it?)
             (interactive "P")
             (condition-case nil
                 (slime-quit-lisp t)
               (error nil))
             (sit-for 1)
             (message ,(concat "Stopping " (symbol-name lisp-sym)))
             ,@quit
             (my-awhen (get-buffer ,(concat "*slime-repl " name "*"))
               (if kill-it?
                   (kill-buffer it)
                 (with-current-buffer it
                   (goto-char (point-max))
                   (insert "\n\nFinished.\n"))))
             (setq slime/running (remove-matching-elements ',lisp-sym slime/running 'eq)))))

  ; Add lisp to slime's implementation list
  ; unless an explicit function body was given
  (when (and (not body-given?) (not call))
    (add-to-list 'slime-lisp-implementations
                 (cons lisp-sym (list command))))
  (add-to-list 'slime/registered-lisps lisp-sym))

(defun slime/validate-lisp-descriptor (which-lisp prefix-arg?)
  "Construct the symbol and its name from a descriptor
and an optional prefix arg. The descriptor WHICH-LISP,
if non-nil, should be a symbol in `slime/registered-lisps' or a string giving
the name of such a symbol. If WHICH-LISP is nil,
the default implementation is used (see `slime/default-lisp')
when called from a lisp program or when called interactively
without a prefix arg. When called interactively with a prefix
arg, the implementation to use is read from the minibuffer.
Other values for which-lisp raise an error; an error is also
raised if no default value can be constructed when needed.

Returns a cons cell of the form (SYMBOL NAME).
"
  (let (sym name)
    (cond
     ((and (null which-lisp) prefix-arg?)
      (setq name (completing-read
                  "Lisp implementation: "
                  (mapcar #'symbol-name slime/registered-lisps)
                  nil t))
      (setq sym (intern name)))
     ((null which-lisp)
      (setq sym (or slime/default-lisp (car-safe slime/registered-lisps)))
      (setq name (and sym (symbol-name sym))))
     ((stringp which-lisp)
      (setq name which-lisp)
      (setq sym (intern name)))
     ((symbolp which-lisp)
      (setq sym which-lisp)
      (setq name (symbol-name sym)))
     (t
      (error (concat "Invalid lisp implementation, "
                     (prin1-to-string which-lisp)))))
    (when (null sym)
      (error "No lisp implementations registered"))
    (when (null (memq sym slime/registered-lisps))
      (error "Default lisp has not been registered"))
    (list sym name)))

(defun slime/start (&optional which-lisp)
  "Start a lisp implementation in slime. WHICH-LISP, if non-nil,
should be a symbol in `slime/registered-lisps' or a string giving
the name of such a symbol. If WHICH-LISP is nil,
the default implementation is used (see `slime/default-lisp')
when called from a lisp program or when called interactively
without a prefix arg. When called interactively with a prefix
arg, the implementation to use is read from the minibuffer.
"
  (interactive)
  (when (null slime/registered-lisps)
    (error "No Registered Lisp Implementations Available"))

  (let* ((lisp-impl
          (second (slime/validate-lisp-descriptor
                   which-lisp current-prefix-arg)))
         (fsym (intern (concat "slime/start-" lisp-impl))))
    (funcall fsym)))

(defun slime/stop (&optional which-lisp kill-the-buffer?)
  "Stop a running lisp implementation in slime. When called from
a lisp program, WHICH-LISP should either be nil or should be a
symbol in `slime/registered-lisps' or a string giving the name of
such a symbol. If WHICH-LISP is nil, the implementation is read
from the minibuffer with the most recently run as the initial
input. If KILL-THE-BUFFER? is non-nil, the repl buffer is killed,
otherwise a completion message is printed at the end of that
buffer.
"
  (interactive
   (list
    (completing-read "Quit which lisp? "
                     (mapcar #'symbol-name slime/registered-lisps)
                     nil t nil nil
                     (symbol-name (car slime/running)))
    current-prefix-arg))
  (when (null slime/registered-lisps)
    (error "No Registered Lisp Implementations Available"))

  (let* ((lisp-impl
          (second (slime/validate-lisp-descriptor
                   which-lisp current-prefix-arg)))
         (fsym (intern (concat "slime/stop-" lisp-impl))))
    (funcall fsym kill-the-buffer?)))

;; Clojure support

(defvar slime-swank-clojure-logbuf-name "*swank-clojure-log*"
  "Name of buffer associated with the swank-clojure process. All
output and error messages of the process are logged in this
buffer.")

(defvar slime-swank-clojure-command "swank-clojure"
  "Command to start an asynchronous swank process with which
emacs/slime communicates.")

(defvar slime-swank-clojure-timeout 15
  "Number of seconds to wait for server before asking user if we
should try again.")

(defun slime-clojure (&optional read-args?)
  (interactive "P")
  (let ((logbuf (get-buffer-create slime-swank-clojure-logbuf-name))
        (host "127.0.0.1")
        (port 4005)
        (ready nil))
    (when read-args?
      (setq host (read-from-minibuffer
                  "Host: " (first slime-connect-host-history)
                  nil nil '(slime-connect-host-history . 1)))
      (setq port (string-to-number
                  (read-from-minibuffer
                   "Port: " (first slime-connect-port-history)
                   nil nil '(slime-connect-port-history . 1)))))
    (message "Starting swank server...")
    (async-shell-command slime-swank-clojure-command logbuf logbuf)
    (bury-buffer logbuf)
    ; Wait for swank server to acknowledge with buffer output
    (let ((wait-time 0)
          (keep-waiting t))
      (while (and keep-waiting
                  (= 1 (with-current-buffer slime-swank-clojure-logbuf-name
                         (point-max))))
        (sit-for 1)
        (setq wait-time (1+ wait-time))
        (when (> wait-time slime-swank-clojure-timeout)
          (if (y-or-no-p "No response yet from swank server. Shall I try again? ")
              (setq wait-time 0)
            (setq keep-waiting nil))))
      (when keep-waiting (setq ready t)))
    (if (not ready)
        (message "No response from swank server. Giving up.")
      (sit-for 2)
      (message "Connecting to swank server...")
      (slime-connect host port)
      (sit-for 1)
      (message "Ready."))))


;; Configuration

(setq inferior-lisp-program "sbcl")
(setq slime-default-lisp 'sbcl)
(setq slime-lisp-implementations nil)

(slime/register-a-lisp 'cmucl)
(slime/register-a-lisp 'sbcl)
(slime/register-a-lisp 'clojure
                       :call 'slime-clojure
                       :quit '((my-awhen (get-buffer slime-swank-clojure-logbuf-name)
                                 (my-awhen (get-buffer-process it)
                                   (kill-process it))
                                 (kill-buffer it))))
(setq slime/default-lisp 'clojure)

(with-library 'slime-autoloads
  (slime-setup '(slime-repl)))




