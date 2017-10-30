;;; cc-modes.el -- Common C mode editing and tools -*- lexical-binding: t; -*-

;;; Commentary:
;;
;;  These are based on the common modes in cc-mode.el. This currently
;;  handles C, C++, Java, and Objective C. Java might be moved to its
;;  own component if I get Eclim working.


;;; Code:

(autoload 'c-mode    "cc-mode" "C Editing Mode" t)    ;; Use cc-mode for all C editing
(autoload 'c++-mode  "cc-mode" "C++ Editing Mode" t)
(autoload 'java-mode "cc-mode" "Java Editing Mode" t)

(add-my-hook c-initialization-hook
  (define-key c-mode-base-map [(control meta h)] nil)
  (define-key c-mode-base-map [(control c) (control h)] 'c-mark-function))

(add-my-hook c-mode-common-hook
  "Base hook for cc-mode programming languages"
  (if (not (assoc "crg" c-style-alist))
      (c-add-style "crg" c-crg-style))
  (c-set-style "crg")
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
  (local-set-key [f4]       'c-fill-paragraph))
  
(defvar c-crg-style
  '((c-basic-offset . 4)
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
			(inher-intro . ++)))  ; was c-lineup-inher-intro but that seems gone now
    (c-auto-newline . t)
    (c-hanging-braces-alist . ((brace-list-open)
			       (brace-list-close)
			       (substatement-open before after)
			       (block-close before after)))
    (c-hanging-comment-ender-p . nil)
    (c-cleanup-list . '(scope-operator list-close-comma)))
  "My personal formatting style for editing C/C++ source.")

(defconst java-crg-style
  `((c-recognize-knr-p . nil) ; K&R style argument declarations are not valid
    (c-basic-offset . 4)
    (indent-tabs-mode . nil) ; tabs are evil!
    (c-comment-only-line-offset . 0)
    (c-hanging-braces-alist . ((defun-open after)
                               (defun-close before after)
                               (class-open after)
                               (class-close before after)
                               (namespace-open after)
                               (brace-entry-open after)
                               (inline-open after)
                               (inline-close before after)
                               (block-open after)
                               (block-close . c-snug-do-while)
                               (extern-lang-open after)
                               (extern-lang-close after)
                               (statement-case-open after)
                               (substatement-open after)))
    (c-hanging-colons-alist . ((case-label)
                               (label after)
                               (access-label after)
                               (member-init-intro before)
                               (inher-intro)))
    (c-hanging-semi&comma-criteria
     . (c-semi&comma-no-newlines-for-oneline-inliners
        c-semi&comma-inside-parenlist
        c-semi&comma-no-newlines-before-nonblanks))
    (c-indent-comments-syntactically-p . nil)
    (comment-column . 40)
    (c-cleanup-list . (brace-else-brace
                       brace-elseif-brace
                       brace-catch-brace
                       defun-close-semi
                       list-close-comma
                       scope-operator))
    (c-offsets-alist . ((member-init-intro . ++)
                        (inher-intro . ++)
                        (comment-intro . 0)
                        (topmost-intro . 0)
                        (block-open . 0)
                        (inline-open . 0)
                        (substatement-open . 0)
                        (statement-cont .
                         (,(when (fboundp 'c-no-indent-after-java-annotations)
                             'c-no-indent-after-java-annotations)
                          ,(when (fboundp 'c-lineup-assignments)
                             'c-lineup-assignments)
                          ++))
                        (label . /)
                        (case-label . /)
                        (statement-case-open . +)
                        (statement-case-intro . +)
                        (access-label . /) ; case w/o {
                        (func-decl-cont . c-lineup-java-throws)
                        (inher-cont . c-lineup-java-inher)
                        (innamespace . 0)
                        (arglist-intro  . c-lineup-arglist-intro-after-paren)
                        (arglist-close  . c-lineup-arglist)
                        (arglist-cont-nonempty . c-lineup-arglist)
                        (annotation-var-cont . 0)))
    (c-block-comment-prefix . "* "))
  "My Java Programming Style")

(add-my-hook java-mode-hook
  (setq indent-tabs-mode nil)
  (if (not (assoc "java-crg" c-style-alist))
      (c-add-style "java-crg" java-crg-style))
  (c-set-style "java-crg")
  ;(c-set-style "java")
  (local-set-key "{" 'my/c-mode-balanced-brace)
  (subword-mode))


;;; cc-modes.el ends here
