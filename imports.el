;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                           ;;
;;(@* "Imported Mode Requirements")                          ;;;;;;
;;                                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;
;; Lists of Packages to Import
;;

(defvar my-must-features
  '(comint shell calc)
  "*List of *built-in* features that must be required for later
   processing in this initialization file. Each element is either
   a symbol that is a valid argument to require or is a cons-cell
   whose car is a major version number for Emacs and whose cdr is
   a symbol that is a valid argument to require. These features
   will be required, and failure of the require will lead to an
   error.")

(defvar my-ought-features
  '(org-install icicles ido)
  "*List of features that are distributed with emacs that would be
   desireable to require for later processing in this initi file. 
   If for some reason, they are not available, a message will be
   displayed but no error produced.

   Each element of the list is either a symbol that is a valid
   argument to require or is a cons-cell whose car is a major
   version number for Emacs and whose cdr is a symbol that is a
   valid argument to require.")

(defvar my-want-features
  '(bookmark+ linkd lacarte dropdown-list yasnippet anything-config)
  "*List of features, not necessarily available, that it would be 
   desireable to require for later processing in this init file. 
   If for some reason, they are not available, a message will be
   displayed but no error produced.

   Each element of the list is either a symbol that is a valid
   argument to require or is a cons-cell whose car is a major
   version number for Emacs and whose cdr is a symbol that is a
   valid argument to require.")

(defvar my-autoloaded-features
  (list '(nxml-mode                 "nxml-mode.el"        "nXML Mode"      t)
        (if (<= emacs-major-version 22)
            '(nxhtml-mode           "nxml/autostart.el"   "nXHTML Mode"    t)
          '(nxhtml-mode             "nxhtml/autostart.el" "nXHTML Mode"    t))
        '(texmathp-compile          "texmathp"            "Parse TeX Math" t)
        )
  "*List of features to be autoloaded along with the arguments
   to be passed directly to autoload. Elements of the lists are of
   the form (SYMBOL ARGS...). Note that this list need not include
   any (auto-generated) autoloads in the system files.")

(defvar my-special-features
  (list
   (list 'dired-x    '(deferred-import "Loaded when dired is loaded, see my-dired-load-hook."))
   (list 'dired-aux  '(deferred-import "Loaded when dired is loaded, see my-dired-load-hook."))
   (list 'dired+     '(deferred-import "Loaded when dired is loaded, see my-dired-load-hook."))
   (list 'ibuf-ext   '(deferred-import "Only loaded as needed when ibuffer is first instantiated"))
   (list 'texmathp   '(deferred-import "Only loaded as needed when AucTeX is first instantiated"))
   (list 'ess-site   '(deferred-import "Temporarily deferring load to ess.el for timing purposes"))
   (list 'slime-autoloads '(deferred-import "Loaded after SLIME setup."))
   (list 'thingatpt+ '(eval-after-load "thingatpt"
                        '(require-soft 'thingatpt+)))
   (list 'cedet      '(if (not (featurep 'cedet))
                            (load-file (concat my-site-lisp-dir "cedet/common/cedet.el"))))
   (list 'jde        '(progn
                        (setq defer-loading-jde t)
                        (if (not defer-loading-jde)
                            (require-soft 'jde)
                          (autoload 'jde-mode "jde" "JDE mode." t)
                          (add-to-list 'auto-mode-alist '("\\.java\\'" . jde-mode))
                          )))
   (list 'ecb        '(progn
                        (eval-when-compile
                          (if (not (featurep 'cedet))
                              (require 'cedet)))
                        (require-soft 'ecb)
                        ))
   )
  "List of features that will need special processing (usually beyond
   a simple require) for use in this init file. Although somewhat
   awkward, the code to handle these cases should be given as
   a form in the list. Specifically, each element of the list
   should in turn be a list with either two or three elements
   corresponding to 

      (list 'SYMBOL 'BODYFORM) 
      (list  CONDITION 'SYMBOL 'BODYFORM),

   where CONDITION is a lisp expression that evaluates to t
   if this feature should be acted upon. A typical example
   for condition would be (>= emacs-major-version 22).
   In any case, CONDITION must be valid for evaluation at
   either load or compile time.")


;;
;; Do Imports and Autoloads
;;

(import my-must-features       'require)
(import my-ought-features      'require-soft)
(import my-want-features       'require-soft)
(import my-autoloaded-features 'autoload)
(import my-special-features    (lambda (sym body) (eval body)))


;;
;; Automatic Mode Detection
;;

(add-to-list 'auto-mode-alist '("\\.[0-9]\\'" . fundamental-mode))
(add-to-list 'auto-mode-alist '("\\.\\([pP]\\([LlmM]?\\|od\\|erl\\)\\|al\\)\\'" . perl-mode))
(add-to-list 'auto-mode-alist 
             (cons (concat "\\." (regexp-opt '("xml" "xsd" "sch" "rng" "xslt" "svg" "rss") t) "\\'")
                   'nxml-mode))
(add-to-list 'auto-mode-alist '("\\.s?html?\\'" . nxhtml-mode))
(add-to-list 'auto-mode-alist '("\\.pde\\'" . arduino-mode))
(add-to-list 'auto-mode-alist '("\\.[sS]\\'" . S-mode))
(add-to-list 'auto-mode-alist '("\\.[rR]\\'" . R-mode))
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-to-list 'auto-mode-alist '("\\.txt\\'" . org-mode))

(fset 'xml-mode  'nxml-mode)    ;; should rhs be (symbol-function 'nxml-mode) ??
(fset 'html-mode 'nxhtml-mode)

(add-to-list 'magic-mode-alist '("<\\?xml " . nxml-mode))
(add-to-list 'magic-mode-alist '("#!.*perl" . perl-mode))
(add-to-list 'magic-mode-alist '("#!.*python" . python-mode))
(add-to-list 'magic-mode-alist '("#!.*\\(t?c\\|ba\\|k\\|z\\|\\)sh" . shell-script-mode))
(add-to-list 'magic-mode-alist '("\\\\input " . plain-TeX-mode))
(add-to-list 'magic-mode-alist '("\\\\document\\(style\\|class\\)" . LaTeX-mode))


;;
;; Help Configuration
;; 
;; Remapping Help Keys, as in (@> "Global Key Bindings") below,
;; requires some coordination for imported modes. So, describe
;; new help events here for later use.
;;

(setq my-help-events   ; try several for now and see
      (list [?\M-\C-h] [?\C-c ?h] [?\C-c ?\C-h]))  
(setq help-event-list ; only keys seem to be allowed
      (nconc (list ?\M-\C-h) help-event-list))
;;or eventually, if I settle on \M-\C-h, do:
;(setq help-char ?\M-\C-h)


