;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                           ;;
;;(@* "Environment Configuration")                           ;;;;;;
;;                                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This component is automatically generated from a template
;; by ./makeconfig, perl, and make. See Makefile for details.


(defvar my-user-name "genovese"
  "*If non-nil, this should be set to a string giving the user name
to use by default in locating the ....  If set to nil, this will
always use the HOME directory in the environment if it is available
as the root for the startup information.  Set this is you wish to
load your startup file when running as someone else via the -u flag.")

(defvar my-home-aliases (list "/Users/genovese/")
  "*This is a list of strings indicating possible aliases for the users
HOME directory that emacs might not recognize.  The first element of the list should be the
prefered or most common or most specific form.  This variable is most useful with
cross-mounted file systems, and is used by functions to determine whether 
or not a directory name should be changed to \"~/\".")

(defvar my-home-dir (if (and (equal my-user-name (getenv "USER")) (getenv "HOME"))
                        (concat (getenv "HOME") "/")
                      (or  (car my-home-aliases) ""))
  "Home directory")

(defvar my-site-lisp-dir (expand-directory-name (if (= emacs-major-version 22) "/Applications/Emacs22.app/Contents/Resources/site-lisp/" "/Applications/Emacs.app/Contents/Resources/site-lisp/"))
  "The site-lisp directory (absolute path) containing the globally available
   add-on modes and .el files. This name must end in /.")

(defvar my-home-lisp-dir (concat "~/.emacs.d/"
                                 (if (null (string-match "/$" "~/.emacs.d/")) "/"))
  "The site-lisp directory (absolute path) containing the users local
   add-on modes and .el files. This name must end in /.")

(when (>= emacs-major-version 23)
  (setq user-emacs-directory my-home-lisp-dir))

(defvar my-frame-alist `((top . 1) (left . 1) (width . ,(if (= emacs-major-version 23) 224 180)) (height . ,(if (= emacs-major-version 23)  64  62)))
  "Geometry parameters that will be used for initial-frame-alist to determine initial window size.")

(if (and (boundp 'my-home-dir) (stringp my-home-dir)
         (not (string= my-home-dir "")) (not (member my-home-dir my-home-aliases)))
    (setq my-home-aliases (cons my-home-dir my-home-aliases)))


