;;; user-system.el --- settings reflecting user- and system-specific configuration -*- lexical-binding: t; -*-

(defconst my-platform
  (cond ((or (string-equal system-type "darwin")
             (eq window-system 'ns)
             (eq window-system 'mac))
	 'macosx)
	((or (string-equal system-type "ms-dos")
             (string-equal system-type "windows-nt")
	     (string-equal system-type "cygwin"))
	 'windows)
        (t
         'linux))
  "The platform on which we are currently running, derived from
the variables `system-type' and `window-system'. Value is a
symbol. For the moment, all *nix variants are converted to
`linux', though this can be generalized later if needed")

(defvar user-home-directory
  (let ((user-env (getenv "USER"))
        (home-env (getenv "HOME")))
    (cond
     ((and (equal user-login-name user-env) home-env)
      (f-full home-env))
     ((f-directory? (expand-file-name "~/"))
      (f-full (expand-file-name "~/")))
     (t
      (f-full default-directory))))
  "The users home directory, as a directory name with slash appended.")

(defvar user-site-lisp-dir
  (f-slash* (locate-user-emacs-file "site-lisp"))
  "The site-lisp directory (absolute path) containing the
globally available add-on modes and .el files. This name must end
in /.")

(defvar user-custom-file
  (let ((custom (locate-user-emacs-file "emacs-custom.el")))
    (when (f-exists? custom) custom))
  "File where settings are saved by customize.")

(defvar user-env-file
  (let ((envir (locate-user-emacs-file "my-env.el")))
    (when (f-exists? envir) envir))
  "File setting environment variables not inherited by the system
This is especially useful on Mac OS X which does not load the
users's environment, so items like PATH are mis-set.")

(defvar user-home-page (get-preference 'user-home-page))

(defvar user-email-address (get-preference 'user-email-address))


;;; user-system.el ends here
