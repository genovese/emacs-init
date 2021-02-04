;;; packages.el -- initial package installation -*- mode: emacs-lisp; lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)
(require 'find-func)


;;; Configuration

(defvar init/package-archives
  '(("gnu" . "http://elpa.gnu.org/packages/")
    ("melpa" . "http://melpa.org/packages/")
    ("melpa-stable" . "http://stable.melpa.org/packages/")
    ("org" . "http://orgmode.org/elpa/"))
  "Package archives from which to draw initial packages")

(defvar init/priority-packages
  '(org-plus-contrib dash s f ht use-package)
  "Packages that should be installed before all others, in decreasing
order of importance.")


;;; Generic Tools

(defmacro doforms (form-spec &rest body)
  "Loop over the elisp forms in a buffer, evaluate BODY with VAR
bound to each successive form in BUFFER (default: current buffer). 
Then evalute and RESULT (default: nil).

If BUFFER and RESULT are missing, VAR can be supplied directly,
without surrounding parentheses.

\(fn [&or VAR (VAR [BUFFER] [RESULT])] BODY...)"
  (declare (indent 1) (debug ([&or symbolp (symbolp &optional form form)] body)))
  (let ((var (if (listp form-spec) (car form-spec) form-spec))
        (buffer (when (listp form-spec) (cadr form-spec)))
        (result (when (listp form-spec) (cddr form-spec)))
        (ignored (make-symbol "_")))
    `(save-excursion
       ,@(when buffer `((set-buffer ,buffer)))
       (goto-char (point-min))
       (let (,var)
         (while (setq ,var (condition-case ,ignored
                               (read (current-buffer))
                             (end-of-file nil)))
           ,@body))
       ,@result)))


;;; Special Case Package Fixes

(defun init/install-fix-auctex (&optional logger)
  "Make auctex.el provide 'auctex for easier load by use-package"
  (let* ((auctex (condition-case _
                     (find-library-name "auctex")
                   (error nil)))
         (buf (when auctex (find-file-noselect auctex)))
         (compiled? nil)
         (tag "(provide 'auctex)"))
    (when auctex
      (with-current-buffer buf
        (goto-char (point-min))
        (unless (condition-case _ (search-forward tag) (search-failed nil))
          (goto-char (point-max))
          (insert "\n" tag "\n")
          (set (make-local-variable 'backup-inhibited) t)
          (basic-save-buffer)))
      (setq compiled? (byte-compile-file auctex))
      (when logger
        (funcall logger "\nPackage auctex fixed,"
                 "byte compilation " (if compiled? "succeeded." "failed."))))
    compiled?))

(defun init/install-specialty-fixes (&optional logger)
  "Perform all specialty fixes, logging if possible.
LOGGER is a function that takes two strings (and a third optional string)
to send to the echo area and log buffer (if interactive)."
  (interactive (list (lambda (s1 s2 &optional s3)
                       (message "%s %s%s" s1 s2 (or s3 "")))))
  (init/install-fix-auctex logger))


;;; Installation Utilities

(defun init/packages-from-cask (cask-file)
  "Construct list of package symbols from depends-on forms in CASK-FILE.
All other cask constructs, including source, are ignored."
  (interactive "F")
  (with-temp-buffer
    (insert-file-contents cask-file)
    (let (packages)
      (doforms (form nil (nreverse packages))
        (when (eq (car form) 'depends-on)
          (setq packages (cons (intern (cadr form)) packages)))))))

(defun init/promote-priority-packages (package-list priority-packages)
  "Rearrange PACKAGE-LIST so that packages in PRIORITY-PACKAGES come first.
Priority packages are only included if they are in PACKAGE-LIST to begin
with, in which case they occur in the order listed in PRIORITY-PACKAGES.

Returns the updated package list (a copy of the original list)."
  (let (packages priorities)
    (dolist (package package-list)
      (if (memq package priority-packages)
          (setq priorities (cons package priorities))
        (setq packages (cons package packages))))
    (append (cl-remove-if-not (lambda (p) (memq p priorities)) priority-packages)
            (nreverse packages))))

(defun init/install-packages (package-list &optional no-log hidden-log)
  "Install requested package using `package-install'.
PACKAGE-LIST is a list of symbols"
  (let* ((log-buf (or no-log (get-buffer-create "*Package Installation Log*")))
         (log (lambda (header pkg &optional extra)
                (let ((mesg (format "%s %s%s" header pkg (or extra ""))))
                  (message (if (string-match "\\`[ \t\n\r]+" mesg)
                               (replace-match "" t t mesg)
                             mesg))
                  (unless no-log
                    (with-current-buffer log-buf
                      (goto-char (point-max))
                      (insert mesg "\n"))))))
         (success (lambda (sp su pkg &optional extra)
                    (funcall log
                      (concat (propertize sp 'face '(:foreground "green")) su)
                      pkg extra)))
         (failure (lambda (sp su pkg &optional extra)
                    (funcall log
                      (concat (propertize sp 'face '(:foreground "red")) su)
                      pkg extra)))
         (cnt 0)
         (error-cnt 0))
    (unless no-log                      ; initialize log buffer
      (with-current-buffer log-buf
        (view-mode -1)
        (fundamental-mode)
        (setq buffer-read-only nil)
        (erase-buffer)
        (insert "\n\n")))
    (dolist (package package-list)
      (condition-case e
          (progn
            (package-install package)
            (funcall success "Installed" " package" package)
            (setq cnt (1+ cnt)))
        (error
         (funcall failure "**Error**" " installing package" package
                  (concat ": " (cadr e)))
         (setq error-cnt (1+ error-cnt)))))
    (funcall log (format "\nInstalled %d packages in %s" cnt package-user-dir)
             (format "with %d error%s (%d/%d successful)."
                     error-cnt (if (= error-cnt 1) "" "s")
                     cnt (+ cnt error-cnt)))
    (init/install-specialty-fixes log)
    (unless no-log                      ; finalize log buffer
      (with-current-buffer log-buf
        (view-mode 1)
        (setq buffer-read-only t
              view-exit-action #'kill-buffer)
        (goto-char (point-max))
        (beginning-of-line))
      (unless hidden-log
        (set-window-buffer nil log-buf t)))))


;;; Script Entry Point

(defun install-init-packages (&optional install-dir cask-file)
  "Install packages specified in CASK-FILE, with priority packages
from `init/priority-packages' put first."
  (interactive)
  (setq package-enable-at-startup t
        package-archives          init/package-archives
        package-user-dir          (or install-dir
                                      (thread-first "elpa"
                                        expand-file-name
                                        abbreviate-file-name
                                        directory-file-name))
        user-emacs-directory      (file-name-directory
                                   (expand-file-name "./" install-dir)))
  (when (< emacs-major-version 27) (package-initialize))
  (unless package-archive-contents
    (package-refresh-contents))
  (thread-first (or cask-file
                    (let (file)
                      (and install-dir
                           (file-readable-p
                            (setq file (expand-file-name "../Cask" install-dir)))
                           cfile))
                    "init/Extras/Cask")
    (init/packages-from-cask)
    (init/promote-priority-packages init/priority-packages)
    (init/install-packages noninteractive)))

;;; packages.el ends here
