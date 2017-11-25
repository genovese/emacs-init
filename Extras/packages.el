;;; packages.el -- initial package installation -*- mode: emacs-lisp; lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)


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


;;; Tools

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
    (unless no-log  ; initialize log buffer
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
    (funcall log "\nPackage installation complete"
             (format "(%d/%d successful)." cnt (+ cnt error-cnt)))
    (unless no-log ; finalize log buffer
      (with-current-buffer log-buf
        (goto-char (point-min))
        (insert (format "Installed %d packages with %d error%s."
                        cnt error-cnt (if (= error-cnt 1) "" "s")))
        (view-mode 1)
        (setq buffer-read-only t
              view-exit-action #'kill-buffer)
        (goto-char (point-min)))
      (unless hidden-log
        (set-window-buffer nil log-buf t)))))


;;; Script Entry Point

(defun install-init-packages (&optional cask-file)
  "Install packages specified in CASK-FILE, with priority packages
from `init/priority-packages' put first."
  (setq package-enable-at-startup t
        package-archives          init/package-archives)
  (thread-first (or "init/Extras/Cask" cask-file)
    (init/packages-from-cask)
    (init/promote-priority-packages init/priority-packages)
    (init/install-packages noninteractive)))

;;; packages.el ends here
