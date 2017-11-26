;;; review.el -- tools to review and adjust init file settings -*- mode: emacs-lisp; lexical-binding: t; -*-

(require 'cl-lib)
(require 'map)
(require 'subr-x)
(require 'widget)
(require 'cus-edit)

(eval-when-compile
  (require 'wid-edit))


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


;;; User Interfaces

(defvar review/fancy-buttons-p (facep 'custom-button)
  "Are fancy buttons available?")

(defvar review/ui-wbutton-face
  (if (facep 'custom-button) 'custom-button 'widget-button))

(defvar review/ui-button-face
  (if (facep 'custom-button) 'custom-button 'button))

(defvar review/ui-buffer-names
  '((manager     . "*Initial Setup*")
    (custom      . "*Custom Variables and Faces*")
    (preferences . "*Preferences Data*")
    (featuers    . "*Setup Features*"))
  "Alist of buffer names for each component of the user interface.")

(defvar review/manager-buffer nil
  "Buffer managing the review steps, to be returned to after each step.")

(defun review/ui-bufname (key)
  (map-elt review/ui-buffer-names key))

(defun review/show-buffer-exclusively (buffer)
  "Switch to buffer and make it the only buffer in its frame."
  (pop-to-buffer-same-window buffer)
  (delete-other-windows (get-buffer-window buffer)))

(defun review/return-to-manager ()
  "Show manager buffer after completing a step. Returns manager buffer."
  (let ((manager (get-buffer-create (review/ui-bufname 'manager))))
    (review/show-buffer-exclusively manager)
    (setq review/manager-buffer manager)))

(define-minor-mode review/button-list-nav-mode
  "Minor mode for navigating a read-only buffer that acts as a button list."
  :init-value nil
  :lighter "But"
  :keymap (let ((keys (make-sparse-keymap)))
            (define-key keys [tab]         #'forward-button)
            (define-key keys [(shift tab)] #'backward-button)
            (define-key keys "n"           #'forward-button)
            (define-key keys "p"           #'backward-button)
            (define-key keys [down]        #'forward-button)
            (define-key keys [up]          #'backward-button)
            keys)
  (if review/button-list-nav-mode
      (setq buffer-read-only t)
    (setq buffer-read-only nil)))

(define-minor-mode review/form-nav-mode
  "Minor mode for navigating a read-only buffer holding a form."
  :init-value nil
  :lighter "Form"
  :keymap (let ((keys (make-sparse-keymap)))
            (define-key keys "n"           #'next-line)
            (define-key keys "p"           #'previous-line)
            (define-key keys "<"           #'beginning-of-buffer)
            (define-key keys ">"           #'end-of-buffer)
            (map-keymap (lambda (key binding)
                          (define-key keys (vector key) binding))
                        widget-keymap)
            keys)
  (if review/form-nav-mode
      (progn
        (setq buffer-read-only t)
        (message "TAB: next widget, S-TAB: prev widget, RET: push button, n: next line, p: prev line, </>: beg/end buffer"))
    (setq buffer-read-only nil)))

(define-minor-mode review/clean-step-mode
  "Minor mode to attach a cleanup action key (C-c C-c) in a review buffer."
  :init-value nil
  :lighter "C-cC-c"
  :keymap (make-sparse-keymap))


;;; Primary Review Tools

(defun review/delimit-preference-fields ()
  "Marks the next key-value pair in valid `set-preferences' as an editable field.
Returns the extent of the value field. Assumes that the current
buffer is read-only. Callers should catch movement errors that
arise from trying to `forward-sexp' at the end of a list; such an
error indicates that there are no more fields to delimit."
  (interactive)
  (let* ((key-end (progn (forward-sexp 1) (point)))
         (key-start (progn (backward-sexp 1) (point)))
         (val-end (progn (forward-sexp 2) (point)))
         (val-start (progn (backward-sexp 1) (point)))
         (key-name (intern
                    (buffer-substring-no-properties (1+ key-start) key-end))))
    (forward-sexp 1)                ; end of value
    (let ((inhibit-read-only t))
      (add-text-properties val-start val-end
                           (list 'field key-name
                                 'inhibit-read-only t
                                 'front-sticky t)))
    (cons val-start val-end)))

(defun review/prepare-preferences-data (&optional done-fn)
  "Identifies preferences data with overlays and makes values read-only fields.
DONE-FN is a function to be called when the user is finished
editing the preferences; it is called with no arguments with the
preferences buffer current. Starts with first `set-preferences'
form after point, and leaves point at the beginning of the first
field for user editing, returning point."
  (interactive)
  (search-forward "(set-preferences" nil t)
  (goto-char (match-beginning 0))
  (let* ((head (point))
         (tail (save-excursion (forward-sexp 1) (point)))
         (prev (save-excursion (forward-line -1) (point)))
         (mesg (make-overlay prev prev))
         (edit (make-overlay head tail))
         (cleanup (lambda (&rest _) (interactive)
                    (when done-fn (funcall done-fn))
                    (message "Finished Editing Preference Data"))))
    (overlay-put mesg 'before-string
                 (propertize
                  (concat "Each of the preferences below, marked by ==>, has a"
                          "key and a value.\nOnly the values are editable in this"
                          "buffer. Change the values as desired.\n\n")
                  'face '(:foreground "#8fb28f")))
    (overlay-put mesg 'after-string
                 (propertize
                  "When finished editing, click here or type C-c C-c\n"
                  'mouse-face '(:foreground "black" :background "gray80")
                  'follow-link t 'face '(:foreground "red")
                  'action cleanup
                  'category 'default-button 'button '(t) 'fontified t))
    (overlay-put edit 'line-prefix (propertize "==> "
                                               'face '(:foreground "#8fb28f")))
    (review/clean-step-mode 1)
    (define-key review/clean-step-mode-map "\C-c\C-c" cleanup)
    (down-list 1)
    (forward-sexp 1)
    (while (and (< (point) tail)
                (condition-case _
                    (review/delimit-preference-fields)
                  (error nil))))
    (goto-char head)
    (down-list 1)
    (forward-sexp 3)
    (backward-sexp 1)
    (point)))

(defun review-data-preferences (preferences-file &optional cleanup-action)
  "Tool to review and personalize preferences data used by this init-file."
  (interactive (list (let ((dir (thread-first "./init/data/"
                                  expand-file-name
                                  file-name-directory))
                           (prefs "preferences.el"))
                       (read-file-name "Preferences file: "
                                       dir prefs nil (when ivy-mode prefs)))))
  (let ((pref-buf (find-file-noselect preferences-file))
        (start-point 1))
    (with-current-buffer pref-buf
      (let ((inhibit-read-only t)) (remove-overlays))
      (setq buffer-read-only t)
      (when (fboundp 'fci-mode) (fci-mode -1))
      (goto-char (point-min))
      (setq start-point (review/prepare-preferences-data
                         (lambda ()
                           (when (buffer-modified-p) (save-buffer))
                           (kill-buffer)
                           (when cleanup-action (funcall cleanup-action))))))
    (review/show-buffer-exclusively pref-buf)
    (goto-char start-point)))

(defun review-custom-file-settings (customize-file &optional cleanup-action)
  "App to review and customize current settings in emacs-custom.el."
  (interactive (list (let ((dir (thread-first "./"
                                  expand-file-name
                                  file-name-directory))
                           (custom "emacs-custom.el"))
                       (read-file-name "Customizations file: "
                                       dir custom nil (when ivy-mode custom)))))
  (let ((custom-buf (get-buffer-create (review/ui-bufname 'custom)))
        (old-custom custom-file))
    (setq custom-file (expand-file-name customize-file)) ;; need dynamic binding
    (with-current-buffer custom-buf
      (setq buffer-read-only nil)
      (erase-buffer)
      (remove-overlays)
      (insert "Review saved customize settings. Use n/TAB/down and p/S-TAB/up to\n")
      (insert "navigate among the customize buttons and RET or mouse-1 to activate.\n")
      (insert "Current values are echoed in the minibuffer. Type 'q' when done.\n\n")
      (insert "In each customization buffer, set the values as desired and hit the\n")
      (insert "'Apply and Save' button. Then type 'q' to return to this buffer.\n\n"))
    (with-temp-buffer
      (insert-file-contents custom-file)
      (doforms form
        (with-current-buffer custom-buf
          (cond
           ((eq (car form) 'custom-set-variables)
            (insert "Variables:\n") ; show key and value in echo area too
            (dolist (setting (cdr form))
              (let ((var (caadr setting))
                    (val (cadadr setting)))
                (insert (format "  %-42s" var))
                (insert-button "[Customize This Variable]"
                               'follow-link t
                               'help-echo (format "%s: %S" var
                                                  (if (and (listp val)
                                                           (eq (car val) 'quote))
                                                      (cadr val)
                                                    val))
                               'action `(lambda (b) (customize-variable ',var)))
                (insert "\n"))))
           ((eq (car form) 'custom-set-faces)
            (insert "Faces:\n") ; show faces for buttons and in echo area too
            (dolist (setting (cdr form))
              (let ((var (caadr setting)))
                (insert (format "  %-42s" var))
                (insert-button "[Customize This Face]"
                               'face var
                               'follow-link t
                               'help-echo (propertize (symbol-name var) 'face var)
                               'action `(lambda (b) (customize-face ',var)))
                (insert "\n"))))
           (t nil))
          (insert "\n"))))
    (with-current-buffer custom-buf
      (view-mode 1)
      (review/button-list-nav-mode 1)
      (setq buffer-read-only t
            view-exit-action (lambda (&rest _)
                               (kill-buffer custom-buf)
                               (when cleanup-action (funcall cleanup-action))
                               (when old-custom
                                 (setq custom-file old-custom))
                               (message
                                "Finished Editing Customization Settings")))
      (goto-char (point-min)))
    (review/show-buffer-exclusively custom-buf)
    (forward-button 1)))

(defun review-tutorial-information (tutorial-file &optional cleanup-action)
  "View the help tutorial on the features of this init-file configuration."
  (interactive (list (let ((dir (thread-first "./init/Extras/"
                                  expand-file-name
                                  file-name-directory))
                           (tutf "tutorial.org"))
                       (read-file-name "Tutorial file: "
                                       dir tutf nil (when ivy-mode tutf)))))
  (let ((tut-buf (find-file-noselect tutorial-file)))
    (with-current-buffer tut-buf
      (view-mode 1)
      (setq buffer-read-only t
            view-exit-action (lambda (&rest _)
                               (kill-buffer tut-buf)
                               (when cleanup-action (funcall cleanup-action))
                               (message
                                "Finished Feature Review"))))
    (review/show-buffer-exclusively tut-buf)
    (search-forward-regexp "^\*" nil t)
    (ignore-errors (backward-char 1))))

(defvar no-user-init-startup-actions t
  "This is bound to inhibit startup actions when loading dot-emacs.el.")

(defvar review-packages-custom-needs
  '(ox ediff)
  "List of requires that must be loaded, above and beyond those in the
main config, for the customization review to change them")

;; This sets the state necessary for the init configuration to be
;; loadable at review time. The primary reason this is needed is for
;; the customization review so that the custom-set variables are
;; marked as customizable and have values and documentation. But it
;; helps also with the tutorial view and with general use pattern.
;; ATTN: This is a bit of a hack, and it sets various files directly,
;; especially emacs-custom.el in a non-DRY way. But it will do for now.
(defun review-load-current-config ()
  "Load the emacs init configuration in this directory without startup actions.
This ensures that the right packages are loaded so that the
customization step can get useful information for customizing the
variables. Without this, all the package-specific variables are
listed in a useless form within the customize buffers.

This assumes that the packages have been installed and that the
script has been evoked in the target directory which contains
both the elpa and init subdirectories. (This can be the user's
emacs directory or any other directory where the config has been
installed.)"
  (let* ((user-emacs-directory (file-name-directory (expand-file-name "./")))
         (custom-file (expand-file-name "emacs-custom.el" user-emacs-directory)))
    (setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
    (package-initialize)
    (load-file "init/dot-emacs.el")
    (dolist (package review-packages-custom-needs)
      (require package nil t))
    (map-put initial-frame-alist 'width 80) ; keep -Q frame size
    (map-put initial-frame-alist 'height 40))) 

;;; User-Interface Entry Point

(defun review-init-settings (&optional init-dir)
  "Start application to review and personalized init-file settings.
This should be called in the directory containing the `init' subdirectory,
which will typically be the user's .emacs.d directory."
  (interactive)
  (review-load-current-config)
  (let ((dir (or init-dir (thread-first "./init/" 
                            expand-file-name
                            file-name-directory)))
        (widget-push-button-prefix (if review/fancy-buttons-p "" "["))
        (widget-push-button-suffix (if review/fancy-buttons-p "" "]"))
        (next-step (lambda ()
                     (review/return-to-manager)
                     (widget-forward 1)))
        (cleanup (lambda (&rest ignore)
                   (interactive)
                   (message "Cleaning up...")
                   (kill-buffer)
                   (save-buffers-kill-terminal))))
    (review/return-to-manager)
    (kill-all-local-variables)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (remove-overlays)
      (local-set-key "\C-c\C-c" cleanup))
    (widget-insert
     (concat "Push each of the following buttons in turn to "
             "review and adjust emacs setup.\n"
             "When finished, click 'All Done' button below or type C-c C-c\n\n"))
    ;; Step 1
    (widget-insert "1. Review and adjust preferences data         ")
    (widget-create 'push-button
                   :button-face review/ui-wbutton-face
                   :notify (let ((prefs (thread-last dir
                                          (expand-file-name "data")
                                          (expand-file-name "preferences.el"))))
                             (lambda (&rest ignore)
                               (review-data-preferences prefs next-step)
                               (message "Step 1. Review Preferences")))
                   "Do This")
    (widget-insert "\n")
    ;; Step 2
    (widget-insert "2. Review and adjust customization settings   ")
    (widget-create 'push-button
                   :button-face review/ui-wbutton-face
                   :notify (let ((custom (thread-last dir
                                           (expand-file-name "..")
                                           (expand-file-name "emacs-custom.el"))))
                             (lambda (&rest ignore)
                               (review-custom-file-settings custom next-step)
                               (message "Step 2. Review Customization Settings")))
                   "Do This")
    (widget-insert "\n")
    ;; Step 3
    (widget-insert "3. Review init-file features (optional)       ")
    (widget-create 'push-button
                   :button-face review/ui-wbutton-face
                   :notify (let ((tut (thread-last dir
                                        (expand-file-name "Extras")
                                        (expand-file-name "tutorial.org"))))
                             (lambda (&rest ignore)
                               (review-tutorial-information tut next-step)
                               (message "Step 3. Review Init-file Features")))
                   "Do This")
    (widget-insert "\n")
    (widget-insert "\n\n")
    (widget-create 'push-button
                   :button-face review/ui-wbutton-face
                   :help-echo "Clean up the user interface and exit emacs"
                   :notify cleanup
                   "All Done")
    (widget-insert "\n")
    (widget-setup)
    (review/form-nav-mode 1)
    (goto-char (point-min))
    (widget-forward 1)))


;;; review.el ends here
