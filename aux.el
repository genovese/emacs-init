;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                           ;;
;;(@* "Other Modes and Features")                            ;;;;;;
;;                                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Formats

(defvar my-date-format "%Y %b %d %a"
  "My favorite date format; see format-time-string for details.")
(setq header-date-format "%Y %b %d %a %H:%M:%S")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; IBuffer

(defun ibuffer-list-buffers-select (&optional noselect)
  "Display a list of buffers, in another window.
If optional argument FILES-ONLY is non-nil, then add a filter for
buffers which are visiting a file."
  (interactive "P")
  (let ((starting-buffer (buffer-name (current-buffer))))
    (ibuffer t nil nil noselect)
    (if (and (null noselect)
             ibuffer-start-at-most-recent
             (eq major-mode 'ibuffer-mode))  ; only do this in the ibuffer window
        (ibuffer-jump-to-buffer starting-buffer))))

(defun ibuffer-match-my-files (label &optional subdir pattern)
  "Create ibuffer filter groups associated with specified file or
subdirectory names in my home directory. Files in the group are
those that match the path, which can be only partially specified.
LABEL is the ibuffer name associated with the filter group.
SUBDIR is a subdirectory path that restricts the files selected
if non-nil. It should not start with / in general.
PATTERN is a regexp that limits the files considered within
the subdirectory.
   The return value of the function is a list containing the
list entry specifying the filter group, so that it can be included
in the filter group list with ,@ and will not affect the resulting
list if no such files exist."
  (let* ((subdir (or subdir ""))
         (nodes  (mapcar (lambda (x) `(filename . ,x))
                         (directory-files (concat my-home-dir subdir) t pattern))))
    (if (= (length nodes) 0) nil
      (list (cons label (if (= (length nodes) 1) nodes (list (cons 'or nodes))))))))

(defvar ibuffer-start-at-most-recent nil
  "If non-nil, \\[ibuffer-list-buffers-select] will, if selecting the
ibuffer listing window, put the cursor on the line corresponding to the
most recent buffer. Otherwise, the cursor is positioned on the top
buffer line in the ibuffer listing.")

(defvar my-ibuffer-loaded nil
  "Variable used to control loading of ibuffer customized setup.
Should always be defined as NIL initially.")

(defun my-ibuffer-mode-hook () 
  ;; Setup definitions and filter groups.
  ;; To avoid worrying about requires, handle the setup
  ;; in the hook but only on the first time called.
  (do-only-once my-ibuffer-loaded
    (require 'ibuf-ext) ;; Only required as need and when ibuffer exists
    (define-ibuffer-filter mode-nostar
      "Toggle current view to buffers with major mode QUALIFIER
     and no star at beginning of the buffer name."
      (:description "major mode with no star in name"
                    :reader
                    (intern
                     (completing-read "Filter by major mode: " obarray
                                      #'(lambda (e)
                                          (string-match "-mode$"
                                                        (symbol-name e)))
                                      t
                                      (let ((buf (ibuffer-current-buffer)))
                                        (if (and buf (buffer-live-p buf))
                                            (with-current-buffer buf
                                              (symbol-name major-mode))
                                          "")))))
      (and
       (not (string-match "^\\s-*\\*" (buffer-name buf)))
       (eq qualifier (with-current-buffer buf major-mode))))
    (setq ibuffer-saved-filter-groups
          `(("default"
             ,@(ibuffer-match-my-files "Teaching" "class/" "^s")
             ("Documents" (or (mode . plain-tex-mode)
                              (mode . latex-mode)
                              (mode . bibtex-mode)
                              (mode . amstex-mode)
                              (mode-nostar . org-mode)
                              (mode-nostar . fundamental-mode)
                              (mode . nxml-mode)
                              (mode . nxhtml-mode)))
             ,@(ibuffer-match-my-files "Android" "Documents/" "^Eclipse")
             ,@(ibuffer-match-my-files "Arduino" "Programming/" "^Arduino")
             ("Code" (or (mode . cc-mode)
                         (mode . python-mode)
                         (mode . java-mode)
                         (mode . ess-mode)
                         (mode . js2-mode)
                         (mode . ruby-mode)
                         (mode . haskell-mode)
                         (mode . perl-mode)
                         (mode . tuareg-mode)
                         (mode . R-mode)
                         (mode . r-mode)
                         (mode . R-transcript-mode)
                         (mode . r-transcript-mode)
                         (mode . sh-mode)
                         (filename . "[Mm]akefile")))
             ("Emacs" (mode-nostar . emacs-lisp-mode))
             ("Shells" (or (mode . shell-mode)
                           (mode . comint-mode)
                           (mode . inferior-ess-mode)
                           (mode . eshell-mode)
                           (mode . lisp-interaction-mode)
                           (name . "^\\*[iI][pP]ython\\*")
                           (mode . slime-repl-mode)))
             ("Dirs"  (mode . dired-mode))
             ("Help"  (or (name . "^\\*Help\\*")
                          (name . "^\\*help\\[R\\]")
                          (name . "^\\*Man\\s-")
                          (name . "^\\*Apropos\\*")
                          (mode . help-mode)
                          (mode . Man-mode)
                          (mode . Info-mode)))
             ("Utility" (name . "^\\*.*\\*\\(<[0-9]+>\\)?$"))
             ("Other" (name . "^\\S-")))
                  ("all"
                   ("All" (name . ".")))
                  )))
  ;; Normal operational settings.
  (setq ibuffer-start-at-most-recent t)
  (ibuffer-auto-mode 1)
  (local-set-key "\M-ss"   'ibuffer-do-isearch)
  (local-set-key "\M-sr"   'ibuffer-do-isearch-regexp)
  (local-set-key "\M-\C-f" 'ibuffer-forward-filter-group)
  (local-set-key "\M-\C-b" 'ibuffer-backward-filter-group)
  (local-set-key "/F"      (lambda () (interactive) (ibuffer-filter-by-filename ".*")))
  (setq ibuffer-show-empty-filter-groups nil)
  (ibuffer-switch-to-saved-filter-groups "default"))

(add-hook 'ibuffer-mode-hook 'my-ibuffer-mode-hook)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Intelligent Copy and Kill

  ;; ATTN: not done yet

(defvar my-noregion-copykill-thing-default-order
  '((url . nearest)
    (word . nearest)
    (line . at)
    (page . at)
    (sentence . at)
    (paragraph . at)
    (sexp . nearest)
    (defun . nearest))
  "A list defining the order and scope of things
that are captured by smart copy and kill operations.
Each element of the list is a cons cell of the form
(THING . SCOPE) where THING is a symbol that corresponds
to a capturable thing in thingatpt/thingatpoint+ and
SCOPE is either the symbol at or nearest, corresponding
to the functions thing-at-point or thing-nearest-point.
The nearest scope only works if thingatpt+ is available,
otherwise it is transformed to at. The default")

 
(defvar my-noregion-copykill-thing-order-alist
  '((emacs-lisp-mode .
      ((sexp . nearest)
       (defun . nearest)
       (line . at)
       (page . at)
       (word . nearest)
       (sentence . at))))
  "")


