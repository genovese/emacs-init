;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; IBuffer

(add-to-list 'same-window-buffer-names "*Ibuffer*")

(defun ibuffer-list-buffers-select (&optional noselect)
  "Display a list of buffers, in another window.
If optional argument NOSELECT is non-nil, do not select
that buffer, unless it is already the current buffer."
  (interactive "P")
  (let ((in-ibuffer (derived-mode-p 'ibuffer-mode))
        (starting-buffer (buffer-name)))
    (if in-ibuffer
        (ibuffer-update nil)
      (ibuffer t nil nil noselect)
      (if (and (null noselect)
               ibuffer-start-at-most-recent
               (derived-mode-p 'ibuffer-mode)) ; only do this in ibuffer window
          (ibuffer-jump-to-buffer starting-buffer)))))

;; ibuffer movement
;; Based on Irreal's http://irreal.org/blog/?p=3544 modification
;; of Magnar's code to do the same thing for dired, see
;; http://whattheemacsd.com/setup-dired.el-02.html.
;; Modified by crg to allow movement to groups and to fix 
(defun ibuffer-move-to-top (&optional to-group)
  (interactive "P")
  (beginning-of-buffer)
  (if to-group (ibuffer-forward-filter-group) (next-line 3))
  (forward-line 0))

(defun ibuffer-move-to-bottom (&optional to-group)
  (interactive "P")
  (end-of-buffer)
  (if to-group (ibuffer-backward-filter-group) (next-line -2))
  (forward-line 0))

(defvar ibuffer-start-at-most-recent nil
  "If non-nil, \\[ibuffer-list-buffers-select] will, if selecting the
ibuffer listing window, put the cursor on the line corresponding to the
most recent buffer. Otherwise, the cursor is positioned on the top
buffer line in the ibuffer listing.")

(with-eval-after-load 'ibuffer
  (setq ibuffer-saved-filter-groups
        `(("default"
           ("Org"
            (directory . ,(concat user-home-directory "org/")))
           ("Teaching"
            (directory . ,(concat user-home-directory "class/")))             
           ("Documents" (or (mode . plain-tex-mode)
                            (mode . latex-mode)
                            (mode . bibtex-mode)
                            (mode . amstex-mode)
                            (and (mode . org-mode)
                                 (not starred-name))
                            (and (mode . fundamental-mode)
                                 (not starred-name))))
           ("Dirs"  (mode . dired-mode))
           ("Code" (or (mode . c-mode)
                       (mode . python-mode)
                       (mode . java-mode)
                       (mode . jde-mode)
                       (mode . ess-mode)
                       (mode . ruby-mode)
                       (mode . clojure-mode)
                       (mode . clojurescript-mode)
                       (mode . lisp-mode)
                       (mode . js2-mode)
                       (mode . rjsx-mode)
                       (mode . javascript-mode)
                       (mode . haskell-mode)
                       (mode . perl-mode)
                       (mode . cperl-mode)
                       (mode . tuareg-mode)
                       (mode . c++-mode)
                       (mode . sh-mode)
                       (mode . php-mode)
                       (mode . objc-mode)
                       (mode . processing-mode)
                       (mode . arduino-mode)
                       (mode . sql-mode)
                       (mode . ps-mode)
                       (mode . R-mode)
                       (mode . r-mode)
                       (mode . R-transcript-mode)
                       (mode . r-transcript-mode)
                       (basename . "[Mm]akefile")))
           ("Web" (or (mode . html-mode)
                      (mode . sass-mode)
                      (mode . scss-mode)
                      (mode . css-mode)
                      (mode . js-mode)
                      (mode . nxml-mode)
                      (mode . nxhtml-mode)))
           ("Emacs" (or (and (mode . emacs-lisp-mode)
                             (not starred-name))
                        (name . "^\\*elisp\\*")))
           ("Arduino"
            (directory . ,(concat user-home-directory "Programming/Arduino/")))
           ("REPLs"  (or (mode . shell-mode)
                         (mode . comint-mode)
                         (mode . inferior-ess-mode)
                         (mode . eshell-mode)
                         (mode . lisp-interaction-mode)
                         (mode . cider-repl-mode)
                         (name . "^\\*[iI][pP]ython\\*")
                         (mode . slime-repl-mode)))
           ("Help"  (or (name . "^\\*Help\\*")
                        (name . "^\\*help\\[R\\]")
                        (name . "^\\*Man\\s-")
                        (name . "^\\*Apropos\\*")
                        (mode . help-mode)
                        (mode . helpful-mode)
                        (mode . Man-mode)
                        (mode . Info-mode)))
           ("Magit" (name . "^\\*magit"))             
           ("Customize"  (name . "^\\*Customize"))             
           ("Tools"  (or (name . "^\\*Calendar\\*")
                         (name . "^\\*Messages\\*")
                         (name . "^\\*Backtrace\\*")
                         (name . "^\\*Customize")
                         (name . "^\\*ESS\\*")
                         (name . "^\\*Apropos\\*")
                         (mode . help-mode)
                         (mode . Man-mode)
                         (mode . Info-mode)))
           ("Output" (name . "^\\*.*\\*\\(<[0-9]+>\\)?$"))
           ("Misc" (name . "^\\S-")))
          ("all"
           ("All" (name . ".")))))
  (define-key ibuffer-mode-map
    (vector 'remap 'end-of-buffer) 'ibuffer-move-to-bottom)
  (define-key ibuffer-mode-map
    (vector 'remap 'beginning-of-buffer) 'ibuffer-move-to-top))

(add-my-hook ibuffer-mode-hook
  "Ibuffer configuration."
  ;; To avoid worrying about requires, handle the setup
  ;; in the hook but only on the first time called.

  ;; Normal operational settings.
  (setq ibuffer-start-at-most-recent t)
  (ibuffer-auto-mode 1)
  (local-set-key "\M-ss"   'ibuffer-do-isearch)
  (local-set-key "\M-sr"   'ibuffer-do-isearch-regexp)
  (local-set-key "\M-\C-f" 'ibuffer-forward-filter-group)
  (local-set-key "\M-\C-b" 'ibuffer-backward-filter-group)
  (local-set-key "/\C-m"   'ibuffer-filter-by-mode) 
  (local-set-key "U"       'ibuffer-unmark-all)
  (local-set-key "X"       'ibuffer-do-replace-regexp)
  (local-set-key "r"       'ibuffer-do-toggle-read-only)
  (local-set-key "/F"      (lambda () (interactive) (ibuffer-filter-by-filename ".*")))
  (local-set-key "/-"      'ibuffer-filter-disable)
  (local-set-key "//"      'ibuffer-filter-by-directory)
  (local-set-key "/\t"     'ibuffer-filter-chosen-by-completion)
  (setq ibuffer-show-empty-filter-groups nil)
  (ibuffer-switch-to-saved-filter-groups "default"))
