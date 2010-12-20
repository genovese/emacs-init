;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                           ;;
;;(@* "Text Modes")                                          ;;;;;;
;;                                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Generic Text Modes

(defun my-generic-text-modes-hook ()
  "Sets up environment useful for a variety of text editing modes.
   Add this to the corresponding hooks."
  (turn-on-flyspell)
  (turn-on-local-comment-auto-fill))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Org Mode

(defun my-org-load-hook ()
  (setq org-startup-folded nil)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (R . t)
     (ditaa . t)))
  )

(defun my-org-mode-hook ()
  (local-set-key "\C-c\C-x\M-k" 'org-cut-special)
  (local-set-key "\C-c\C-x\M-c" 'org-copy-special)
  (local-set-key "\C-c\M-c"     'org-edit-src-code) ; see which I like best
  (local-set-key "\C-c\M-s"     'org-edit-src-code) ; C-c ' taken by icicles
  )

(add-hook 'org-load-hook 'my-org-load-hook)
(add-hook 'org-mode-hook 'turn-on-font-lock)
(add-hook 'org-mode-hook 'my-org-mode-hook)

;(setq org-todo-keywords
;      '((sequence "TODO(t)" "WAIT(w@/!)" "DONE(d)")
;        (sequence "READING" "REVIEWING" "RESPONDED")
;        (sequence "PREPARED" "DISPATCHED" "PROCESSED" "FINISHED")
;        (sequence "FOUND" "TESTING" "FIXING" "FIXED")))


