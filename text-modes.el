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
  (setq org-startup-folded 'content) ; nil also good
  (setq org-cycle-separator-lines 2)
  (setq org-log-into-drawer t)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (R . t)
     (ditaa . t)))
  (setq org-todo-keywords
        '((sequence "TODO" "WAIT" "DONE")))
  (setq org-agenda-custom-commands '(("A" "Agenda for two week span" agenda ""
                                      ((org-agenda-span 14) (org-agenda-start-day "-1mon")))))
  (setq org-tags-column -80)
  (copy-face 'org-todo 'org-wait-face) ; bug with string when doing org-write-agenda
  (set-face-foreground 'org-wait-face "lightgoldenrod2")
  (setq org-todo-keyword-faces '(("WAIT" . org-wait-face))) 
  )

(defun my-org-mode-hook ()
  (local-set-key "\C-c\C-x\M-k" 'org-cut-special)
  (local-set-key "\C-c\C-x\M-c" 'org-copy-special)
  (local-set-key "\C-c\M-c"     'org-edit-src-code) ; see which I like best
  (local-set-key "\C-c\M-s"     'org-edit-src-code) ; C-c ' taken by icicles
  (local-set-key [\C-\M-return] 'org-insert-subheading)
  (local-set-key [\C-\M-\S-return] 'org-insert-todo-subheading)
  )

(add-hook 'org-load-hook 'my-org-load-hook)
(add-hook 'org-mode-hook 'turn-on-font-lock)
(add-hook 'org-mode-hook 'my-org-mode-hook)

;(setq org-todo-keywords
;      '((sequence "TODO(t)" "WAIT(w@)" "DONE(d)")
;        (sequence "READING" "REVIEWING" "RESPONDED")
;        (sequence "PREPARED" "DISPATCHED" "PROCESSED" "FINISHED")
;        (sequence "FOUND" "TESTING" "FIXING" "FIXED")))


