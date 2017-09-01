;; -*- lexical-binding: t; -*-

;; ;; This is rough at the moment
;; (defun LaTeX-smart-paren (&optional prefix)
;;   "Rough paredit like behavior when opening delimiters are set to
;; `LaTeX-insert-left-brace' (and `LaTeX-electric-left-right-brace'
;; is non-nil). Move out of the current list unless there is no list
;; to move out of. In that case or with a prefix arg, just self
;; insert."
;;   (interactive "P")
;;   (if (or prefix)
;;       (self-insert-command (prefix-numeric-value prefix))
;;     (condition-case _
;;         (up-list)
;;       (error (self-insert-command 1)))))
;;  
;; ;; in LaTeX or TeX mode when ([{ have been set to LaTeX-insert-left-brace
;; ;; and LaTeX-electric-left-right-brace is non-nil. This gives
;; ;; nice behavior with pairs, like paredit but still can be improved.
;; (add-hook 'LaTeX-mode-hook
;;           (lambda ()
;;             (setq LaTeX-electric-left-right-brace t)
;;             (local-set-key ")" 'LaTeX-smart-paren)
;;             (local-set-key "]" 'LaTeX-smart-paren)
;;             (local-set-key "}" 'LaTeX-smart-paren)))

