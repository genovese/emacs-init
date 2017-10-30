;;; common-lisp.el -- Common Lisp editing and tools -*- lexical-binding: t; -*-

(add-hook 'lisp-mode-hook 'enable-paredit-mode t)
(add-hook 'lisp-mode-hook 'fci-mode t)

(add-my-hook lisp-mode-hook
  "Lisp with slime"
  :append t
  (local-set-key [(control ?\;)] 'comment-indent-new-line)
  (local-set-key "\C-c\C-ds" 'slime-describe-symbol)
  (local-set-key "\C-c\C-dh" 'cl-hyperspec-lookup)
  (setq lisp-simple-loop-indentation  1
        lisp-loop-keyword-indentation 6
        lisp-loop-forms-indentation   6))


;;; common-lisp.el ends here
