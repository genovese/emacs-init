;;; common-lisp.el -- Common Lisp editing and tools -*- lexical-binding: t; -*-

(add-hook 'lisp-mode-hook 'enable-paredit-mode t)
(when (< emacs-major-version 27)
  (add-hook 'lisp-mode-hook 'fci-mode t))

(add-my-hook lisp-mode-hook
  "Lisp with slime"
  :append t
  (slime-mode 1)
  (local-set-key [(control ?\;)] 'comment-indent-new-line)
  (local-set-key "\C-c\C-ds" 'slime-describe-symbol)
  (setq lisp-simple-loop-indentation  1
        lisp-loop-keyword-indentation 6
        lisp-loop-forms-indentation   6))

(use-package slime
  ;:ensure t
  :config
  (progn
    (setq inferior-lisp-program "/usr/local/bin/sbcl")
    (add-to-list 'load-path (f-join user-emacs-directory "elpa" "slime-current"))
    (require 'slime-autoloads)
    (add-to-list 'slime-contribs 'slime-fancy)
    (add-hook 'slime-repl-mode-hook 'enable-paredit-mode t)))




;;; common-lisp.el ends here
