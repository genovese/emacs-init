;;; display.el -- control emacs's information display -*- lexical-binding: t; -*-

(use-package uniquify
  :init (setq uniquify-buffer-name-style 'post-forward 
              uniquify-separator " in "
              uniquify-after-kill-buffer-p t))

(use-package fill-column-indicator
  :init (setq-default fci-rule-column 80))

(use-package which-func
  :config (setq which-func-modes 
              '(emacs-lisp-mode c-mode c++-mode python-mode
                perl-mode cperl-mode makefile-mode sh-mode
                fortran-mode f90-mode ada-mode)))

(use-package smart-mode-line
  :config (setq sml/theme 'respectful))


;;; display.el ends here
