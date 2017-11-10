;;; display.el -- control emacs's information display -*- lexical-binding: t; -*-

(use-package uniquify
  :init (setq uniquify-buffer-name-style 'post-forward 
              uniquify-separator " in "
              uniquify-after-kill-buffer-p t))

(use-package fill-column-indicator
  :init (setq-default fci-rule-column 80))

(use-package which-func
  :config (setq which-func-modes ;; ATTN: maybe set this back to t
              '(emacs-lisp-mode c-mode c++-mode python-mode
                perl-mode cperl-mode makefile-mode sh-mode
                fortran-mode f90-mode ada-mode)))

(use-package which-key ;; see https://github.com/justbur/emacs-which-key
  :config (progn 
            (setq which-key-idle-delay 1.5)
            (which-key-mode 1)))

(use-package smart-mode-line ;; ATTN: plan to switch to spaceline
  :config (setq sml/theme 'respectful))


;;; display.el ends here
