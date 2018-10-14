;;; python.el -- python editing and ide tools -*- lexical-binding: t; -*-

(use-package pyvenv)

(use-package elpy
  :config (elpy-enable))

(add-my-hook python-mode-hook
  ;(local-set-key "\M-\C-a"  'beginning-of-python-def-or-class)
  (local-set-key [(control ?\;)] 'comment-indent-new-line))

(add-hook 'python-mode-hook 'fci-mode t)

(with-eval-after-load 'elpy
  (remove-hook 'elpy-modules 'elpy-module-flymake)
  (add-hook 'elpy-mode-hook 'flycheck-mode))

;(require 'py-autopep8)
;(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)



;;; python.el ends here
