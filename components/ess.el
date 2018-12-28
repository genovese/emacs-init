;;; ess.el -- ESS and related tools -*- lexical-binding: t; -*-

(use-package ess
  :defer t
  :init (progn
          (unless (boundp 'ess-S-assign)
            (defvar ess-S-assign "<- "
              "Deprecated variable holding default assignment operator string"))
          (setq ess-R-smart-operators t) ; smart commas only for now
          (autoload 'R-mode "ess-site"
            "Major mode for editing R source.  See `ess-mode' for more help."
            t))
  :config (progn
            (eval-after-load 'ess-smart-equals
              '(progn
                 (add-hook 'ess-mode-hook 'ess-smart-equals-mode)
                 (add-hook 'inferior-ess-mode-hook 'ess-smart-equals-mode)))
            (require 'ess-smart-equals nil t)
            (define-key ess-mode-map "\C-\M-h" nil)
            (define-key ess-mode-map (kbd "A-h") 'ess-mark-function-or-para)))


(use-package julia-mode
  :mode "\\.jl\\'")

;;; ess.el ends here
