;;; ess.el -- ESS and related tools -*- lexical-binding: t; -*-

(use-package ess
  :defer t
  :init (progn
          (setq ess-R-smart-operators t) ; smart commas only for now
          (autoload 'R-mode "ess-site"
            "Major mode for editing R source.  See `ess-mode' for more help."
            t))
  :config (progn
            (eval-after-load 'ess-smart-equals
              '(progn
                 (add-hook 'ess-mode-hook 'ess-smart-equals-mode)
                 (add-hook 'inferior-ess-mode-hook 'ess-smart-equals-mode)))
            (require 'ess-smart-equals nil t)))


(use-package julia-mode
  :mode "\\.jl\\'")

;;; ess.el ends here
