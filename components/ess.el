;;; ess.el -- ESS and related tools -*- lexical-binding: t; -*-

(use-package ess
  :defer t
  :init (progn
          ;; smart commas only for now
          (setq ess-R-smart-operators t)) 
  :config (with-eval-after-load 'ess-r-mode
            (define-key ess-r-mode-map (kbd "C-h") nil)
            (define-key ess-r-mode-map (kbd "M-C-h") nil)
            (define-key ess-r-mode-map (kbd "C-c SPC") 'ess-mark-function-or-para)))

(use-package ess-smart-equals
  :init   (setq ess-smart-equals-extra-ops '(brace paren percent))
  :after  (:any ess-r-mode inferior-ess-r-mode ess-r-transcript-mode)
  :config (ess-smart-equals-activate))

(use-package julia-mode
  :mode "\\.jl\\'"
  :defer t)

;;; ess.el ends here
