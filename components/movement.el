;;; movement.el -- movement/navigation tools -*- lexical-binding: t; -*-

(use-package win-switch
  :config (progn
            (win-switch-authors-configuration)
            (setq win-switch-other-window-first nil)
            (setq win-switch-window-threshold 1)))

(use-package ace-jump-mode
  :bind ("C-." . ace-jump-mode)
  :config (bind-key "C-," 'ace-jump-mode-pop-mark)) ;;ATTN: maybe temporary

(use-package expand-region
  :bind (("C-="   . er/expand-region)
         ([?\A- ] . er/expand-region)
         ("C-+"   . er/contract-region)
         ([?\s- ] . er/contract-region)))

(use-package multiple-cursors
  :defer t)  ; ATTN: bind some hydras here to make this usable


;;; movement.el ends here
