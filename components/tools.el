;;; tools.el -- miscellaneous tools -*- lexical-binding: t; -*-

;;(use-package my-workgroups
;;  :config (progn
;;            (setq wg-prefix-key "\C-cw")
;;            (wg-set-prefix-key)
;;            (setq wg-default-buffer "*unix*")
;;            (setq wg-file "~/.emacs.d/saved-workgroups/u13a.el")
;;            (setq wg-morph-on nil)
;;            (wg-set-prefix-key)
;;            (global-set-key "\C-cwl" 'wg-echo-all-workgroups)))

(use-package calendar
  :init (progn
          (load-local 'calendar :mods)
          (use-package astronomy :config (load-local 'astronomy :mods)))
  :config (setq calendar-font-lock-keywords ; Change how weekends are highlighted 
                (cl-subst 'font-lock-keyword-face 'font-lock-comment-face
                       calendar-font-lock-keywords)))

(use-package undo-tree
  :defer t)

(use-package treemacs
  :defer t)

;;; tools.el ends here
