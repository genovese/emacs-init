;;; tex.el -- tex/latex tools -*- lexical-binding: t; -*-

(use-package auctex)

(use-package reftex
  :init (setq reftex-plug-into-AUCTeX t))

(use-package auctex-latexmk
  :defer t
  :config (progn
            (setq auctex-latexmk-inherit-TeX-PDF-mode t)
            (auctex-latexmk-setup)))


;;; tex.el ends here
