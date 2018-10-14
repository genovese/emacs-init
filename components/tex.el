;;; tex.el -- tex/latex tools -*- lexical-binding: t; -*-

(use-package auctex
  :config (progn
            (setq TeX-parse-self t
                  TeX-auto-save t
                  TeX-auto-untabify t)
            ;; Not defined until later apparently
            ;;(add-to-list 'TeX-style-path "./tex/style" t)
            ;;(add-to-list 'TeX-style-path "./tex/auto" t)
            ;;(add-to-list 'TeX-style-path "../tex/style" t)
            ;;(add-to-list 'TeX-style-path "../tex/auto" t)
            ;;(add-to-list 'TeX-style-path "~/tex/style" t)
            ;;(add-to-list 'TeX-style-path "~/tex/auto" t)
            ;; set TeX-style-private TeX-auto-private etc.   
            ))

(use-package reftex
  :init (setq reftex-plug-into-AUCTeX t))

(use-package auctex-latexmk
  :defer t
  :config (progn
            (setq auctex-latexmk-inherit-TeX-PDF-mode t)
            (auctex-latexmk-setup)))


;;; tex.el ends here
