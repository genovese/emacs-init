;;; nxhtml.el -- nxHtml setup -*- lexical-binding: t; -*-

;;; Commentary:
;;
;;  nxHtml support seems to have disappeared, with parts of the package
;;  becoming part of emacs and the rest seemingly languishing.
;;  For that reason, this component is *not loaded* (ATTN!)
;;  But I collect my old nxhtml settings for future reference.


;;; Code:

;; ATTN: do not load this component

;(add-to-list 'auto-mode-alist '("\\.s?html?\\'" . nxhtml-mode)) ; ATTN: Where is nxhtml now?
; ATTN: where is nxhtml mode now?
;(fset 'xml-mode  'nxml-mode)    ;; should rhs be (symbol-function 'nxml-mode) ??
;(fset 'html-mode 'nxhtml-mode)

;(add-to-list 'magic-mode-alist '("<\\?xml " . nxml-mode))

(add-my-hook nxhtml-mode-hook
  (local-set-key "\M-m" 'nxml-mark-paragraph)
  (local-set-key "\M-h" 'backward-kill-word))


;;; nxhtml.el ends here
