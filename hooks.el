;;; hooks.el -- hook settings especially for builtins  -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2017 Christopher R. Genovese, all rights reserved.
;; Author: Christopher Genovese <genovese@cmu.edu>
;; Version: 1.1.0

;;; Commentary:
;;
;;  Note: Hooks for programming languages or third-party tools are typically
;;  in their own component (in components/ not here).

;;; Code:

(add-hook 'minibuffer-exit-hook 'my/bury-completions t)

(add-hook 'eval-expression-minibuffer-setup-hook #'paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)

;(add-hook 'view-mode-hook 'visual-line-mode) ;viewing is easier with wrapping, no editing ambiguity

(defun isearch-yank-hook ()
  (make-local-variable 'isearch-mode-map)
  (define-key isearch-mode-map "\M-w" 'isearch-yank-lisp-symbol))
  ; consider making this \C-w  because it is easier to use

(add-my-hook view-mode-hook
  "Navigation consistent with my movement commands"
  (define-key view-mode-map "\C-\M-w" 'View-scroll-half-page-backward)
  (define-key view-mode-map "\C-\M-v" 'View-scroll-half-page-forward)
  (define-key view-mode-map "\M-v"    'end-of-buffer)
  (define-key view-mode-map "k"       'View-kill-and-leave)
  (define-key view-mode-map "l"       'View-leave)
  (unless (derived-mode-p 'org-mode)
    (visual-line-mode 1)))

(add-my-hook doc-view-mode-hook
  (auto-revert-mode 1))

(add-my-hook calc-mode-hook
  (local-set-key "\M-i" 'calc-pop)
  (local-set-key "\C-w" 'scroll-down-or-beg)
  (local-set-key "\M-w" 'beginning-of-buffer)
  (local-set-key "\M-k" 'calc-kill-region)
  (local-set-key "\M-c" 'calc-copy-region-as-kill)
  (local-set-key "\C-c\C-l" 'calc-copy-as-kill))


;;; hooks.el ends here
