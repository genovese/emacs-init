;;; emacs-lisp.el -- emacs-lisp setup and tools -*- lexical-binding: t; -*-

(use-package macrostep
  :commands macrostep-expand
  :config (progn
            (bind-key "C-c e" 'macrostep-expand lisp-interaction-mode-map)
            (bind-key "C-c e" 'macrostep-expand emacs-lisp-mode-map)))

(add-my-hook emacs-lisp-mode-hook
  (local-set-key "\C-c\C-e" 'eval-defun)
  (local-set-key "\C-c\C-o" 'outline-minor-mode)
  (local-set-key "\C-c\C-s" 'eval-last-sexp)
  (local-set-key [(control ?\;)] 'comment-indent-new-line)
  (defun emacs-lisp-outline-minor-setup ()
    (setq outline-regexp ";;; \\|;; \\|(....")
    (local-set-key "\C-c\C-m" outline-mode-prefix-map)
    (when (featurep 'org)
      (define-key outline-minor-mode-map [(control tab)] 'org-cycle)
      (define-key outline-minor-mode-map [(shift tab)]   'org-global-cycle))) 
  (make-local-variable 'outline-minor-mode-hook)
  (add-hook 'outline-minor-mode-hook 'emacs-lisp-outline-minor-setup))

(add-hook 'emacs-lisp-mode-hook 'highlight-attn-words t)
(add-hook 'emacs-lisp-mode-hook 'isearch-yank-hook)
(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode t)
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode t)
(add-hook 'emacs-lisp-mode-hook 'fci-mode t)
(add-hook 'emacs-lisp-mode-hook 'company-mode)

(font-lock-add-keywords 'emacs-lisp-mode
 '(("(\\(use-package\\)\\_>[ 	']*\\(\\(?:\\sw\\|\\s_\\)+\\)?"
    1 'font-lock-keyword-face)))


(add-hook 'inferior-emacs-lisp-mode-hook 'enable-paredit-mode)

(add-my-hook lisp-interaction-mode-hook
  "Elisp interaction as in *elisp* buffer."
  (local-set-key "\C-j"     'newline-and-indent)
  (local-set-key [C-return] 'eval-print-last-sexp)
  (local-set-key "\C-c\C-c" 'eval-print-last-sexp))
(add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)


;;; emacs-lisp.el ends here
