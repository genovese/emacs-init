;;; version-control.el -- version-control tools -*- lexical-binding: t; -*-

(use-package magit
  :config (progn
            (defalias 'magit 'magit-status)
            ; (global-set-key "\M-gm" 'magit-status) ;; put in my/go-map
            (define-key magit-mode-map "\C-w" 'scroll-down-command)
            (define-key magit-mode-map "\M-w" 'beginning-of-buffer)
            (define-key magit-mode-map [?\A-w] 'magit-copy-section-value)
            (define-key magit-mode-map [?\s-W] 'magit-copy-buffer-revision)
            (define-key magit-mode-map [?\A-W] 'magit-copy-buffer-revision)
            (define-key magit-mode-map "\M-k" 'magit-copy-item-as-kill)
            (add-to-list 'same-window-regexps "\\`\\*magit: ")
            ;; NEW VERSION OF EMACS AND MAGIT  (VERSION DEPENDENT) OCT 2013
            (-when-let (client (executable-find "emacsclient"))
              (setq magit-emacsclient-executable client))))


;;; version-control.el ends here
