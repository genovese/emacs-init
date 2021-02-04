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
            ;(define-key magit-file-mode-map "\C-xg" nil)
            (add-to-list 'same-window-regexps "\\`\\*magit: ")
            (-when-let (client (executable-find "emacsclient"))
              (setq magit-emacsclient-executable client))
            (magit-define-popup-switch 'magit-log-popup ?n
              "Show changed file name and status" "--name-status")
            ;; (add-to-list 'magit-log-arguments "--color") ;; deprecated
            (magit-define-popup-action 'magit-file-popup ?m
              "Magit Status" 'magit-status nil t)
            (setq-default magit-diff-refine-hunk t)
            (setq-default magit-log-margin
                          '(nil "%Y-%b-%d %H%M " magit-log-margin-width t 24))))


;;; version-control.el ends here
