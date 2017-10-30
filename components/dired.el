;;; dired.el -- dired extensions and tools -*- lexical-binding: t; -*-

(use-package dired-aux)
(use-package dired-x)
(use-package dired+
  :init (setq diredp-hide-details-initially-flag nil)
  :config (progn
            (set-face-foreground 'diredp-dir-priv    "#33cc33") ; was "magenta3"
            (set-face-background 'diredp-dir-priv    nil)
            (set-face-foreground 'diredp-file-suffix "cornflower blue")
            (set-face-foreground 'diredp-file-name   "#E0CF9F")
            (set-face-foreground 'diredp-number      "gray60")
            (set-face-foreground 'diredp-dir-heading "Blue")
            (set-face-background 'diredp-dir-heading "bisque1")
            (set-face-background 'diredp-no-priv     "black")
            (set-face-foreground 'diredp-date-time   "#74749A9AF7F7")))
(use-package dired-filter)
(use-package dired-open)
(use-package dired-subtree) ;ATTN: set prefix command for subtree commands in C-/
(use-package dired-ranger)


;;; dired.el ends here
