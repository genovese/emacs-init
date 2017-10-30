;;; org-mode.el -- org mode and related tools -*- lexical-binding: t; -*-

(use-package org
  :config (progn
            ;; Need to clarify when org-load-hook is called
            ;; many things set here do not seem to "stick"
            ;; eg., org-file-apps, babel-do-load-languages, ...
            (add-my-hook org-load-hook
              (setq org-startup-folded 'content) ; nil also good
              (setq org-cycle-separator-lines 2)
              (setq org-log-into-drawer t)
              (setq org-special-ctrl-a/e t)
              (setq org-src-window-setup 'current-window) ; try 'other-frame
              (setq org-file-apps   ; not taking, perhaps move to org-mode-hook
                    '((auto-mode . emacs)
                      (directory . emacs)
                      (system . "open %s")
                      ("\\.mm\\'" . default)
                      ("\\.x?html?\\'" . system)
                      ("\\.pdf\\'" . system)
                      ("\\.\\(?:png\\|jpe?g\\|gif\\|tiff?\\|bmp\\)\\'" . system)
                      ("\\`\\(?:.*/\\)?[^.]+\\'" . emacs)))
              (org-babel-do-load-languages
               'org-babel-load-languages
               '((emacs-lisp . t)
                 (R . t)
                 (python . t)
                 (clojure . t)
                 (latex . t)
                 (lisp . t)
                 (haskell . t)
                 (C . t)
                 (ruby . t)
                 (org . t)
                 (css . t)
                 (sass . t)
                 (dot . t)
                 (java . t)
                 (ocaml . t)
                 (calc . t)
                 (ditaa . t)
                 (sql . t)
                 (asymptote . t)))
              (setq org-todo-keywords
                    '((sequence "TODO" "WAIT" "DONE")))
              (setq org-agenda-custom-commands '(("A" "Agenda for two week span" agenda ""
                                                  ((org-agenda-span 14) (org-agenda-start-day "-1mon")))))
              (setq-default org-tags-column -116) ;; -96 a good generic choice, -116 better in practice; convert to a function
              (copy-face 'org-todo 'org-wait-face) ; bug with string when doing org-write-agenda
              (set-face-foreground 'org-wait-face "lightgoldenrod2")
              (setq org-todo-keyword-faces '(("WAIT" . org-wait-face))))
            (add-my-hook org-mode-hook
              ;; ATTN: These are in load hook, which should be right
              ;; but they didn't work last time. Check proper placement
              (setq org-todo-keywords
                    '((sequence "TODO" "WAIT" "DONE")))
              (setq org-file-apps   ; not taking, perhaps move to org-mode-hook
                    '((auto-mode . emacs)
                      (directory . emacs)
                      (system . "open %s")
                      ("\\.mm\\'" . default)
                      ("\\.x?html?\\'" . system)
                      ("\\.pdf\\'" . system)
                      ("\\.\\(?:png\\|jpe?g\\|gif\\|tiff?\\|bmp\\)\\'" . system)
                      ("\\`\\(?:.*/\\)?[^.]+\\'" . emacs)))              (org-babel-do-load-languages
               'org-babel-load-languages
               '((emacs-lisp . t)
                 (R . t)
                 (python . t)
                 (clojure . t)
                 (latex . t)
                 (lisp . t)
                 (haskell . t)
                 (C . t)
                 (ruby . t)
                 (org . t)
                 (css . t)
                 (sass . t)
                 (dot . t)
                 (java . t)
                 (ocaml . t)
                 (calc . t)
                 (ditaa . t)
                 (sql . t)
                 (asymptote . t)))
              (copy-face 'org-todo 'org-wait-face) ; bug with string when doing org-write-agenda
              (set-face-foreground 'org-wait-face "lightgoldenrod2")
              (setq org-todo-keyword-faces '(("WAIT" . org-wait-face)))
              (setq org-tags-column -116) ;; -96 a good generic choice, -116 better in practice; convert to a function
              ;; END ATTN
              (setq org-archive-location "~/org/archive::")
              (setq org-hide-emphasis-markers t)
              (setq org-src-window-setup 'current-window)
              (setq org-babel-python-command "python3")
              (local-set-key "\C-c\C-x\C-i" 'org-display-inline-images)
              (local-set-key "\C-c\C-xt"    'org-clock-in)
              (local-set-key "\C-c\C-x\M-k" 'org-cut-special)
              (local-set-key "\C-c\C-x\M-c" 'org-copy-special)
              (local-set-key "\C-c\M-c"     'org-edit-src-code) ; see which I like best
              (local-set-key "\C-c\C-v\M-\C-h" 'org-babel-describe-bindings)
              (local-set-key "\C-c\C-vh" 'org-hide-block-all)
              (local-set-key [(control ?c) (control ?v) (control ?\ )] 'org-babel-mark-block)
              (local-set-key [(control ?c) (control ?')] 'org-edit-src-code) ; C-c ' taken by icicles
              (local-set-key [\C-\M-return] 'org-insert-subheading)
              (local-set-key [\C-\M-\S-return] 'org-insert-todo-subheading)
              (local-set-key "\C-\M-q" 'fill-region) ; possibly do some indenting here instead
              (local-unset-key "\M-h") ; new org version shadows backward-kill-word with mark-element
              (local-set-key [(control meta ?\ )] 'org-mark-element)) 

            (add-hook 'org-mode-hook 'turn-on-font-lock)))

(use-package org-ref
  :defer t)

(use-package markdown-mode
  :mode "\\.md\\'")


;;; org-mode.el ends here
