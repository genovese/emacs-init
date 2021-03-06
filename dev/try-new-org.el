;; New org visual setup

;; Fonts

(defun my/org-font-settings ()
  (set-face-attribute 'default nil
                      :font "Fira Mono"
                      :height 140)
  (set-face-attribute 'fixed-pitch nil
                      :font "Fira Code" ;; Anonymous Pro
                      :height 140)
  (set-face-attribute 'variable-pitch nil
                      :font "Libre Franklin" ;; Fira Sans
                      :height 140
                      :weight 'regular))

;; Unicode Glyph Support

(defun dw/replace-unicode-font-mapping (block-name old-font new-font)
  (let* ((block-idx (cl-position-if
                         (lambda (i) (string-equal (car i) block-name))
                         unicode-fonts-block-font-mapping))
         (block-fonts (cadr (nth block-idx unicode-fonts-block-font-mapping)))
         (updated-block (cl-substitute new-font old-font block-fonts :test 'string-equal)))
    (setf (cdr (nth block-idx unicode-fonts-block-font-mapping))
          `(,updated-block))))

(use-package unicode-fonts
  :custom
  (unicode-fonts-skip-font-groups '(low-quality-glyphs))
  :config
  ;; Fix the font mappings to use the right emoji font
  (mapcar
   (lambda (block-name)
     (dw/replace-unicode-font-mapping block-name "Apple Color Emoji" "Noto Color Emoji"))
   '("Dingbats"
     "Emoticons"
     "Miscellaneous Symbols and Pictographs"
     "Transport and Map Symbols"))
  (unicode-fonts-setup))

(use-package unicode-fonts
  :straight t
  :if (not dw/is-termux)
  :custom
  (unicode-fonts-skip-font-groups '(low-quality-glyphs))
  :config
  ;; Fix the font mappings to use the right emoji font
  (mapcar
   (lambda (block-name)
     (dw/replace-unicode-font-mapping block-name "Apple Color Emoji" "Noto Color Emoji"))
   '("Dingbats"
     "Emoticons"
     "Miscellaneous Symbols and Pictographs"
     "Transport and Map Symbols"))
  (unicode-fonts-setup))

;; visual line mode and centering

(defun my/org-mode-visual-fill ()
  (interactive)
  (setq visual-fill-column-width       72
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

;; Define  org-writing-mode  -- modification to org mode for writing documents

(defvar org-writing-mode-hook nil
  "Hook run when starting org-writing-mode")
(defvar org-writing-mode--fixed-pitch-cookie nil)

(define-minor-mode org-writing-mode
  "A minor mode for use in org-mode specialized for written documents."
  nil nil nil
  (if org-writing-mode
      (progn
        (setq visual-fill-column-width       72
              visual-fill-column-center-text t)
        (visual-fill-column-mode 1)
        (setq org-writing-mode--fixed-pitch-cookie
              (face-remap-add-relative 'fixed-pitch :height 120))
        (run-hooks 'org-writing-mode-hook))
    (visual-fill-column-mode 0)
    (when org-writing-mode--fixed-pitch-cookie
      (face-remap-remove-relative org-writing-mode--fixed-pitch-cookie)
      (setq org-writing-mode--fixed-pitch-cookie nil))))

(use-package visual-fill-column
  :hook (org-writing-mode . my/org-writing-mode-hook))

(use-package adaptive-wrap
  :after org)

(defun my/org-display-setup ()
  "Setup line display and wrapping for org mode buffers."
  (interactive)
  (org-indent-mode)
  (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 1)
  (setq-local adaptive-wrap-extra-indent 4) ;; 2 also ok
  (adaptive-wrap-prefix-mode 1) ;; ATTN: Check it out
  (setq evil-auto-indent nil)
  (diminish org-indent-mode))

(use-package org
  :init (progn
          (add-my-hook org-load-hook
            (setq org-startup-folded 'content) ; nil also good
            (setq org-cycle-separator-lines 0)
            (setq org-log-into-drawer t)
            (setq org-special-ctrl-a/e 'reversed)
            (setq org-src-window-setup 'current-window) ; try 'other-frame
            (setq org-todo-keywords
                  '((sequence "TODO" "WAIT" "DONE")))
            (setq org-agenda-custom-commands '(("A" "Agenda for two week span" agenda ""
                                                ((org-agenda-span 14) (org-agenda-start-day "-1mon")))))
            (setq-default org-tags-column -116))  ;; -96 a good generic choice, -116 better in practice; convert to a function
          (add-my-hook org-mode-hook
            (setq org-file-apps
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
               (js . t)
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
            (local-set-key [(control meta ?\ )] 'org-mark-element)))
  :config (progn
            ;; Ensure that anything that should be fixed-pitch in Org files appears that way
            (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
            (set-face-attribute 'org-table nil  :inherit 'fixed-pitch)
            (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
            (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
            ;; ATTN: This does not always seem to work for some reason
            (set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
            (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
            (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
            (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
            (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)             
            (add-hook 'org-mode-hook 'turn-on-font-lock)))
