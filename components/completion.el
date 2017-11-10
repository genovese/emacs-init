;;; completion.el -- completion tools -*- lexical-binding: t; -*-

(use-package company
  :config (progn
            (bind-key [(control ?\')] 'company-complete)
            (bind-key "C-d" 'company-show-doc-buffer company-active-map)
            (bind-key [(control return)] 'company-show-location company-active-map)))

(use-package ivy
  :config
  (progn
    (bind-key "M-x" 'counsel-M-x)
    (bind-keys :map ivy-minibuffer-map
               ("M-v")
               ("C-w" . ivy-scroll-down-command))
    (setq ivy-wrap t)
    (global-set-key (kbd "C-x C-f") 'counsel-find-file)
    (setq counsel-find-file-ignore-regexp "\\(?:\\`[#.]\\)\\|\\(?:[#~]\\'\\)")
    (ivy-set-actions t
                     '(("y" ivy-yank-action "yank")))
    (use-package ivy-hydra)
    (ivy-mode 1)))

(use-package helm
  :config (progn
            (setq helm-split-window-in-side-p       t
                  helm-move-to-line-cycle-in-source t
                  helm-recentf-fuzzy-match          t
                  helm-buffers-fuzzy-matching       t)
            (bind-key "C-v"     'helm-next-page           helm-map)
            (bind-key "C-w"     'helm-previous-page       helm-map)
            (bind-key "M-C-n"   'helm-next-source         helm-map)
            (bind-key "M-C-p"   'helm-previous-source     helm-map)
            (bind-key "M-v"     'helm-end-of-buffer       helm-map)
            (bind-key "M-w"     'helm-beginning-of-buffer helm-map)
            (bind-key "M-k"     'helm-yank-text-at-point  helm-map)
            (cl-loop for n from 0 to 9 do
                     (bind-key (vector (list 'control (+ ?0 n)))
                               `(lambda ()
                                  (interactive)
                                  (helm-select-nth-action ,n))
                               helm-map))
            ;; External helm access bindings
            (bind-key "C-x r h" 'helm-register)
            (bind-key "M-C-y"   'helm-show-kill-ring)
            (bind-key "A-y"     'append-next-kill)))

(use-package smex
  :config (progn
            (bind-key "M-X" 'smex-major-mode-commands)
            (setq smex-save-file (locate-user-emacs-file "smex-items"))
            (advice-add #'smex-prepare-ido-bindings :override
              (lambda  ()
                (let ((m (make-keymap)))
                  (define-key m (kbd "f") 'smex-describe-function)
                  (define-key m (kbd "w") 'smex-where-is)
                  (define-key ido-completion-map (kbd "M-C-h") m))
                (define-key ido-completion-map (kbd "TAB") 'minibuffer-complete)
                (define-key ido-completion-map (kbd "M-.") 'smex-find-function)
                (define-key ido-completion-map (kbd "C-a") 'move-beginning-of-line))
              '((name . my-smex-ido-help)))
            (smex-initialize)))

(use-package yasnippet
  :config (setq yas-prompt-functions '(yas/ido-prompt
                                       yas/dropdown-prompt
                                       yas/completing-prompt
                                       yas/x-prompt
                                       yas/no-prompt)))

;;; completion.el ends here
