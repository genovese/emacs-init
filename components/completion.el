;;; completion.el -- completion tools -*- lexical-binding: t; -*-

;; Additional completion commands and tools

(defun ivy-yank-action (x)
  (kill-new x))

(defun counsel-find-file-read-only (&optional initial-input)
  "Forward to `find-file-read-only'.
When INITIAL-INPUT is non-nil, use it in the minibuffer during completion."
  (interactive)
  (ivy-read "Find file: " 'read-file-name-internal
            :matcher #'counsel--find-file-matcher
            :initial-input initial-input
            :action
            (lambda (x)
              (with-ivy-window
                (if (and counsel-find-file-speedup-remote
                         (file-remote-p ivy--directory))
                    (let ((find-file-hook nil))
                      (find-file-read-only (expand-file-name x ivy--directory)))
                  (find-file-read-only (expand-file-name x ivy--directory)))))
            :preselect (when counsel-find-file-at-point
                         (require 'ffap)
                         (let ((f (ffap-guesser)))
                           (when f (expand-file-name f))))
            :require-match 'confirm-after-completion
            :history 'file-name-history
            :keymap counsel-find-file-map
            :caller 'counsel-find-file-read-only))

(defun counsel-insert-file-action (x)
  "Find file X."
  (with-ivy-window
    (insert-file-contents (expand-file-name x ivy--directory))))

(defun counsel-insert-file (&optional initial-input)
  "Forward to `inisert-file'.
     When INITIAL-INPUT is non-nil, use it in the minibuffer during completion."
  (interactive)
  (ivy-read "Insert file: " #'read-file-name-internal
            :matcher #'counsel--find-file-matcher
            :initial-input initial-input
            :action #'counsel-insert-file-action
            :preselect (counsel--preselect-file)
            :require-match 'confirm-after-completion
            :history 'file-name-history
            :keymap counsel-find-file-map
            :caller 'counsel-insert-file))


;; Completion packages

(use-package company
  :config (progn
            (bind-key [(control ?\')] 'company-complete)
            (bind-key "C-d" 'company-show-doc-buffer company-active-map)
            (bind-key [(control return)] 'company-show-location company-active-map)))

(defun ivy-directory-from-last-kill ()
  "Replace current directory with copy of last kill when completing file names."
  ;; ATTN: this can be extended in a number of ways; e.g., cycle through directory stack or history
  (interactive)
  (let* ((last-kill (substring-no-properties (current-kill 0 t))))
    (when (f-directory? last-kill)
      (ivy--cd (file-name-as-directory last-kill)))))

(use-package all-the-icons)

(use-package ivy
  :bind (("C-c C-r" . ivy-resume)
         ("C-c 8" . counsel-unicode-char)
         ("C-c v" . ivy-push-view)
         ("C-c V" . ivy-pop-view)
         ("M-y" . counsel-yank-pop)
         ("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-x C-i" . counsel-insert-file))
  :config
  (progn
    (bind-keys :map ivy-minibuffer-map
               ("M-v")
               ("C-w" . ivy-scroll-down-command))
    (setq ivy-wrap t)
    (setq ivy-use-selectable-prompt t)
    (setq ivy-use-virtual-buffers t)
    (setq ivy-count-format "(%d/%d) ")
    (ivy-set-actions t
                     '(("y" ivy-yank-action "yank")))
    ;;(setq ivy-extra-directories '("../"))  ; without ./ just return opens parent
    (use-package counsel)
    (use-package swiper)
    ;;(bind-key "M-x" 'counsel-M-x)
    ;;(global-set-key (kbd "C-x C-f") 'counsel-find-file)
    ;;(global-set-key (kbd "C-x C-i") 'counsel-insert-file)
    (setq counsel-find-file-ignore-regexp "\\(?:\\`[#.]\\)\\|\\(?:[#~]\\'\\)")
    (define-key counsel-find-file-map (kbd "C-SPC") 'ivy-directory-from-last-kill)
    (use-package ivy-hydra)
    (ivy-mode 1)))

(use-package prescient
  :after (counsel ivy))

(use-package ivy-prescient
  :after (prescient ivy))

(use-package all-the-icons-ivy
  :init (add-hook 'after-init-hook 'all-the-icons-ivy-setup))

(use-package all-the-icons-ivy-rich
  :init (all-the-icons-ivy-rich-mode 1))

(use-package ivy-rich
  :after (ivy all-the-icons-ivy-rich)
  :hook (ivy-mode . ivy-rich-mode)
  :init (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))

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
