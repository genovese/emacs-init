;; Try lsp-mode with javascript

;; First do
;;  npm install -g typescript-language-server
;;  npm install -g typescript  
;; Second, for tide, need to do some setup but later on that

(defun my/simple-js-hook ()
  (setq forward-sexp-function nil)
  (subword-mode 1)
  (if (fboundp #'js2-smart-pair-setup)
      (js2-smart-pair-setup)
    (setq skeleton-pair t)
    (local-set-key "(" 'skeleton-pair-insert-maybe)
    (local-set-key "[" 'skeleton-pair-insert-maybe)
    (local-set-key "{" 'skeleton-pair-insert-maybe)))

(use-package js2-mode
  :mode "\\.js\\'"
  :hook (js2-mode . my/simple-js-hook))

(use-package rjsx-mode
  :mode "\\.jsx?\\'"
  :hook (rjsx-mode . my/simple-js-hook)
  :defer t)

(use-package js2-refactor
  :defer t)

(use-package js2-closure
  :defer t)

;; This needs to move the lsp-mode-map binding into :config (eval-after...)
(use-package company
  :bind (([(control ?\')]    . company-complete)
         (:map company-active-map
               ("<tab>"      . company-complete-selection)
               ("C-d"              . company-show-doc-buffer)
               ([(control return)] . company-show-location)))
  :config (with-eval-after-load 'lsp-mode
            (bind-key "<tab>" #'company-indent-or-complete-common lsp-mode-map)))

(use-package company-box
  :hook (company-mode . company-box-mode))

(defun my/lsp-mode-hook ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook ((js2-mode  . lsp-deferred)
         (rjsx-mode . lsp-deferred)
         (lsp-mode  . my/lsp-mode-hook))
  :init (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
  :config (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode)
  :custom ;; see https://emacs-lsp.github.io/lsp-ui/#lsp-ui-sideline
  (lsp-ui-doc-position 'bottom)
  (lsp-ui-sideline-enable nil)
  (lsp-ui-sideline-show-hover nil))

(use-package lsp-treemacs
  :after lsp-mode)

(use-package lsp-ivy
  :after lsp-mode)

;; ATTN: put this in a hook
(setq company-backends (cons 'company-capf (cl-remove 'company-capf company-backends)))


;; No longer supported
;;(use-package company-lsp
;;  :config (push 'company-lsp company-backends))
