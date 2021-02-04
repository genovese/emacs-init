;;; clojure.el -- clojure editing, IDE, and tools -*- lexical-binding: t; -*-

(use-package clojure-mode
  ;:commands clojure-mode
  :demand t
  :config (progn
            (add-my-hook clojure-mode-hook
              (local-set-key "\M-[" 'paredit-wrap-square)
              (local-set-key "\M-{" 'paredit-wrap-curly)
              (local-set-key "\C-ch" 'hs-toggle-hiding)
              (local-set-key "\C-c " 'clojure-cheatsheet)
              (local-set-key "\C-c\C-y" 'my/clojure-snippet-prefix)
              (clj-refactor-mode 1)
              (yas-minor-mode 1)
              (cljr-add-keybindings-with-prefix "C-c r")
              (local-set-key "\C-c\C-r" 'cljr-helm)
              (local-set-key "\C-c\C-f" 'find-file-in-project))

            (add-to-list 'same-window-regexps "\\`\\*cider ")

            (add-hook 'clojure-mode-hook 'company-mode)
            (add-hook 'clojure-mode-hook 'eldoc-mode)
            (add-hook 'clojure-mode-hook 'hs-minor-mode)
            (add-hook 'clojure-mode-hook 'subword-mode)
            (add-hook 'clojure-mode-hook 'column-number-mode)
            ;; Done elsewhere, but doesn't hurt to do here either
            (add-hook 'clojure-mode-hook 'enable-paredit-mode)
            (when (< emacs-major-version 27)
              (add-hook 'clojure-mode-hook 'fci-mode))
            (add-to-list 'auto-mode-alist '("\\.cljx?\\'"  . clojure-mode))
            (add-to-list 'auto-mode-alist '("\\.cljs\\'"  . clojurescript-mode))
            (add-to-list 'auto-mode-alist '("\\.cljc\\'"  . clojurec-mode))

            (define-clojure-indent
              ;; midje
              (facts 1)
              (fact 1))))

(use-package cider
  :commands (cider-jack-in cider-jack-in-clj cider-jack-in-cljs
             cider-jack-in-clj&cljs)
  :config (progn
            (add-my-hook cider-repl-mode-hook
              (enable-paredit-mode)
              (subword-mode)
              (local-set-key "\C-cn" 'cider-browse-ns)
              (local-set-key "\C-cN" 'cider-browse-ns-all)
              (local-set-key "\M-{" 'paredit-wrap-curly)
              (local-set-key "\M-[" 'paredit-wrap-square)
              (local-set-key "\C-c " 'clojure-cheatsheet)
              (setq cider-popup-stacktraces t)
              (setq cider-repl-popup-stacktraces nil))
            (define-key cider-docview-mode-map "d" #'cider-doc) ; ATTN right place?
            (add-hook 'cider-repl-mode-hook 'company-mode)
            (add-hook 'cider-repl-mode-hook 'eldoc-mode)
            (add-hook 'cider-interaction-mode-hook 'company-mode)
            (add-hook 'cider-interaction-mode-hook 'eldoc-mode)
            (add-hook 'cider-mode-hook 'company-mode)
            (add-hook 'cider-mode-hook 'eldoc-mode)))

(use-package clojure-cheatsheet
  :commands clojure-cheatsheet)

(use-package clj-refactor
  :commands clj-refactor-mode)

;; Check on squiggly-clojure
;;(use-package flycheck-clojure
;;  :defer t)


;;; clojure.el ends here
