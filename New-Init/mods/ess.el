;;; ESS-mode for R, S, Julia

;; Formatting Style 

(setq ESS-crg-style '(CRG (ess-indent-offset . 4)
                          (ess-offset-arguments . open-delim)
                          (ess-offset-arguments-newline . open-delim)
                          (ess-offset-block . prev-call)
                          (ess-offset-continued . straight)
                          (ess-align-nested-calls "ifelse")
                          (ess-align-arguments-in-calls "function[ 	]*(")
                          (ess-align-continuations-in-calls . t)
                          (ess-align-blocks control-flow fun-decl)
                          (ess-indent-from-lhs arguments fun-decl-opening)
                          (ess-indent-from-chain-start . t)
                          (ess-indent-with-fancy-comments . nil)
                          (ess-auto-newline . t)
                          (ess-tab-always-indent . t)))

;; Hooks

(add-my-hook ess-mode-load-hook
  (if (not (assoc 'CRG ess-style-alist))
      (setq ess-style-alist (cons ESS-crg-style ess-style-alist)))
  (setq ess-default-style 'CRG)
  (setq ess-style 'CRG)
  (setq ess-auto-newline t)
  (setq-default ess-loop-timeout '1000000)
  (setq-default inferior-ess-help-command "help(\"%s\")\n")
  (setq ess-use-toolbar nil)
  (setq ess-fancy-comments nil)
  ;; Fix 06 Jun 2013 (ESS stomps on my help)
  ;; ess-mode-map maps "\M-\C-h" to ess-mark-function
  ;; which causes problems with my help setup
  ;; Unset the key in ess-mode-map and set C-cf to ess-mark-function
  (define-key ess-mode-map "\M-\C-h" nil)
  (define-key ess-mode-map "\C-cf" 'ess-mark-function))

(add-my-hook inferior-ess-mode-hook
  ;; Key bindings
  (local-set-key "\C-d" 'delete-char)
  (local-set-key "\C-a" 'my/comint-bol)
  (local-set-key "\C-c\M-p" 'comint-backward-matching-input)
  (local-set-key "\C-c\M-n" 'comint-forward-matching-input)
  ;; Comint configuration
  (add-hook 'comint-input-filter-functions 'ess-search-path-tracker nil t)
  (setq comint-move-point-for-output 'all) ; follow output after eval or entry
  (setq input-ring-size '1024)
  ;; Style Configuration (these three lines needed?)
  (setq ess-style 'CRG) 
  (setq ess-auto-newline t)
  (ess-set-style ess-style)
  ;; General ESS config
  (setq inferior-ess-help-command "help(\"%s\")\n"))

(add-my-hook ess-pre-run-hook
  (progn
    (remove-hook 'ess-mode-hook             'turn-on-font-lock)
    (remove-hook 'ess-transcript-mode-hoonk 'turn-on-font-lock)
    (remove-hook 'inferior-ess-mode-hook    'turn-on-font-lock)))

(add-my-hook ess-mode-hook
  (setq ess-style 'CRG)
  (setq ess-auto-newline t)
  (ess-set-style ess-style))

(add-my-hook ess-transcript-mode-hook
  (setq ess-style 'CRG)
  (setq ess-auto-newline t)
  (ess-set-style ess-style))

;; (setq ess-style-alist (cons ESS-crg-style ess-style-alist)) ;;  needed?


