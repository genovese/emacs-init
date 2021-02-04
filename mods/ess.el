;;; ESS-mode for R, S, Julia

;; Brace Handling

;; ATTN: this is not used yet
(defcustom ess-electric-brace-newline '(after . before)
  "Non-nil means automatically newline before and after braces
inserted in S code."
  :type '(cons (choice (const before) (const before) (const t))
               (choice (const before) (const before) (const t)))
  :group 'ess-edit)

(defun ess-electric-brace (&optional literal)
  "Inserts properly indented and spaced brace pair."
  (interactive "P")
  (if literal
      (self-insert-command (if (integerp literal) literal 1))
    (when (not (eq (char-syntax (char-before)) ?\ ))
      (insert " "))
    (let ((pt (point))
          (skeleton-pair t)
          (skeleton-pair-alist '((?\{ "\n" > _ "\n" > ?\}))))
      (skeleton-pair-insert-maybe nil)
      (goto-char pt)
      (ess-indent-exp)
      (forward-char 2)
      (ess-indent-command))))

;; Formatting Style 
;;
;; CRG and OWN (set in emacs-custom.el) are set to be equivalent initially.
;; Modify OWN for in-session alteration of the indentation style.

(setq ess-crg-style '(CRG (ess-indent-offset . 4)
                          (ess-offset-arguments . open-delim)
                          (ess-offset-arguments-newline . prev-call)
                          (ess-offset-block . open-delim)
                          (ess-offset-continued . straight)
                          (ess-align-nested-calls "ifelse")
                          (ess-align-arguments-in-calls "function[ 	]*(")
                          (ess-align-continuations-in-calls . t)
                          (ess-align-blocks control-flow)
                          (ess-indent-from-lhs arguments fun-decl-opening)
                          (ess-indent-from-chain-start . nil)
                          (ess-indent-with-fancy-comments . nil)
                          (ess-auto-newline . t)
                          (ess-tab-always-indent . t)))

(defun ess-add-crg-style (&optional default)
  "Add CRG style to `ess-style-alist' and if `default' make it the default style."
  (unless (assoc 'CRG ess-style-alist)
    (push ess-crg-style ess-style-alist))
  (when default
    (setq ess-default-style 'CRG))
  'CRG)


;; Hooks

(add-my-hook ess-mode-hook
  ;; ESS style configuration
  (ess-add-crg-style :default)
  (ess-set-style 'CRG)
  ;; ESS configuration
  (setq-default ess-use-ido nil)        ; prefer ivy
  (setq ess-use-toolbar nil)
  (setq ess-fancy-comments nil)
  (setq ess-assign-list '(" <- " " = " " <<- " " -> " " ->> "))
  ;; Key bindings -- ESS stomps on my help key
  (define-key ess-mode-map "{" 'ess-electric-brace)
  (define-key ess-mode-map "\M-\C-h" nil) ; ESS stomps on my help key
  (define-key ess-mode-map (kbd "A-h") 'ess-mark-function-or-para)
  (define-key ess-mode-map (kbd "C-h") 'ess-mark-function-or-para))

(add-my-hook inferior-ess-mode-hook
  ;; Style Configuration
  (ess-add-crg-style :default)
  (ess-set-style 'CRG)
  ;; General ESS config
  ;(setq ess-auto-newline t)
  (setq ess-use-ido nil) ; prefer ivy
  ;; Key bindings
  (local-set-key "\C-d" 'delete-char)
  (local-set-key "\C-a" 'my/comint-bol)
  (local-set-key "\C-c\M-p" 'comint-backward-matching-input)
  (local-set-key "\C-c\M-n" 'comint-forward-matching-input)
  ;; Comint configuration
  (add-hook 'comint-input-filter-functions 'ess-search-path-tracker nil t) ;; deprecated?
  (setq comint-move-point-for-output 'all) ; follow output after eval or entry
  (setq input-ring-size '1024))

(add-my-hook ess-pre-run-hook
  (ess-add-crg-style :default))

;;; Was in ess-pre-run-hook but can't remember why
;;(progn
;;    (remove-hook 'ess-mode-hook             'turn-on-font-lock)
;;    (remove-hook 'ess-transcript-mode-hoonk 'turn-on-font-lock)
;;    (remove-hook 'inferior-ess-mode-hook    'turn-on-font-lock))

(add-my-hook ess-transcript-mode-hook
  ;; Style Configuration
  (ess-add-crg-style :default)
  (ess-set-style 'CRG))


