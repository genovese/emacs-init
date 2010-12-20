;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                           ;;
;;(@* "ESS Mode for R and S Code")                           ;;;;;;
;;                                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ESS-mode for S 


;;
;; Formatting Style 
;;

(setq ESS-crg-style '(CRG (ess-indent-level . 4)
                          (ess-continued-statement-offset . 4)
                          (ess-continued-brace-offset . -4 )
                          (ess-brace-offset . 0)
                          (ess-brace-imaginary-offset . 0)
                          (ess-close-brace-offset . 0)
                          (ess-else-offset . 0)
                          (ess-expression-offset . 0)
                          (ess-arg-function-offset . 0)
                          (ess-fancy-comments . nil)
                          (ess-auto-newline . t)
                          (ess-tab-always-indent . t)
                   ))

;;
;; Basic Configuration
;;

(defun my-ess-mode-load-hook ()
  (if (not (assoc 'CRG ess-style-alist))
      (setq ess-style-alist (cons ESS-crg-style ess-style-alist)))
  (setq ess-default-style 'CRG)
  (setq ess-style 'CRG)
  (setq ess-auto-newline t)
  (setq-default ess-loop-timeout '1000000)
  (setq-default inferior-ess-help-command "help(\"%s\")\n")
  (setq ess-use-toolbar nil)
  (setq ess-fancy-comments nil)
  )

(defun my-inferior-ess-mode-hook ()
  ;; Key bindings
  (local-set-key "\C-d" 'delete-char)
  (local-set-key "\C-a" 'my-comint-bol)
  (local-set-key "\C-c\M-p" 'comint-backward-matching-input)
  (local-set-key "\C-c\M-n" 'comint-forward-matching-input)
  ;; Comint configuration
  (add-hook 'comint-input-filter-functions 'ess-search-path-tracker nil t)
  (setq comint-move-point-for-output 'all) ; follow output after eval or entry
  (setq input-ring-size '1024)
  ;; Style Configuration (these three lines needed?)
  (setq ess-style 'CRG) 
  (setq ess-auto-newline t)
  (ess-set-style)
  ;; General ESS config
  (setq inferior-ess-help-command "help(\"%s\")\n")
  )

(defun my-ess-pre-run-hook ()
  (progn
    (remove-hook 'ess-mode-hook             'turn-on-font-lock)
    (remove-hook 'ess-transcript-mode-hoonk 'turn-on-font-lock)
    (remove-hook 'inferior-ess-mode-hook    'turn-on-font-lock)
    )
  )

(defun my-ess-mode-hook ()
  (setq ess-style 'CRG)
  (setq ess-auto-newline t)
  (ess-set-style)
  )

(defun ess-transcript-mode-hook ()
  (setq ess-style 'CRG)
  (setq ess-auto-newline t)
  (ess-set-style)
  )

;;
;; Enable the hooks
;;

(add-hook 'ess-mode-load-hook       'my-ess-mode-load-hook)
(add-hook 'inferior-ess-mode-hook   'my-inferior-ess-mode-hook)
(add-hook 'ess-pre-run-hook         'my-ess-pre-run-hook)
(add-hook 'ess-mode-hook            'my-ess-mode-hook)
(add-hook 'ess-transcript-mode-hook 'my-ess-transcript-mode-hook)
(require-soft 'ess-site)

(if (not (featurep 'ess))
    (message "Cannot load ESS."))

;; (setq ess-style-alist (cons ESS-crg-style ess-style-alist)) ;;  needed?



