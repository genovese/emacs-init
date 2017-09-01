;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; IDE Configuration CEDET/ECB/JDEE  (necessary files loaded in imports.el)

(defun my-semantic-init-hook ()
  (global-semantic-idle-scheduler-mode '1)
  (semantic-load-enable-excessive-code-helpers)
  (require 'semantic-ia))

;ATTN disable TEMPORARILY until I can figure out what semantic is doing wrong
;(add-hook 'semantic-init-hook 'my-semantic-init-hook)

(eval-after-load 'senator   ;; alternatively could put this in a hook
  '(progn
     (define-key senator-mode-map "\C-c,c"    'semantic-ia-describe-class)
     (define-key senator-mode-map "\C-c,d"    'semantic-ia-show-doc)
     (define-key senator-mode-map "\C-c,D"    'semantic-ia-show-summary)
     (define-key senator-mode-map "\C-c,e"    'eassist-list-methods)
     (define-key senator-mode-map "\C-c,F"    'semantic-ia-fast-jump)
     (define-key senator-mode-map "\C-c,l"    'semantic-complete-jump-local-members)
     (define-key senator-mode-map "\C-c,m"    'semantic-ia-complete-symbol)
     (define-key senator-mode-map "\C-c,M"    'semantic-ia-complete-symbol-menu)
     (define-key senator-mode-map "\C-c,s"    'semantic-symref)
     (define-key senator-mode-map "\C-c,S"    'semantic-symref-symbol)
     (define-key senator-mode-map "\C-c,x"    'semantic-symref-regexp)
     (define-key senator-mode-map "\C-c,y"    'semantic-ia-show-summary)
     (define-key senator-mode-map "\C-c,\C-k" 'senator-kill-tag)
     (define-key senator-mode-map "\C-c,\M-c" 'senator-copy-tag)
     (define-key senator-mode-map "\C-c,\C-w" nil)
     (define-key senator-mode-map "\C-c,\M-w" nil)
     (define-key senator-mode-map [?\C-c ?, return] 'semantic-mrub-switch-tags)
     ))

(eval-after-load 'semantic
  `(progn
     (global-semantic-idle-scheduler-mode -1)
     (global-semantic-decoration-mode -1)))


