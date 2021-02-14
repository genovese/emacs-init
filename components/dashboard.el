;; (use-package page-break-lines)
;;  
;; (use-package dashboard
;;   :after (projectile page-break-lines)
;;   :config (progn
;;             (setq dashboard-items '((recents  . 5)
;;                                     (projects . 5)
;;                                     (registers . 5)))
;;             (setq dashboard-set-heading-icons t)
;;             (setq dashboard-set-file-icons t)
;;             (setq dashboard-projects-switch-function
;;                   'counsel-projectile-switch-project-by-name)
;;             (dashboard-setup-startup-hook)))
