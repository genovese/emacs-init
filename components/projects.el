;;; projects.el -- project management tools -*- lexical-binding: t; -*-

(use-package find-file-in-project
  :commands (find-file-in-project find-file-in-project-by-selected)
  :config (add-to-list 'ffip-project-file "project.clj" t))

(use-package counsel-projectile)

(use-package projectile
  :config (progn
            (counsel-projectile-mode)
            (projectile-mode 1)))


;;; projects.el ends here
