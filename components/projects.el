;;; projects.el -- project management tools -*- lexical-binding: t; -*-

(use-package find-file-in-project
  :commands (find-file-in-project find-file-in-project-by-selected)
  :config (add-to-list 'ffip-project-file "project.clj" t))

(use-package counsel-projectile)

(use-package projectile
  :config (progn
            (def-projectile-commander-method ?s
              "Open a *shell* buffer for the project."
              ;; This requires a snapshot version of Projectile.
              (projectile-run-shell))
            (def-projectile-commander-method ?c
              "Run `compile' in the project."
              (projectile-compile-project nil))
            (def-projectile-commander-method ?\C-?
              "Go back to project selection."
              (projectile-switch-project))
            (def-projectile-commander-method ?j
              "Jack-in."
              (let* ((opts (projectile-current-project-files))
                     (file (ido-completing-read
                            "Find file: "
                            opts
                            nil nil nil nil
                            (car (cl-member-if
                                  (lambda (f)
                                    (string-match "core\\.clj\\'" f))
                                  opts)))))
                (find-file (expand-file-name
                            file (projectile-project-root)))
                (run-hooks 'projectile-find-file-hook)
                (cider-jack-in)))
            (counsel-projectile-mode)
            (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
            (projectile-mode 1)))


;;; projects.el ends here
