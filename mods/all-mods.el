;;; all-mods.el -- load custom modifications -*- lexical-binding: t; -*-

(defvar all-mods
  '(comint dired ess ess-extra help ibuffer icicles
    org shell tex)
  "Mods to be loaded automatically on startup.")

(let ((path (file-name-directory load-file-name)))
  (dolist (mod all-mods)
    (message "Loading mod %s..." mod)
    (load (concat path (symbol-name mod)))))

;;; all-mods.el ends here
