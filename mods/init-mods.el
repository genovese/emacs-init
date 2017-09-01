;;; -*- mode: emacs-lisp; -*-


(defvar init-mods
  '(comint dired ess help ibuffer icicles
    org shell tex)
  "Mods to be loaded automatically on startup.")

(let ((path (file-name-directory load-file-name)))
  (dolist (mod init-mods)
    (message "Loading mod %s..." mod)
    (load (concat path (symbol-name mod)))))

