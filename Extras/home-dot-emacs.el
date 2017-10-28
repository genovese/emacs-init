;; Select and initialize one package system: package, cask, cask-homebrew
;; This must come before configuration of installed packages.
(let ((package-system 'package))
  (cond
   ((eq package-system 'package)
    (package-initialize))
   ((eq package-system 'cask)
    (require 'cask "~/.cask/cask.el")
    (cask-initialize))
   ((eq package-system 'cask-homebrew)
    (require 'cask "/usr/local/share/emacs/site-lisp/cask/cask.el")
    (cask-initialize))))

(load-file "~/.emacs.d/init/dot-emacs.el")
