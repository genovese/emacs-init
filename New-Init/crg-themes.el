;;; -*- mode: emacs-lisp; lexical-binding: t; -*-

;; crg-themes --- theme settings and utilities
;;
;; Copyright (C) 2014 Christopher R. Genovese, all rights reserved.

;; Author: Christopher Genovese <genovese@cmu.edu>
;; Maintainer: Christopher R. Genovese <genovese@cmu.edu>
;; URL: http://www.stat.cmu.edu/~genovese/emacs/

;; Version: 1.0.0
;; Update#: 1
;; Created:      Thu 17 Aug 2017
;; Last-Updated: Thu 17 Aug 2017 at 13:30 EDT
;; By: Christopher R. Genovese

;;; Commentary:

;; These utilities are used in my init file (and emacs sessions).
;; Hence, they are unprefixed.

;;; Code:

(defun my/zenburn+-on-hook ()
  (interactive)
  (when (member 'zenburn+ custom-enabled-themes)
    (set-face-foreground 'region "white")
    (set-face-background 'region "blue")
    (when (featurep 'dired+)
      (set-face-foreground 'diredp-dir-priv    "#33cc33") ; was "magenta3"
      (set-face-background 'diredp-dir-priv    nil)
      (set-face-foreground 'diredp-file-suffix "cornflower blue")
      (set-face-foreground 'diredp-file-name   "#E0CF9F")
      (set-face-foreground 'diredp-number      "gray60")
      (set-face-foreground 'diredp-dir-heading "Blue")
      (set-face-background 'diredp-dir-heading "bisque1")
      (set-face-background 'diredp-no-priv     "black")
      (set-face-foreground 'diredp-date-time   "#74749A9AF7F7"))
    (setq fci-rule-color "#999999")))

(defun my/zenburn ()
  (interactive)
  (load-theme 'zenburn+)
  (enable-theme 'zenburn+)
  (my/zenburn+-on-hook))

(provide 'crg-themes)

;;; crg-themes.el ends here
