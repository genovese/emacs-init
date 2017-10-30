;;; theme-support.el -- custom theme loading and hooks -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2017 Christopher R. Genovese, all rights reserved.
;; Author: Christopher Genovese <genovese@cmu.edu>
;; Version: 1.2.0

;;; Commentary:
;;  These utilities are only for daily use, not for use within distributed
;;  packages. Hence, some of the functions are unprefixed, some replace
;;  existing functions, and some use the `my/' prefix if its seems appropriate.


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


;;; theme-support.el ends here
