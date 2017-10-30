;;; translations.el -- key translation maps -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2017 Christopher R. Genovese, all rights reserved.
;; Author: Christopher Genovese <genovese@cmu.edu>
;; Version: 1.2.0

;;; Commentary:
;;  
;;  The primary use of these maps at the moment is for Mac OS X:
;;  emulating a three-button mouse, unshadowing a few keys that are
;;  usually captured by the system, and setting the modifier keys
;;  properly.


;;; Code:

;; Modifier keys, apparently only on OS X but keep outside just in case
(setq ns-command-modifier  'meta) 
(setq ns-option-modifier   'alt)
(setq ns-function-modifier 'super)

(when (eq my-platform 'macosx)
  ;; Three-button-mouse emulations
  (define-key key-translation-map     [s-mouse-1]          [mouse-2])
  (define-key key-translation-map   [C-s-mouse-1]        [C-mouse-2])
  (define-key key-translation-map   [M-s-mouse-1]      [M-C-mouse-2])

  (define-key key-translation-map   [s-S-mouse-1]     [down-mouse-2])
  (define-key key-translation-map [C-s-S-mouse-1]   [C-down-mouse-2])
  (define-key key-translation-map [M-s-S-mouse-1] [M-C-down-mouse-2])

  (define-key key-translation-map     [A-mouse-1]          [mouse-3])
  (define-key key-translation-map   [C-A-mouse-1]        [C-mouse-3])
  (define-key key-translation-map   [M-A-mouse-1]      [M-C-mouse-3])

  (define-key key-translation-map   [A-S-mouse-1]     [down-mouse-3])
  (define-key key-translation-map [C-A-S-mouse-1]   [C-down-mouse-3])
  (define-key key-translation-map [M-A-S-mouse-1] [M-C-down-mouse-3])

  ;; Key unshadowing
  (define-key key-translation-map [(alt ?\ )] nil)
  (define-key key-translation-map [(alt ?u)] nil))


;;; translations.el ends here
