;;; frames.el -- frame configuration and tools -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2017 Christopher R. Genovese, all rights reserved.
;; Author: Christopher Genovese <genovese@cmu.edu>
;; Version: 1.2.0

;;; Commentary:
;;  These utilities are only for daily use, not for use within distributed
;;  packages. Hence, some of the functions are unprefixed, some replace
;;  existing functions, and some use the `my/' prefix if its seems appropriate.


;;; Code:

(setq initial-frame-alist `(,@(get-preference 'initial-frame)
                            (cursor-color . ,(get-preference 'cursor-color)))
      default-frame-alist `(,@(get-preference 'default-frame)
                            (cursor-color . ,(get-preference 'cursor-color))
                            (menu-bar-lines . 1) (tool-bar-lines . 0)))

(defvar frame-title-prefix nil
  "Optional default label in frame title")

(setq frame-title-format
      '("" (:eval (or (frame-parameter nil 'frame-base-title)
                      frame-title-prefix
                      invocation-name)) " <%b>"))

(defun my/set-frame-title (title &optional frame)
  "Sets the base frame title to TITLE in FRAME, selected fame by default"
  (interactive "sFrame title: ")
  (let ((f (or frame (selected-frame))))
    (set-frame-parameter f 'frame-base-title title)))

(defun my/make-frame-command (&optional name)
  "Like `make-frame-command' but with prefix arg, prompt for frame title."
  (interactive (list (and current-prefix-arg
                          (read-from-minibuffer "Frame Title: "))))
  (let ((set-title? (and name (stringp name) (> (length name) 0)))
        (frame      (make-frame-command)))
    (when set-title?
      (my/set-frame-title name frame))
    frame))


;;; frames.el ends here
