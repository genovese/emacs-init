;;; web.el -- web and related tools -*- lexical-binding: t; -*-

(use-package edit-server
  :if window-system
  :init
  (add-hook 'after-init-hook 'edit-server-start t)
  :config
  (setq edit-server-new-frame-alist '((name . "Edit with Emacs")
                                      (top . 4)
                                      (left . 16)
                                      (width . 96)
                                      (height . 42)
                                      (minibuffer . t)
                                      (menu-bar-lines . t))))

(use-package edit-server-htmlize
  :after (edit-server)
  :config
  (add-hook 'edit-server-start-hook 'edit-server-maybe-dehtmlize-buffer)
  (add-hook 'edit-server-done-hook 'edit-server-maybe-htmlize-buffer)
  ;; The built-in version has a bug fixed by max -> (+ max change)
  (defun html2text-replace-string (from-string to-string min max)
    "Replace FROM-STRING with TO-STRING in region from MIN to MAX."
    (goto-char min)
    (let ((delta (- (length to-string) (length from-string)))
	  (change 0))
      (while (search-forward from-string (+ max change) t)
        (replace-match to-string)
        (setq change (+ change delta)))
      change))
  ;; Don't use pre, just change line ends and entities, for now.
  (defun edit-server-htmlize-buffer ()
    "Do a simple HTMLification of the buffer as plain text.
This produces HTML intended to reproduce the original plain text contents
of the buffer."
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (edit-server-htmlize-replace edit-server-htmlize-regexp
                                   edit-server-htmlize-replacements)
      (html2text-replace-string "\n" "<br>" (point-min) (point-max))))
  (defun edit-server-save-buffer ()
    (kill-ring-save (point-min) (point-max)))
  ;; We want this at the beginning
  (add-hook 'edit-server-done-hook 'edit-server-save-buffer))

(use-package js2-mode
  :mode "\\.js\\'"
  :config
  (add-hook 'js2-mode-hook
            (lambda ()
              (setq forward-sexp-function nil)
              (setq skeleton-pair t)
              (subword-mode 1)
              (local-set-key "(" 'skeleton-pair-insert-maybe)
              (local-set-key "[" 'skeleton-pair-insert-maybe)
              (local-set-key "{" 'skeleton-pair-insert-maybe))))

(use-package rjsx-mode
  :mode "\\.jsx\\'"
  :defer t)

(use-package js2-refactor
  :defer t)

(use-package js2-closure
  :defer t)

(use-package restclient
  :defer t)

(use-package sass-mode
  :mode "\\.sass\\'")

(use-package scss-mode
  :mode "\\.scss\\'")

(use-package simple-httpd
  :defer t)

(use-package skewer-mode
  :defer t)

(use-package web-mode
  :defer t)

(use-package web
  :defer t)

(use-package zencoding-mode
  :defer t)

;;; web.el ends here
