;; ATTN: not needed see treemacs-toggle-show-dotfiles
(defun treemacs-toggle-hidden-files (&optional arg)
  "Toggle display of dot files in treemacs buffer.
With prefix arg, turn dot file display on."
  (interactive "P")
  (setq treemacs-show-hidden-files
        (if arg t (not treemacs-show-hidden-files)))
  (treemacs-run-in-every-buffer
   (treemacs--do-refresh (current-buffer) 'all))
  treemacs-show-hidden-files)

