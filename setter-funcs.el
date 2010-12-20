;; (@* "Hook Setters and Toggles")
;;
;; Convenience functions for turning on, turning off,
;; or toggling various features in mode hooks.
;;
;; Functions defined here:
;; `turn-on-tex-parser-ispell', `turn-on-local-show-paren-mode',
;; `turn-on-global-show-paren-mode', `turn-on-local-comment-auto-fill',
;; `highlight-attn-words'
;;

(defun turn-on-tex-parser-ispell ()
  (make-local-variable 'ispell-parser)
  (setq ispell-parser 'tex))

(defun turn-on-local-show-paren-mode ()
  (make-local-variable 'show-paren-mode)
  (setq show-paren-delay 0)  ; default 0.125
  (setq show-paren-style 'parenthesis)
  (show-paren-mode '1)
  (set-face-background 'show-paren-mismatch "red"))

(defun turn-on-global-show-paren-mode ()
  (setq show-paren-delay 0)  ; default 0.125
  (setq show-paren-style 'parenthesis)
  (show-paren-mode '1)
  (set-face-background 'show-paren-mismatch "red"))

(defun turn-on-local-comment-auto-fill ()
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (auto-fill-mode '1))

(defun highlight-attn-words ()
  (font-lock-add-keywords
   nil '(("\\<\\(ATTN\\|TODO\\|FIX\\|FIXME\\|HACK\\):?"
          1 font-lock-warning-face t))))


