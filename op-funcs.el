;; (@* "Emacs Operations")
;;
;; Various functions that effect how emacs operates,
;; from the level of copying text to making backup
;; files and more.
;;
;; Functions defined here:
;; `make-backup-file-name', `set-abbrev-mark', `my-other-frame',
;; `my-kill-ring-save', `my-copy-region-as-kill',
;; 

(defun make-backup-file-name (file)
  "Create the non-numeric backup file name for FILE.
This is a separate function so you can redefine it for customization."
  (concat (file-name-directory file) (concat "." (concat (file-name-nondirectory file) "~"))))

(defun set-abbrev-mark ()
  "Mark current point as the beginning of an abbrev.
Abbrev to be expanded starts here rather than at beginning of word before point."
  (interactive)
  (setq abbrev-start-location (point-marker)
	abbrev-start-location-buffer (current-buffer))
  (message "Abbrev Mark Set"))

(defun my-other-frame (arg)
  "Select the ARG'th different visible frame, and raise it, even if
the frame is iconified or invisible. All frames are arranged in a
cyclic order, and this command selects the frame ARG steps away in
that order.  A negative ARG moves in the opposite order."
  (interactive "p")
  (let ((frame (selected-frame)))
    (while (> arg 0)
      (setq frame (next-frame frame))
      (setq arg (1- arg)))
    (while (< arg 0)
      (setq frame (previous-frame frame))
      (setq arg (1+ arg)))
    (raise-frame frame)
    (select-frame frame)
    (set-mouse-position (selected-frame) (1- (frame-width)) 0)
    (if (fboundp 'unfocus-frame)
	(unfocus-frame))))

;; Region copying:
;; 
;; I like a message even if the entire region is visible
;; and transient mark mode is on. Because it is conditional
;; cannot implement this easily with advice.

(defun my-kill-ring-save (beg end)
  "Save the region as if killed, but don't kill it.
In Transient Mark mode, deactivate the mark.
If `interprogram-cut-function' is non-nil, also save the text for a window
system cut and paste.

If you want to append the killed line to the last killed text,
use \\[append-next-kill] before \\[kill-ring-save].

This command is similar to `copy-region-as-kill', except that it gives
visual feedback indicating the extent of the region being copied, and
prints a message to the echo area accordingly."
  (interactive "r")
  (copy-region-as-kill beg end)
  ;; This use of interactive-p is correct
  ;; because the code it controls just gives the user visual feedback.
  (if (interactive-p)
      (let ((other-end (if (= (point) beg) end beg))
	    (opoint (point))
	    ;; Inhibit quitting so we can make a quit here
	    ;; look like a C-g typed as a command.
	    (inhibit-quit t))
	(if (pos-visible-in-window-p other-end (selected-window))
	    (if (and transient-mark-mode
                     (face-background 'region))
                (message "Region Copied")
	      ;; Swap point and mark.
	      (set-marker (mark-marker) (point) (current-buffer))
	      (goto-char other-end)
	      (sit-for blink-matching-delay)
	      ;; Swap back.
	      (set-marker (mark-marker) other-end (current-buffer))
	      (goto-char opoint)
	      ;; If user quit, deactivate the mark
	      ;; as C-g would as a command.
	      (and quit-flag mark-active
		   (deactivate-mark)))
	  (let* ((killed-text (current-kill 0))
		 (message-len (min (length killed-text) 40)))
	    (if (= (point) beg)
		;; Don't say "killed"; that is misleading.
		(message "Saved text until \"%s\""
			(substring killed-text (- message-len)))
	      (message "Saved text from \"%s\""
		      (substring killed-text 0 message-len))))))))

  ;; Standard version is silent, so it is hard to tell if the key was hit
  ;; Only added a message. ATTN: This could be implemented with advice,
  ;; but copy-region-as-kill is used programmatically in other functions
  ;; (e.g., kill-ring-save) that may not want a message printed.

(defun my-copy-region-as-kill (beg end)
  "Save the region as if killed, but don't kill it.
If `interprogram-cut-function' is non-nil, also save the text for a window
system cut and paste.  Produces a message to confirm copy."
  (interactive "r")
  (if (eq last-command 'kill-region)
      (kill-append (buffer-substring beg end) (< end beg))
    (kill-new (buffer-substring beg end)))
  (message "Region Copied")
  (if transient-mark-mode
      (setq deactivate-mark t))
  nil)

