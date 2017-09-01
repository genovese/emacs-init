;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Interactive Do Things (ido-mode)

;; Added Functionality
;;
;; * Smart beginning-of-line command
;;
;;   Command `my/ido-begin-line' moves to the
;;   beginning of the line on first invocation,
;;   but successive invocations toggle among
;;   the home-directory and the root-directory.
;;   This is used for quick editing when
;;   the desired file is not in current part
;;   of the tree
;;   

(defvar my/ido-begin-str "~/")

(defvar my/ido-begin-home 'home
  "*Determines the behavior of \\[my/ido-begin-line].
    If equal 'home,  replaces existing
    file text with the home directory. If 'root,
    replaces it with root. And if 'old, replaces
    it with value of \\[my/ido-begin-str]. The
    function toggles among these states.")

(defun my/ido-begin-line ()
  "*Moves to the beginning of the find file line,
    toggling between the home directory and root
    on successive calls."
  (interactive)
  (let
      ;; ATTN: Fix this 12 == length("Find file: ")
      ((contents (buffer-substring-no-properties 12 (minibuffer-prompt-end))))
    (cond
     ((equal my/ido-begin-home 'home)
      (insert "~/")
      (setq my/ido-begin-home 'root)
      ;;(setq my/ido-begin-str contents)
      (kill-new contents) ;;ATTN: for now make old string available to yank
      )
     ((equal my/ido-begin-home 'root)
      (insert "//")
      (setq my/ido-begin-home 'home) ;;'old)
      )
     (t ;; ATTN: for now this isn't working because delete fails so ignore it
      (setq my/ido-begin-home 'home)
      ;;(delete-region 12 (minibuffer-prompt-end))
      ;;(insert my/ido-begin-str)
      )))
  (setq ido-rescan t))

;; Configuration
;;
;; A rebinding of the standard map that I find more memorable.
;; 

(add-my-hook ido-setup-hook
  "Adjust keymaps for ido completion and other customizations."
  (when (and (bound-and-true-p ido-file-completion-map)
             (keymapp ido-file-completion-map))
    (define-key ido-file-completion-map "\C-a" 'my/ido-begin-line)
    (define-key ido-file-completion-map "\C-p" 'ido-prev-match)
    (define-key ido-file-completion-map "\C-n" 'ido-next-match)
    (define-key ido-file-completion-map "\C-xc"    'ido-toggle-case)
    (define-key ido-file-completion-map "\C-x\C-c" 'ido-toggle-case)
    (define-key ido-file-completion-map "\C-x\C-i" 'ido-toggle-ignore)
    (define-key ido-file-completion-map "\C-xi"    'ido-toggle-ignore)
    (define-key ido-file-completion-map "\C-xp"    'ido-toggle-prefix)
    (define-key ido-file-completion-map "\C-x\C-p" 'ido-toggle-prefix)
    (define-key ido-file-completion-map "\C-xr"    'ido-toggle-regexp)
    (define-key ido-file-completion-map "\C-x\C-r" 'ido-toggle-regexp))
  (setq ido-enable-flex-matching t))


