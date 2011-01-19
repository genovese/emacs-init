;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                           ;;
;;(@* "Initial State")                                       ;;
;;                                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load-file custom-file)   ;; holds customization-only customizations
(my-set-operating-state)

;; handle scratch buffer if requested
(my-aif (get-buffer "*scratch*")  
    (cond
     ((stringp my-keep-scratch-buf)
      (with-current-buffer it
        (rename-buffer my-keep-scratch-buf)
        (funcall initial-major-mode)))
     ((null my-keep-scratch-buf)
      (kill-buffer it))))

;; load additional environment variables, if any
;; Note: Mac OS X uses ~/.MacOSX/environment.plist
;; Might also want some emacs-specific environment settings.
(let ((envir (concat my-home-lisp-dir "my-env.el")))
  (if (file-exists-p envir)
    (load-file envir)))  

;; start in the shell
;;   use *unix* as the buffer name, rather than *shell*,
;;   to avoid completion conflicts with "*Shell Command Output*"
(let* ((shell-buf (generate-new-buffer-name "*unix*")))
  (shell shell-buf)
  (with-selected-window (get-buffer-window shell-buf)
    (delete-other-windows)))
