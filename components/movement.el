;;; movement.el -- movement/navigation tools -*- lexical-binding: t; -*-

(use-package win-switch
  :config (progn
            (win-switch-authors-configuration)
            (setq win-switch-other-window-first nil)
            (setq win-switch-window-threshold 1)))

(use-package avy
  :bind (("C-." . avy-goto-char-timer)
         ("C-:" . avy-goto-char-2)
         ("M-g g" . avy-goto-line)  ;; delegates to goto-line with a digit
         ("C-," . avy-goto-word-1)
         ))

;;(use-package ace-jump-mode
;;  :bind ("C-." . ace-jump-mode)
;;  :config (bind-key "C-," 'ace-jump-mode-pop-mark)) ;;ATTN: maybe temporary

(use-package expand-region
  :bind (("C-="   . er/expand-region)
         ([?\A- ] . er/expand-region)
         ("C-+"   . er/contract-region)
         ([?\s- ] . er/contract-region)))

(use-package multiple-cursors
  :init
  (defhydra hydra-multiple-cursors (:hint nil)
    "
           ^Up/Down^    ^Mark^                  ^Other^
      ----------------------------------------  ----------------------
      [_p_]   Next     [_a_] Mark all           [_l_] Edit lines
      [_P_]   Skip     [_A_] Mark all alike     [_b_] Edit line starts
      [_M-p_] Unmark   [_d_] Mark all defun     [_e_] Edit line ends
      [_n_]   Next     [_D_] Mark words defun   [_#_] Insert numbers
      [_N_]   Skip     [_w_] Mark words alike   [_z_] Insert letters
      [_M-n_] Unmark   [_s_] Mark symbols alike [_o_] Pop Mark
      [_m_]   More     [_<_] Mark sgml pair     [_q_] Quit
      ^ ^              [_r_] Mark by regexp     ^ ^
     "
    ("n" mc/mark-next-like-this)
    ("N" mc/skip-to-next-like-this)
    ("M-n" mc/unmark-next-like-this)
    ("p" mc/mark-previous-like-this)
    ("P" mc/skip-to-previous-like-this)
    ("M-p" mc/unmark-previous-like-this)
    ("m" mc/mark-more-like-this-extended)
    ("a" mc/mark-all-dwim :exit t)
    ("A" mc/mark-all-like-this-dwim :exit t)
    ("d" mc/mark-all-like-this-in-defun :exit t)
    ("D" mc/mark-all-words-like-this-in-defun :exit t)
    ("w" mc/mark-all-words-like-this :exit t)
    ("s" mc/mark-all-symbols-like-this :exit t)
    ("<" mc/mark-sgml-tag-pair :exit t)
    ("r" mc/mark-all-in-region-regexp :exit t)
    ("l" mc/edit-lines)  ; :exit t causes failure here (why??)
    ("b" mc/edit-beginnings-of-lines) ; :exit t causes failure here
    ("e" mc/edit-ends-of-lines) ; :exit t causes failure here
    ("#" mc/insert-numbers)
    ("z" mc/insert-letters)
    ("o" mc/mark-pop)
    ("x" mc/repeat-command)
    ("q" nil))
  :bind ("C-9" . hydra-multiple-cursors/body)
  :demand t)


;;; movement.el ends here
