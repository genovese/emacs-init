;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Calendar Mode


(add-my-hook calendar-mode-hook
  (local-set-key "\C-w"    'calendar-scroll-right-three-months)
  (local-set-key "\M-p"    'calendar-backward-month)
  (local-set-key "\M-n"    'calendar-forward-month)
  (local-set-key "\M-w"    'calendar-beginning-of-year)
  (local-set-key "\M-v"    'calendar-end-of-year)
  (local-set-key "\C-\M-p" 'calendar-backward-year)
  (local-set-key "\C-\M-n" 'calendar-forward-year)
  (local-set-key "L"       'lunar-phases)
  (when (featurep 'astronomy)
    (local-set-key "T" 'calendar-astronomical-twilight)))

(add-hook 'calendar-today-visible-hook 'calendar-mark-today)

(setq calendar-week-start-day 1)  ; Start week on monday
(setq calendar-date-display-form
      '((when dayname (concat (substring dayname 0 3) " "))
        (format "%02d" (string-to-number day)) " "
        (substring monthname 0 3) " " year))
