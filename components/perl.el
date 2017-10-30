;;; perl.el -- perl setup and tools -*- lexical-binding: t; -*-

(add-hook 'perl-mode-hook 'highlight-attn-words)
(add-my-hook perl-mode-hook
  "Use C-style indenting"
  (setq perl-indent-level '4)
  (setq perl-continued-statement-offset '4)
  (setq perl-continued-brace-offset '-4)
  (setq perl-brace-offset '0)
  (setq perl-brace-imaginary-offset '0)
  (setq indent-tabs-mode 'nil) 
  (setq perl-label-offset '-2))


;;; perl.el ends here
