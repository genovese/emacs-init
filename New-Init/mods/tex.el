(defun turn-on-tex-parser-ispell ()
  (make-local-variable 'ispell-parser)
  (setq ispell-parser 'tex))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; AUC TeX support

;; I primarily use genTeX, which is why I use `plain-TeX-mode'
;; as the default AUCTeX mode. But the configuration below supports
;; plain TeX, laTeX, and genTeX. The primary extension here
;; is easy functionality in `TeX-expand-list' to run
;; make on a target, which is a convenient for extended projects.

(setq tex-default-mode 'plain-tex-mode) ;includes non AucTeX case
(setq TeX-default-mode 'plain-TeX-mode)
(setq my-TeX-generic-setup-done nil)

(defun my-generic-TeX-setup ()
  "Configure global settings and interface that affects all 
AucTeX modes. Only performed once across all modes."
  (do-only-once my-TeX-generic-setup-done
    (add-to-list 'TeX-format-list '("TEX" plain-tex-mode "\\input\\b"))
    (add-to-list 'TeX-expand-list '("%a" file "pdf" t))
    (add-to-list 'TeX-expand-list '("%M"
                                    (lambda ()
                                      (let ((targets (my/get-makefile-targets))) 
                                        (completing-read "Target: " targets)) )))
    (cl-delete-if (lambda (item) (string-equal "Makeinfo" (car item))) TeX-command-list)
    (cl-delete-if (lambda (item) (string-equal "Makeinfo HTML" (car item))) TeX-command-list)
    (add-to-list 'TeX-command-list '("Info" "makeinfo %t" TeX-run-compile nil
                                     (texinfo-mode) :help "Run makeinfo with Info output"))
    (add-to-list 'TeX-command-list '("Info HTML" "makeinfo --html %t" TeX-run-compile nil
                                     (texinfo-mode) :help "Run makeinfo with HTML output"))
    (add-to-list 'TeX-command-list '("PDF" "%(o?)dvipdf %d %a " TeX-run-command t t
                                     :help "Generate PDF file"))
    (add-to-list 'TeX-command-list '("Make" "make %M " TeX-run-command t t
                                     :help "Generate with a Makefile"))
    (require 'texmathp)
    (eval-after-load 'texmathp
      (progn
        (setq texmathp-tex-commands '(("equations" env-on)
                                      ("\\halign"  arg-off)))
        (texmathp-compile) ))
    (setq TeX-parse-self t)
    (setq TeX-auto-save  t))
  (local-set-key "\C-c`" 'TeX-next-error)
  (local-set-key "\M-gn" 'TeX-next-error)
  (local-set-key "\M-gp" 'TeX-previous-error))

(add-hook 'plain-TeX-mode-hook 'highlight-attn-words)
(add-hook 'plain-TeX-mode-hook 'turn-on-flyspell) 
(add-hook 'plain-TeX-mode-hook 'turn-on-tex-parser-ispell)
(add-hook 'plain-TeX-mode-hook 'my-generic-TeX-setup)

(add-my-hook plain-TeX-mode-hook
  "Personal configuration for GenTeX and plain TeX."
  (local-set-key "\C-cp" 'TeX-PDF-mode)
  (local-set-key "\C-c[" 'TeX-insert-braces) ; why shift?
  (local-set-key "\C-c\C-n" nil)
  (local-set-key "\C-c\C-m" nil)
  (local-set-key [?\C-c return] 'TeX-insert-macro)
  ;; electric $ balks on embedded $ but try again for now
  ;(local-set-key "$" 'self-insert-command)
  ;; GenTeX specific font specifiers
  (setq TeX-font-list '( (?\C-b "\\textbold{" "}" "\\mathbold{" "}") 
			 (?\C-c "\\smcaps{" "}")
			 (?\C-e "\\emph{" "}")
			 (?\C-i "{\\it " "\\/}")
			 (?\C-r "{\\rm " "}")
			 (?\C-s "\\sans{" "}")
			 (?\C-t "\\tty{" "}")
			 (?\C-u "{" "}")
			 (?\C-d "" "" t)
			 (?\M-p "{\\xlarge " "}")
			 (?\C-p "{\\large " "}")
			 (?\C-m "{\\medium " "}")
			 (?\C-n "{\\small " "}")
			 (?\M-n "{\\xsmall " "}")
			 ))
  (LaTeX-add-environments '("equations" LaTeX-env-label)) 
  (local-set-key "\C-c\C-e" 'LaTeX-environment)
  (setq LaTeX-default-environment "itemize"))
  

(defun my-LaTeX-mode-hook ()
  "Personal configuration for LaTeX."
  (TeX-PDF-mode 1))

(add-hook 'LaTeX-mode-hook #'highlight-attn-words)
(add-hook 'LaTeX-mode-hook #'turn-on-tex-parser-ispell)
(add-hook 'LaTeX-mode-hook #'turn-on-flyspell) 
(add-hook 'LaTeX-mode-hook #'my-LaTeX-mode-hook)
(add-hook 'LaTeX-mode-hook #'my-generic-TeX-setup)
(add-hook 'LaTeX-mode-hook #'LaTeX-preview-setup)

(add-hook 'latex-mode-hook 'highlight-attn-words)
(add-hook 'latex-mode-hook 'turn-on-tex-parser-ispell)
(add-hook 'latex-mode-hook 'turn-on-flyspell) 
(add-hook 'latex-mode-hook 'my-LaTeX-mode-hook)
(add-hook 'latex-mode-hook 'my-generic-TeX-setup)

(autoload 'LaTeX-environment "latex" "Make Environment Using AucTeX commands" t)
(autoload 'LaTeX-add-environments "latex" "Add Environment Info Using AucTeX commands" t)

(add-hook 'AmS-TeX-mode-hook 'my-generic-TeX-setup)


(eval-after-load 'tex
 '(progn
    (add-to-list 'TeX-view-program-selection '(output-pdf "OSX-open"))
    (add-to-list 'TeX-view-program-list '("OSX-open" "open %o"))

    ;; Define font-latex-keyword-face (if not already defined)
    ;; and remap this in latex buffers
    ;; This allows keywords to be less obtrusive in TeX but not in programming languages
    (defun my-latex-fix-keywords-face ()
      (when (facep 'font-latex-keyword-face)
        (face-remap-add-relative 'font-lock-keyword-face 'font-latex-keyword-face)))
    (add-hook 'LaTeX-mode-hook 'my-latex-fix-keywords-face t)
    (add-hook 'latex-mode-hook 'my-latex-fix-keywords-face t)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; BibTeX-mode

(add-my-hook bibtex-mode-hook
  (local-set-key "\"" 'self-insert-command)
  (setq bibtex-include-OPTannote nil)
  (setq bibtex-include-OPTcrossref nil))


