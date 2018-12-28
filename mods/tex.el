(defun turn-on-tex-parser-ispell ()
  (make-local-variable 'ispell-parser)
  (setq ispell-parser 'tex))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; AUC TeX support

;; Historically, I primarily used genTeX, which is why I used `plain-TeX-mode'
;; as the default AUCTeX mode. But I am transitioning to LaTeX recently.
;; Still the configuration below supports plain TeX, laTeX, and genTeX.
;;
;; The primary extension here
;; is easy functionality in `TeX-expand-list' to run
;; make on a target, which is a convenient for extended projects.

(setq tex-default-mode 'latex-mode) ;includes non AucTeX case
(setq TeX-default-mode 'LaTeX-mode)
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
    (defvar TeX-shell-escape-mode nil
      "Should -shell-escape be used when processing TeX/LaTeX files?")
    (add-to-list 'TeX-expand-list
                 '("%(shell)" (lambda ()
                                (if TeX-shell-escape-mode
                                    " -shell-escape"
                                  ""))))
    (add-to-list 'TeX-expand-list
                 '("%(pdfmk)" (lambda ()
                                (if (and (eq TeX-engine 'default)
                                         (if TeX-PDF-mode
                                             (not (TeX-PDF-from-DVI))
                                           TeX-DVI-via-PDFTeX))
                                    " -pdf"
                                  ""))))
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
    (add-to-list 'TeX-command-list '("Klmake" "latexmk%(pdfmk)%(shell) %s"
                                     TeX-run-command t t
                                     :help "Generate with LatexMk"))
    (require 'texmathp)
    (eval-after-load 'texmathp
      (progn
        (setq texmathp-tex-commands '(("equations" env-on)
                                      ("\\halign"  arg-off)))
        (texmathp-compile) ))
    (setq TeX-parse-self    t)
    (setq TeX-auto-save     t)
    (setq TeX-auto-untabify t))
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
  

(add-my-hook LaTeX-mode-hook
  "Personal configuration for LaTeX."
  (TeX-PDF-mode 1)
  (add-to-list 'LaTeX-font-list '(?n "\\notable{" "}"))
  (add-to-list 'LaTeX-font-list '(?s "\\strong{" "}"))
  (add-to-list 'LaTeX-font-list '(?S "\\salient{" "}"))
  (add-to-list 'LaTeX-font-list '(?\C-v "\\Verb|" "|")))

;; (defun insert-single-space (&rest args)
;;   (insert " "))
;;  
;; (defun my-LaTeX-mode-hook ()
;;   "Personal configuration for LaTeX."
;;   (TeX-PDF-mode 1)
;;   (add-to-list 'TeX-insert-braces-alist (cons "item" nil))
;;   (TeX-add-symbols '("item" ["label"] insert-single-space)))

(add-hook 'LaTeX-mode-hook #'highlight-attn-words)
(add-hook 'LaTeX-mode-hook #'turn-on-tex-parser-ispell)
(add-hook 'LaTeX-mode-hook #'turn-on-flyspell) 
;(add-hook 'LaTeX-mode-hook #'my-LaTeX-mode-hook)
(add-hook 'LaTeX-mode-hook #'my-generic-TeX-setup)
(add-hook 'LaTeX-mode-hook #'LaTeX-preview-setup)

(add-hook 'latex-mode-hook 'highlight-attn-words)
(add-hook 'latex-mode-hook 'turn-on-tex-parser-ispell)
(add-hook 'latex-mode-hook 'turn-on-flyspell) 
(add-hook 'latex-mode-hook 'my-generic-TeX-setup)
;(add-hook 'latex-mode-hook 'my-LaTeX-mode-hook)

(autoload 'LaTeX-environment "latex"
  "Make Environment Using AucTeX commands" t)
(autoload 'LaTeX-add-environments "latex"
  "Add Environment Info Using AucTeX commands" t)

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


