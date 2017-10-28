;;; my-env.el -- local environment and application settings
;;
;; This version of the file is used on my Macbook Pro
;; running OS X 10.7.5.

;;
;; Browser
;;   Note: firefox 3.6.12 does not seem to open new tabs
;;         properly from the command line on OS X.
;;         So, we use open instead.
(setq browse-url-firefox-program "open")
(setq browse-url-firefox-arguments '("-a" "Firefox"))


;; Emacs App location
(setq emacs-app-base-dir "/usr/local/Cellar/emacs/HEAD-329e027/")

;;
;; Paths
;;

(setenv "PATH"    ".:./bin:../bin:/Users/genovese/bin:/usr/local/priority/bin:/usr/local/bin:/Library/TeX/texbin:/Users/genovese/Library/Python/Current/bin:/Users/genovese/.local/bin:/usr/bin:/bin:/usr/X11R6/bin:/usr/sbin:/sbin")
(setenv "MANPATH" "/Users/genovese/man:/usr/local/priority/man:/Library/TeX/Distributions/.DefaultTeX/Contents/Man:/usr/local/man:/usr/local/share/man:/usr/share/man:/usr/local/share/man")

(setq exec-path (cl-delete-duplicates (append (nthcdr 3 (split-string (getenv "PATH") ":")) exec-path)
                                      :test 'string-equal :from-end t))

(if (>= emacs-major-version 24)
    (setenv "INFOPATH" (concat emacs-app-base-dir "share/info/emacs:/usr/local/share/emacs/info:/usr/local/share/info:/usr/share/info:/usr/local/info"))
 (setenv "INFOPATH"    "/Applications/Emacs23.app/Contents/Resources/info:/usr/local/share/emacs/info:/usr/local/share/info:/usr/share/info:/usr/local/info"))

(setenv "TEXINPUTS"   ":./tex:../tex:../../tex:/Users/genovese/tex/ProbabilityExplained:/Users/genovese/tex")


;;
;; Emacs-specific location pointers
;;

(when (>= emacs-major-version 24)
  (setenv "EMACS_APP" (concat emacs-app-base-dir "Emacs.app/Contents/MacOS/Emacs"))
  (setenv "EMACS_DIR" (concat emacs-app-base-dir "Emacs.app/Contents/Resources/"))
  (setenv "EMACS_BIN" (concat emacs-app-base-dir "bin/")))

(when (= emacs-major-version 23)
  (setenv "EMACS_APP"   "/Applications/Emacs23.app/Contents/MacOS/Emacs")
  (setenv "EMACS_DIR"   "/Applications/Emacs23.app/Contents/Resources/"))

(setenv "EMACS_SHARE" "/usr/local/share/emacs/")


;;
;; Special directories
;;

(setenv "s750"        "/Users/genovese/class/s750")     ; class directories



;; 
;; General Emacs-useful environment settings
;;

(setenv "PAGER" "cat")

;;; end my-env.el
