;;; -*- Mode: Emacs-Lisp; Mode: Linkd -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                    ;;
;; Emacs Initialization                                               ;;
;;                                                                    ;;
;; Version:    2.5.4                                                  ;;
;; Author:     Christopher R. Genovese                                ;;
;; Maintainer: Christopher R. Genovese                                ;;
;;                                                                    ;;
;; Last Updated: 2010 Dec 18 Sat 23:40                                ;;
;; By:           Christopher R. Genovese                              ;;
;; Update #:     258                                                  ;;
;;                                                                    ;;
;; Copyright (C) 2010, Christopher R. Genovese, all rights reserved.  ;;                    
;;                                                                    ;;
;;                                                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Commentary
;;
;; This emacs initialization is designed to support GNU Emacs 22+.
;; It has so far been tested with versions 22.2.1 and 23.2. 
;; Support for earlier emacs versions and for XEmacs is not 
;; guaranteed, although much of the functionality may work.
;;
;; This initialization file is built from a number of components
;; and scripts. If you have access to the original files, you
;; should modify those and not this one. You can access the
;; original files at
;;
;;     (@url :file-name "http://www.stat.cmu.edu/~genovese/code/emacs-init/" :display "Genovese Emacs Init")
;;
;; or email the author at (@url :file-name "mailto:genovese@cmu.edu" :display "genovese@cmu.edu").
;;
;; The intialization file works best with a number of third-party
;; and pre-installed modes. See the README file at the above
;; link for a list with version and update numbers. Most, if not
;; all, can be obtained at (@url :file-name "http://www.emacswiki.org/" :display "EmacsWiki"). The code tests for
;; the presence of these libraries, so no errors should occur
;; if they are missing. The code can be customized to incorporate
;; different or additional libraries; see the aforementioned README
;; file for instructions and see imports.el in the build directory
;; or (@> "Imported Mode Requirements") below.
;;
;; NOTE: If you have (@url :file-name "http://www.emacswiki.org/emacs/LinkdMode" :display "linkd.el"), you can engage \\[linkd-mode]
;;       to convert the table of contents below into links 
;;       throughout this file. Also, major divisions are separated
;;       by two consecutive newlines so including \n\n+ in your
;;       page-delimiter regexp will make it easy to navigate
;;       within sections.
;;
;; 1. Added Functionality
;;
;; This file extends the functionality to editing, navigation, dired,
;; icicles, ibuffer, TeX modes, and shell directory tracking, as well as
;; defining a variety of utility functions and commands. See the
;; extended documentation in the sections below.
;;
;; With editing and scrolling see the commands `my-move-beginning-of-line',
;; `scroll-down-or-beg', `scroll-up-or-end', `forward-sexp-or-char', and
;; `backward-sexp-or-char' for some examples.
;;
;; 2. Keybindings
;;
;; The keybindings section below extensively remaps defaults both in
;; the global map for specific modes that I've refined over the years.
;; Each person has different preferences, of course, but I think you
;; will find these useful. I've found these bindings to be both
;; memorable and efficient. An underlying design goal is to make
;; as much of the functionality as possible easily accessible.
;;
;; In addition, there is a consistent theme to some of the bindings
;; that differs from the default GNU keymaps. This has implications
;; for the bindings in the global and various mode maps. These
;; differences are as follows:
;;
;;     A. I use C-w/C-v instead of C-v/M-v for scrolling.
;;        This keeps common level scrolling at the same level,
;;        and I find it easier to use these keys than to
;;        change modifiers.
;;
;;     B. I use M-w/M-v for beginning and end of buffer.
;;
;;     C. I use M-k for kill region rather than C-w,
;;        along with other kill commands C-k and C-M-k.
;;        This is both mnemonic and consistent.
;;
;;     D. Although help is important, an extra key stroke
;;        for help is a small price to pay to reduce a more
;;        commonly used command. Specifically, help-command
;;        is bound to C-c h (and C-c C-h and M-? for convenience).
;;        C-h is bound to `delete-backward-char' (to save pinky
;;        extension for the backspace key) and M-h is bound to
;;        `delete-backward-word' which is very frequently used.
;;
;;     E. Although M-x is still available, I find it more
;;        convenient to bind `execute-extended-command'
;;        to C-x C-m and C-c C-m. 
;;       
;; In addition to the manifold other bindings, I propogate
;; this theme throughout the keybindings when the logical
;; functions in A-E are relevant. Eventually, I will add
;; a switch to make the bindings use the default theme, but
;; for now, you might want to try them as is.
;;
;; See also my quick-nav package for an even more efficient
;; keybinding theme and associated modes.
;;
;; 3. Configuration
;;
;; Configuration for various modes and other tools is given
;; by sections as indicated by the following contents. Several
;; new configuration variables are also introduced. See the
;; documentation below.
;;
;; 4. Conventions
;;
;; I use a prefix my- for *most* new functions and variables to avoid
;; conflicts now or in future versions, although this is used somewhat
;; inconsistently in older code. Most hooks are defined with
;; a my- version first which is then added to the hook. Interactive
;; functions in several modes are modified into my- versions and used
;; for keybindings (see for instance, `my-comint-bol'), although many
;; do not use my-. Variables or functions for purely internal use
;; are prefixed by private/my- with obvious meaning. In a future
;; version, I am likely to transition to a two name space with slash
;; formulation, using pvt-crg/ and crg/  in place of my-, using my-
;; only for configuration variables for which that makes the most sense.
;; I am still considering this, however.
;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TABLE OF CONTENTS

