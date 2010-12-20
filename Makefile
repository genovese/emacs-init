# -*- mode: makefile-gmake; mode: linkd; -*-
#
# Adjust the variables in (@> "Part I") to customize the build
# and install process. See the file (@file :file-name "./README" :display "README") for more details
# on customizing the intialization the .emacs file itself.
# Only change (@> "Part II") if you know what you are doing.
#
# Overview:
#
# To build and install a byte-compiled initialization file
# do the following:
#
#    make
#    make install
#    make clean  
#
# Be sure to verify successful compilation before proceeding to the
# second step. Emacs will give some compilation warnings which you can
# probably ignore. You can set INSTALL on the command line to
# change the final init file name and location;
# see (@> "INSTALL*") below for more options.
#
# To build and install an emacs lisp file, do the following:
#
#    make elisp
#    make install INSTALL_SUFX=.el
#    make fullclean  
#
# Again see (@> "INSTALL*") below for more options.
#
# By doing
#
#    make archive
#
# you can save a copy of these files in a compressed
# archive. The tag ARCHIVE allows you to specify the
# name and location of the archive file.
# See (@> "ARCHIVE*") below for more details.
#
# Note: This makefile follows GNU make conventions,
# particularly in the use of functions for the
# specification of the INSTALL tag.
#
# Requirements: This makefile currently uses
# perl (v5) and python (v2.6) to make the
# whoami.el and toc.el files. The next version
# will fix this unfortunate burden and make
# the build more flexible.
#


################################# (@* "Part I")

# Emacs configuration.
#
# Set EMACS the location of your emacs executable.
# If you want to make the fullarchive target, you
# will need to set the other variables in this block
# as follows:
#
#   EMAPP_LIB  Built-in directory containing
#              lisp and maybe site-lisp subdirectories
#   EMACS_LIB  System wide but local extension directory
#              containing site-lisp and lisp and info.
#   LOCAL_LIB  Users home emacs sub-directory
#   EMACS_INI  Directory containing this build
#
# Current settings are for Mac OS X assuming that
# /usr/local/share/emacs exists. I usually put
# links from EMAPP_LIB into /usr/local/share/emacs
# so that only the main directories are on the
# load path.

EMACS=/Applications/Emacs.app/Contents/MacOS/Emacs  
EMAPP_LIB=/Applications/Emacs.app/Contents/Resources/
EMACS_LIB=/usr/local/share/emacs/
LOCAL_LIB=$(HOME)/.emacs.d
EMACS_INI=$(HOME)/software/Emacs/Startup

# Both perl (v5) and python (v2.6) are used in the MakeWho and MakeTOC
# scripts, respectively, and when running the result of the MakeWho
# script. This should by default use the versions on your path, but if
# you need something different change these to the absolute pathnames.
# This will be adjusted to be more flexible in the next version.

PERL=perl
PYTHON=python


# (@* "ARCHIVE*")
# Specifies name and location of archive file and the archiving method
# and options to be used in doing 'make archive'.
# Set ARCHIVE (e.g., on the command line) to specify an arbitrary name,
# or change individual options selectively.

ARCHIVE_DIR  = /tmp/#                          # directory in which put the archive file
ARCHIVE_APP  = tar#                            # executable name for archiving method  
ARCHIVE_OPT  = zcf#                            # archiving options
ARCHIVE_NAME = crg-emacs-init.tgz
ARCHIVE      = $(ARCHIVE_DIR)$(ARCHIVE_NAME)
ARCHIVE_EXC  = --exclude './.*~' --exclude './.bonz' --wildcards
TODAY        = $(shell /bin/date +%d%b%Y)


# (@* "INSTALL*")
# Specifies the name and location of the installed emacs initialization file.
# Set INSTALL (e.g., on the command line) to specify an arbitrary name.
# Or you can specify the directory, root name, and suffix selectively.
# Note that by default INSTALL is set to drop the suffix if INSTALL_SFX
# exactly equals ".el". In this case, the installed file will be .emacs
# in INSTALL_DIR by default.

INSTALL_DIR  = $(HOME)/
INSTALL_NAME = .emacs
INSTALL_SUFX = .elc   # if this is .el exactly, INSTALL_SUFX suppressed in INSTALL
INSTALL      = $(INSTALL_DIR)$(INSTALL_NAME)$(if $(subst .el,,$(INSTALL_SUFX)),$(INSTALL_SUFX),)


################################# (@* "Part II")

CONFIG    = Config
WHOAMI    = WhoAmI
CORENAME  = crg-emacs-init
UTILITYEL = utility-header.el aliases.el predicate-funcs.el list-funcs.el macros.el name-funcs.el move-funcs.el op-funcs.el import-funcs.el tool-funcs.el setter-funcs.el
MYMODULES = imports.el text-modes.el prog-modes.el shell-modes.el nav-modes.el tex-modes.el aux.el ess-modes.el keybindings.el settings.el system-settings.el faces.el init.el
COREFILES = header.el toc.el $(UTILITYEL) whoami.el $(MYMODULES)


compiled: $(CORENAME).elc

elisp: $(CORENAME).el

install: $(CORENAME)$(INSTALL_SUFX)
	cp $(CORENAME)$(INSTALL_SUFX) $(INSTALL)

archive:
	$(ARCHIVE_APP) $(ARCHIVE_OPT) $(ARCHIVE) $(ARCHIVE_EXC) .

fullarchive:
	cd $(EMACS_INI); $(ARCHIVE_APP) $(ARCHIVE_OPT) $(ARCHIVE_DIR)emacs-init.tgz $(ARCHIVE_EXC) .
	cd $(EMAPP_LIB); $(ARCHIVE_APP) $(ARCHIVE_OPT) $(ARCHIVE_DIR)emacs-site.tgz info site-lisp
	cd $(EMACS_LIB); $(ARCHIVE_APP) $(ARCHIVE_OPT) $(ARCHIVE_DIR)emacs-share.tgz etc info site-lisp
	cd $(LOCAL_LIB)/..; $(ARCHIVE_APP) $(ARCHIVE_OPT) $(ARCHIVE_DIR)emacs-d.tgz .emacs.d
	cd $(ARCHIVE_DIR); tar cvf emacs-crg-$(TODAY).tar emacs-init.tgz emacs-site.tgz emacs-share.tgz emacs-d.tgz

clean:
	rm -i $(CORENAME).elc

fullclean:
	rm -i $(CORENAME).el*

$(CORENAME).el: $(COREFILES)
	cat $(COREFILES) > $@

whoami.el: $(WHOAMI) $(CONFIG)
	$(PERL) MakeWho < $(CONFIG) > tmp-config-sub
	$(PERL) -p tmp-config-sub < $< > $@
	rm -f tmp-config-sub

toc.el: header.el $(UTILITYEL) whoami.el $(MYMODULES)
	$(PYTHON) MakeTOC header.el $(UTILITYEL) whoami.el $(MYMODULES) > $@

.el.elc:
	$(EMACS) -q -batch --eval="(setq byte-compile-warnings '(not free-vars))" -f batch-byte-compile $<

