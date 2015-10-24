CASK       ?= cask
EMACS      ?= emacs
DIST       ?= dist
EMACSFLAGS  = --batch -Q
EMACSBATCH  = $(EMACS) $(EMACSFLAGS)

VERSION    := $(shell EMACS=$(EMACS) $(CASK) version)
PKG_DIR    := $(shell EMACS=$(EMACS) $(CASK) package-directory)
PROJ_ROOT  := $(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))

EMACS_D     = ~/.emacs.d
USER_ELPA_D = $(EMACS_D)/elpa

SRCS        = state.el
TESTS       = $(wildcard tests/*.el)
TAR         = $(DIST)/state-$(VERSION).tar


.PHONY: all install uninstall reinstall clean-all clean
all : $(PKG_DIR) $(TAR)

install : $(TAR)
	$(EMACSBATCH) -l package -f package-initialize \
	--eval '(package-install-file "$(PROJ_ROOT)/$(TAR)")'

uninstall :
	rm -rf $(USER_ELPA_D)/state-*

test:
	${CASK} exec ${EMACS} ${EMACSFLAGS} -l state-tests.el

reinstall : clean uninstall install

clean-all : clean
	rm -rf $(PKG_DIR)

clean :
	rm -f *.elc
	rm -rf $(DIST)
	rm -f *-pkg.el

$(PKG_DIR) : Cask
	$(CASK) install
	touch $(PKG_DIR)

$(TAR) : $(DIST)
	$(CASK) package

$(DIST) :
	mkdir $(DIST)
