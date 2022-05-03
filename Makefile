#
# Makefile for SEMI kernel.
#

PACKAGE = semi
API	= 1.14
RELEASE = 6

FLIM_API= 1.14

TAR	= tar
RM	= /bin/rm -f
CP	= /bin/cp -p

EMACS	= emacs
XEMACS	= xemacs
FLAGS   = -batch -q -no-site-file -l SEMI-MK

PREFIX	= NONE
LISPDIR = NONE
PACKAGEDIR = NONE
VERSION_SPECIFIC_LISPDIR = NONE
PACKAGE_LISPDIR = package-user-dir

GOMI	= *.elc

VERSION	= $(API).$(RELEASE)
ARC_DIR_PREFIX = /home/kanji/tomo/public_html/comp/emacsen/lisp
ARC_DIR = $(ARC_DIR_PREFIX)/semi/semi-$(API)-for-flim-$(FLIM_API)


elc:
	$(EMACS) $(FLAGS) -f compile-semi $(PREFIX) $(LISPDIR) \
		$(VERSION_SPECIFIC_LISPDIR) $(PACKAGE_LISPDIR)

install-elc:	elc
	$(EMACS) $(FLAGS) -f install-semi $(PREFIX) $(LISPDIR) \
		$(VERSION_SPECIFIC_LISPDIR) $(PACKAGE_LISPDIR)

install:	install-elc


clean:
	-$(RM) $(GOMI)

release:
	-$(RM) $(ARC_DIR)/$(PACKAGE)-$(VERSION).tar.gz
	mv /tmp/$(PACKAGE)-$(VERSION).tar.gz $(ARC_DIR)
