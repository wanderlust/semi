#
# Makefile for SEMI kernel.
#

PACKAGE = semi
API	= 1.13
RELEASE = 7

FLIM_API= 1.13

TAR	= tar
RM	= /bin/rm -f
CP	= /bin/cp -p

EMACS	= emacs
XEMACS	= xemacs
VANILLA = -batch -q -no-site-file
FLAGS   = $(VANILLA) -l SEMI-MK
TEXINFMT = $(VANILLA) -l texinfmt
TEXIF = -f texinfo-format-buffer -f save-buffer

PREFIX	= NONE
LISPDIR = NONE
PACKAGEDIR = NONE
VERSION_SPECIFIC_LISPDIR = NONE

GOMI	= *.elc

VERSION	= $(API).$(RELEASE)
ARC_DIR = /pub/mule/semi/semi-$(API)-for-flim-$(FLIM_API)

all: elc info

elc:
	$(EMACS) $(FLAGS) -f compile-semi \
		$(PREFIX) $(LISPDIR) $(VERSION_SPECIFIC_LISPDIR)

install-elc: elc
	$(EMACS) $(FLAGS) -f install-semi \
		$(PREFIX) $(LISPDIR) $(VERSION_SPECIFIC_LISPDIR)

install: install-elc


package: package-elc info

package-elc:
	$(XEMACS) $(FLAGS) -f compile-semi-package $(PACKAGEDIR)

install-package:	package
	$(XEMACS) $(FLAGS) -f install-semi-package $(PACKAGEDIR)


info: emy.info

%.info: %.texi
	makeinfo -o $@ $<

texinfmt: emy.texi
	$(EMACS) $(TEXINFMT) emy.texi $(TEXIF)

xtexinfmt: emy.texi
	$(XEMACS) $(TEXINFMT) emy.texi $(TEXIF)

clean:
	-$(RM) $(GOMI)

distclean: clean
	-$(RM) *.info

tar:
	cvs commit
	sh -c 'cvs tag -RF $(PACKAGE)-`echo $(VERSION) | tr . _`; \
	cd /tmp; \
	cvs -d :pserver:anonymous@cvs.m17n.org:/cvs/root \
		export -d $(PACKAGE)-$(VERSION) \
		-r $(PACKAGE)-`echo $(VERSION) | tr . _` \
		semi'
	$(RM) /tmp/$(PACKAGE)-$(VERSION)/ftp.in
	cd /tmp; $(TAR) cvzf $(PACKAGE)-$(VERSION).tar.gz $(PACKAGE)-$(VERSION)
	cd /tmp; $(RM) -r $(PACKAGE)-$(VERSION)
	sed "s/VERSION/$(VERSION)/" < ftp.in | sed "s/API/$(API)/" \
		| sed "s/PACKAGE/$(PACKAGE)/" \
		| sed "s/FLIM_API/$(FLIM_API)/" > ftp

release:
	-$(RM) $(ARC_DIR)/$(PACKAGE)-$(VERSION).tar.gz
	mv /tmp/$(PACKAGE)-$(VERSION).tar.gz $(ARC_DIR)
