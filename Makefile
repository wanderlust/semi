#
# Makefile for SEMI kernel.
#

PACKAGE = semi
API	= 1.13
RELEASE = 6

FLIM_API= 1.13

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

GOMI	= *.elc

VERSION	= $(API).$(RELEASE)
ARC_DIR = /pub/mule/semi/semi-$(API)-for-flim-$(FLIM_API)


elc:
	$(EMACS) $(FLAGS) -f compile-semi \
		$(PREFIX) $(LISPDIR) $(VERSION_SPECIFIC_LISPDIR)

install-elc:	elc
	$(EMACS) $(FLAGS) -f install-semi \
		$(PREFIX) $(LISPDIR) $(VERSION_SPECIFIC_LISPDIR)

install:	install-elc


package:
	$(XEMACS) $(FLAGS) -f compile-semi-package $(PACKAGEDIR)

install-package:	package
	$(XEMACS) $(FLAGS) -f install-semi-package $(PACKAGEDIR)


clean:
	-$(RM) $(GOMI)


tar:
	cvs commit
	sh -c 'cvs tag -RF $(PACKAGE)-`echo $(VERSION) | tr . _`; \
	cd /tmp; \
	cvs -d :pserver:anonymous@chamonix.jaist.ac.jp:/hare/cvs/root \
		export -d $(PACKAGE)-$(VERSION) \
		-r $(PACKAGE)-`echo $(VERSION) | tr . _` \
		semi'
	$(RM) /tmp/$(PACKAGE)-$(VERSION)/ftp.in
	cd /tmp; $(TAR) cvzf $(PACKAGE)-$(VERSION).tar.gz $(PACKAGE)-$(VERSION)
	cd /tmp; $(RM) -r $(PACKAGE)-$(VERSION)
	sed -e "s/VERSION/$(VERSION)/" -e "s/API/$(API)/" \
		-e "s/PACKAGE/$(PACKAGE)/" \
		-e "s/FLIM_API/$(FLIM_API)/" < ftp.in > ftp

invoice:
	sed -e "s/VERSION/$(VERSION)/" -e "s/API/$(API)/" \
		-e "s/PACKAGE/$(PACKAGE)/" \
		-e "s/FLIM_API/$(FLIM_API)/" < ftp.in > ftp

release:
	-$(RM) $(ARC_DIR)/$(PACKAGE)-$(VERSION).tar.gz
	mv /tmp/$(PACKAGE)-$(VERSION).tar.gz $(ARC_DIR)
