#
# Makefile for WEMI kernel.
#

PACKAGE = wemi
VERSION = 1.10.0

TAR	= tar
RM	= /bin/rm -f
CP	= /bin/cp -p

EMACS	= emacs
XEMACS	= xemacs
FLAGS   = -batch -q -no-site-file -l SEMI-MK

PREFIX	= NONE
EXEC_PREFIX = NONE
LISPDIR = NONE
PACKAGEDIR = NONE
VERSION_SPECIFIC_LISPDIR = NONE

GOMI	= *.elc


elc:
	$(EMACS) $(FLAGS) -f compile-semi \
		$(PREFIX) $(EXEC_PREFIX) $(LISPDIR) \
		$(VERSION_SPECIFIC_LISPDIR)

install-elc:	elc
	$(EMACS) $(FLAGS) -f install-semi \
		$(PREFIX) $(EXEC_PREFIX) $(LISPDIR) \
		$(VERSION_SPECIFIC_LISPDIR)

install:	install-elc


package:
	$(XEMACS) $(FLAGS) -f compile-semi-package $(PACKAGEDIR)

install-package:	package
	$(XEMACS) $(FLAGS) -f install-semi-package $(PACKAGEDIR)


clean:
	-$(RM) $(GOMI)


tar:
	cvs commit
	sh -c 'cvs tag -RF $(PACKAGE)-`echo $(VERSION) \
				| sed s/\\\\./_/ | sed s/\\\\./_/`; \
	cd /tmp; \
	cvs -d :pserver:anonymous@chamonix.jaist.ac.jp:/hare/cvs/root \
		export -d $(PACKAGE)-$(VERSION) \
		-r $(PACKAGE)-`echo $(VERSION) \
			| sed s/\\\\./_/ | sed s/\\\\./_/` semi'
	$(RM) /tmp/$(PACKAGE)-$(VERSION)/ftp.in
	cd /tmp; $(TAR) cvzf $(PACKAGE)-$(VERSION).tar.gz $(PACKAGE)-$(VERSION)
	cd /tmp; $(RM) -r $(PACKAGE)-$(VERSION)
	sed "s/VERSION/$(VERSION)/" < ftp.in \
		| sed "s/PACKAGE/$(PACKAGE)/" > ftp
