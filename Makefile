#
# $Id$
#

VERSION = 0.87

SHELL	= /bin/sh
MAKE	= make
CC	= gcc
CFLAGS	= -O2
RM	= /bin/rm -f
CP	= /bin/cp -p
EMACS	= emacs

GOMI	= *.elc
FLAGS   = -batch -q -no-site-file

PREFIX	= NONE
EXEC_PREFIX = NONE
LISPDIR =

FILES =	Makefile SEMI-MK SEMI-CFG SEMI-ELS *.el ChangeLog

elc:
	$(EMACS) $(FLAGS) -l SEMI-MK -f compile-semi \
		$(PREFIX) $(EXEC_PREFIX) $(LISPDIR)

install-elc:	elc
	$(EMACS) $(FLAGS) -l SEMI-MK -f install-semi \
		$(PREFIX) $(EXEC_PREFIX) $(LISPDIR)


all:	$(UTILS) $(DVI) elc

tex:	ol2
	cd doc; $(MAKE) tex

dvi:	ol2
	cd doc; $(MAKE) dvi

ps:	ol2
	cd doc; $(MAKE) ps


install:	install-elc install-execs

execs:	$(UTILS)

install-execs:
	$(EMACS) $(FLAGS) -l SEMI-MK -f install-execs $(PREFIX) $(EXEC_PREFIX)


update-xemacs:
	$(EMACS) $(FLAGS) -l SEMI-MK -f update-xemacs-source


clean:
	-$(RM) $(GOMI)
	-cd doc   && $(MAKE) clean
	-cd gnus  && $(MAKE) clean
	-cd mh-e  && $(MAKE) clean
	cd ../mel && $(MAKE) clean


tar:
	gtar cvf ../semi-$(VERSION).tar $(FILES)
	-cd ..; mkdir semi-$(VERSION)
	cp ../semi-kernel/README.?? ../semi-$(VERSION)
	cp ../semi-kernel/Makefile ../semi-$(VERSION)
	cd ../semi-$(VERSION); gtar xvf ../semi-$(VERSION).tar
	cd ..; gtar cvzf semi-$(VERSION).tar.gz semi-$(VERSION)
	cd ..; $(RM) -r semi-$(VERSION); rm semi-$(VERSION).tar
