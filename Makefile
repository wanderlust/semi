#
# Makefile for SEMI kernel.
#

VERSION = 1.1.1

SHELL	= /bin/sh
MAKE	= make
CC	= gcc
CFLAGS	= -O2
TAR	= tar
RM	= /bin/rm -f
CP	= /bin/cp -p
EMACS	= emacs

GOMI	= *.elc
FLAGS   = -batch -q -no-site-file

PREFIX	= NONE
EXEC_PREFIX = NONE
LISPDIR = NONE

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


install:	install-elc

update-xemacs:
	$(EMACS) $(FLAGS) -l SEMI-MK -f update-xemacs-source


clean:
	-$(RM) $(GOMI)
	-cd doc   && $(MAKE) clean
	-cd gnus  && $(MAKE) clean
	-cd mh-e  && $(MAKE) clean
	cd ../mel && $(MAKE) clean


tar:
	cvs commit
	sh -c 'cvs tag -RF semi-`echo $(VERSION) \
				| sed s/\\\\./_/ | sed s/\\\\./_/`; \
	cd /tmp; \
	cvs -d :pserver:anonymous@chamonix.jaist.ac.jp:/hare/cvs/root \
		export -d semi-$(VERSION) \
		-r semi-`echo $(VERSION) \
			| sed s/\\\\./_/ | sed s/\\\\./_/` semi'
	$(RM) /tmp/semi-$(VERSION)/ftp.in
	cd /tmp; $(TAR) cvzf semi-$(VERSION).tar.gz semi-$(VERSION)
	cd /tmp; $(RM) -r semi-$(VERSION)
	sed "s/VERSION/$(VERSION)/" < ftp.in > ftp
