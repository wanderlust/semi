#
# $Id$
#

VERSION = 7.105

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
EXEC_PREFIX =

SEMI_FILES = semi/README.en semi/ChangeLog \
		semi/Makefile semi/SEMI-MK semi/SEMI-ELS semi/SEMI-CFG \
		semi/tm-def.el \
		semi/tm-eword.el semi/tm-ew-d.el semi/tm-ew-e.el \
		semi/tm-view.el semi/tm-parse.el \
		semi/tm-text.el semi/tm-image.el \
		semi/tm-play.el semi/tm-partial.el semi/tm-pgp.el \
		semi/tm-ftp.el semi/tm-latex.el \
		semi/tm-html.el semi/tm-file.el \
		semi/tm-tar.el \
		semi/tm-bbdb.el \
		semi/tm-edit*.el semi/signature.el \
		semi/tm-setup.el semi/mime-setup.el.in \
		semi/sc-setup.el \
		semi/methods/tm* semi/methods/ChangeLog \
		semi/doc/*.sgml semi/doc/*.texi semi/doc/*.info \
		semi/doc/Makefile semi/doc/*.ol semi/doc/*.tex \
		semi/old-logs/*.en semi/old-logs/*.ja

TM_MH_E_FILES =	tm-mh-e/Makefile tm-mh-e/mk-tmh tm-mh-e/TMH-ELS \
		tm-mh-e/*.el tm-mh-e/ChangeLog \
		tm-mh-e/*.sgml tm-mh-e/*.texi tm-mh-e/*.info

GNUS_MIME_FILES = gnus-mime/Makefile gnus-mime/*-path \
		gnus-mime/Gnus-MIME-* gnus-mime/*.el \
		gnus-mime/*.sgml gnus-mime/*.texi gnus-mime/*.info \
		gnus-mime/ChangeLog

TM_GNUS_FILES =	tm-gnus/Makefile tm-gnus/*-path tm-gnus/mk-tgnus \
		tm-gnus/TGNUS-ELS tm-gnus/*.el tm-gnus/ChangeLog \
		tm-gnus/*.ol tm-gnus/*.texi tm-gnus/*.info tm-gnus/*.tex

TM_MAIL_FILES = tm-mail/TMAIL-ELS tm-mail/*.el tm-mail/ChangeLog

TM_VM_FILES =	tm-vm/TM-VM-ELS tm-vm/*.el tm-vm/*.texi tm-vm/*.info \
		tm-vm/ChangeLog

TM_MUA_FILES =	$(TM_MH_E_FILES) $(GNUS_MIME_FILES) $(TM_GNUS_FILES) \
		$(TM_MAIL_FILES) $(TM_VM_FILES)

MEL_FILES = mel/*.el mel/Makefile mel/mk-mel mel/MEL-ELS mel/ChangeLog

MU_FILES =	mu/MU-ELS mu/*.el mu/ChangeLog

TL_FILES =	tl/README.en tl/Makefile tl/mk-tl tl/TL-ELS \
		tl/*.el tl/ChangeLog

BITMAP_FILES =	bitmap-mule/BITMAP-* bitmap-mule/Makefile \
		bitmap-mule/*.el bitmap-mule/*.bdf \
		bitmap-mule/README.* bitmap-mule/ChangeLog

EMU_FILES =	emu/EMU-ELS emu/*.el emu/ChangeLog

SINFO_FILES =	sinfo/SINFO-* sinfo/Makefile \
		sinfo/*.dtd sinfo/*.el sinfo/*-mapping sinfo/ChangeLog

FILES	= $(SEMI_FILES) $(TM_MUA_FILES) $(MEL_FILES) $(MU_FILES) \
		$(TL_FILES) $(BITMAP_FILES) $(EMU_FILES) $(SINFO_FILES)

elc:
	$(EMACS) $(FLAGS) -l SEMI-MK -f compile-semi $(PREFIX) $(EXEC_PREFIX)

install-elc:	elc
	$(EMACS) $(FLAGS) -l SEMI-MK -f install-semi $(PREFIX) $(EXEC_PREFIX)


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


oomori:
	cd doc; $(MAKE) tex
	cd ..; gtar cvf tm-$(VERSION).tar $(FILES)
	-cd ..; mkdir tm-$(VERSION)
	cp ../tm-oomori/README.?? ../tm-$(VERSION)
	cd ../tm-$(VERSION); gtar xvf ../tm-$(VERSION).tar
	cd ..; gtar cvzf tm-$(VERSION).tar.gz tm-$(VERSION)
	cd ..; $(RM) -r tm-$(VERSION); rm tm-$(VERSION).tar


release:
	cd ..; mv tm$(VERSION).tar.gz /pub/GNU/elisp/mime/beta/
	-cd /pub/GNU/elisp/mime/beta/ ; rm tm-oomori-current.tar.gz
	cd /pub/GNU/elisp/mime/beta/ ; ln -s tm$(VERSION).tar.gz tm-oomori-current.tar.gz
