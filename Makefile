#
# $Id$
#

VERSION = 0.70

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

SEMI_FILES =	semi/Makefile semi/SEMI-MK semi/SEMI-CFG semi/SEMI-ELS \
		semi/*.el semi/ChangeLog

MEL_FILES =	mel/Makefile mel/MEL-MK mel/MEL-CFG mel/MEL-ELS \
		mel/*.el mel/ChangeLog

MU_FILES =	mu/Makefile mu/MU-MK mu/MU-CFG mu/MU-ELS \
		mu/*.el mu/ChangeLog

APEL_FILES =	apel/Makefile apel/APEL-MK apel/APEL-CFG apel/APEL-ELS \
		apel/*.el apel/ChangeLog

BITMAP_FILES =	bitmap-mule/Makefile bitmap-mule/BITMAP-MK \
		bitmap-mule/BITMAP-CFG bitmap-mule/BITMAP-ELS \
		bitmap-mule/*.el bitmap-mule/*.bdf \
		bitmap-mule/README.* bitmap-mule/ChangeLog

EMU_FILES =	emu/Makefile emu/EMU-MK emu/EMU-CFG emu/EMU-ELS \
		emu/*.el emu/ChangeLog

SINFO_FILES =	sinfo/Makefile sinfo/SINFO-MK \
		sinfo/SINFO-CFG sinfo/SINFO-ELS \
		sinfo/*.dtd sinfo/*.el sinfo/*-mapping sinfo/ChangeLog

FILES	=	$(SEMI_FILES) $(MEL_FILES) $(MU_FILES) \
		$(APEL_FILES) $(BITMAP_FILES) $(EMU_FILES) $(SINFO_FILES)

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


tar:
	cd ..; gtar cvf semi-$(VERSION).tar $(FILES)
	-cd ..; mkdir semi-$(VERSION)
	cp ../semi-kernel/README.?? ../semi-$(VERSION)
	cp ../semi-kernel/Makefile ../semi-$(VERSION)
	cd ../semi-$(VERSION); gtar xvf ../semi-$(VERSION).tar
	cd ..; gtar cvzf semi-$(VERSION).tar.gz semi-$(VERSION)
	cd ..; $(RM) -r semi-$(VERSION); rm semi-$(VERSION).tar
