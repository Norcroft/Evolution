INSTALLDIR = /usr/local/ncc.lib
CC = $(HOME)/bin/ncc
#CFLAGS = -J. -D__unix_lib
CFLAGS = -D__unix_lib

# unix-sbrk.c is here to stop two sbrk's getting loaded if the user uses.

SOURCES = ctype.c error.c fpprintf.c locale.c math.c printf.c \
	scanf.c sort.c stdlib.c string.c stdio.c sparcsys.c alloc.c \
	sbrk.c unix-sbrk.c raise.c jpff-signal.c time.c getenv.c system.c \
	remove.c checks.c
# xmath.c

SOBJ =  mul.o prog8.o sparccall.o sparcfork.o setjmp.o \
        ctype.o error.o fpprintf.o locale.o math.o printf.o \
	scanf.o sort.o stdlib.o string.o stdio.o alloc.o sparcsys.o \
	sbrk.o unix-sbrk.o raise.o jpff-signal.o \
	time.o getenv.o system.o remove.o checks.o
# xmath.o

XSRCS =	getuid.c isatty.c
XOBJS = getuid.o isatty.o

libAnsi.a:	Ncrt.sparc.o $(SOBJ) $(XOBJS)
	ar crv libAnsi.a $(SOBJ) $(XOBJS)
	ranlib libAnsi.a

libUnix.a:	$(XOBJS)
	ar crv libUnix.a $(XOBJS)
	ranlib libUnix.a

.c.o:	hostsys.h
	${CC} ${CFLAGS} -c $*.c

.s.o:
	${CC} ${CFLAGS} -c $*.s

tidy:
	rm Ncrt.sparc.o libAnsi.a ${SOBJ} $(XOBJS)

cmp:
	cmp -l Ncrt.sparc.o $(INSTALLDIR)/Ncrt0.o
	cmp -l libAnsi.a    $(INSTALLDIR)/lib.a

install:	libAnsi.a Ncrt.sparc.o
	mkdir -p BACKUP
	really make installsu

installsu:
	mv $(INSTALLDIR)/Ncrt0.o  $(INSTALLDIR)/lib.a   BACKUP
	cp Ncrt.sparc.o $(INSTALLDIR)/Ncrt0.o
	cp libAnsi.a    $(INSTALLDIR)/lib.a
	ranlib          $(INSTALLDIR)/lib.a

#          /***************************************************/
#          /*                                                 */
#          /*    Define UN*Xmake dependencies                 */
#          /*                                                 */
#          /***************************************************/

depend:
	  -rm makedep
	  for i in ${SOURCES}; do \
	      ${CC} ${OPTS} -M ${CFLAGS} $$i | \
	      awk ' { if ($$1 != prev) \
		  { if (rec != "") print rec; rec = $$0; prev = $$1; } \
		  else { if (length(rec $$2) > 78) { print rec; rec = $$0; } \
		  else rec = rec " " $$2 } } \
		  END { print rec } ' >> makedep; done
	  cp Makefile makefile
	  echo '# DO NOT DELETE THIS LINE' >> makefile
	  cat makedep >> makefile
	  echo '# DEPENDENCIES MUST END AT END OF FILE' >> makefile
	  echo '# IF YOU PUT STUFF HERE IT WILL GO AWAY' >> makefile
	  echo '# see make depend above' >> makefile

# DO NOT DELETE THIS LINE
