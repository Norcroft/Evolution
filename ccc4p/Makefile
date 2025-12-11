.SUFFIXES:
.SUFFIXES: .o .c
#
# Change the following symbols to suit your system.
# CC is the C compiler used to compile the compiler.
# LD is the linker used
# CLIB is the C library
# CFLAGS are the compiler flags with which to compile each source.
# LINKFLAGS are typically unused (passed to the linker).
#

CC = gcc -O
LD = gcc
NEW = ./c4pncc
NEWC = ./c4pcoffncc

# LIB = /local/ncc.lib/muldiv.a
LIB =
#VSN= -DNON_RELEASE_VSN='"0.02 (Codemist Ltd)"'
VSN=
CFLAGS = $(VSN) -D__unix -I. -I../c4p -I../mip -I../cfe -c
AFLAGS = $(VSN) -D__unix -I. -I../c4p -I../mip -I../cfe -S

LINKFLAGS=
AH=/usr/local/ncc.include/UNUSED_HERE
PH=../mip/
PC=../mip/
UC=../util/
MH=../c4p/
MC=../c4p/
LH=../cfe/
LPPH=../cfe/
LC=../cfe/
XH=
XC=

ANSIHDRS =
# ANSIHDRS = \
# $(AH)assert.h $(AH)ctype.h  $(AH)errno.h  $(AH)float.h  $(AH)limits.h \
# $(AH)locale.h $(AH)math.h   $(AH)setjmp.h $(AH)signal.h $(AH)stdarg.h \
# $(AH)stddef.h $(AH)stdio.h  $(AH)stdlib.h $(AH)string.h $(AH)time.h

HDRFILES =
# HDRFILES = \
# assert.h ctype.h  errno.h  float.h limits.h locale.h math.h setjmp.h \
# signal.h stdarg.h stddef.h stdio.h stdlib.h string.h time.h

SOURCES =   ${MC}gen.c \
	    ${MC}mcdep.c \
	    ${MC}asm.c \
	    ${PC}elfobj.c \
	    ${PC}coffobj.c \
	    ${PC}aetree.c \
	    ${PC}bind.c \
	    ${PC}builtin.c \
	    ${PC}cg.c \
	    ${PC}codebuf.c \
	    ${PC}compiler.c \
	    ${PC}cse.c \
	    ${PC}csescan.c \
	    ${PC}dbx.c \
	    ${PC}driver.c \
	    ${PC}flowgraf.c \
	    ${PC}fname.c \
	    ${PC}ieeeflt.c \
	    ${PC}jopprint.c \
	    ${PC}misc.c \
	    ${PC}regalloc.c \
	    ${PC}regsets.c \
	    ${PC}sr.c \
	    ${PC}store.c \
	    ${PC}version.c \
	    ${LC}lex.c \
	    ${LC}pp.c \
	    ${LC}sem.c \
	    ${LC}simplify.c \
	    ${LC}syn.c \
	    ${LC}vargen.c

OBJS = asm.o gen.o mcdep.o dbx.o \
	bind.o builtin.o cg.o codebuf.o cse.o csescan.o driver.o fname.o \
	flowgraf.o ieeeflt.o jopprint.o lex.o \
	misc.o pp.o regalloc.o regsets.o sem.o simplify.o sr.o store.o \
	compiler.o syn.o aetree.o vargen.o version.o

${NEW}:	${OBJS} elfobj.o
	-mv ${NEW} ${NEW}.old
	${LD} -o ${NEW} ${LINKFLAGS} ${OBJS} elfobj.o ${LIB}
	rm version.o

${NEWC}:	${OBJS} coffobj.o
	-mv ${NEWC} ${NEWC}.old
	${LD} -o ${NEWC} ${LINKFLAGS} ${OBJS} coffobj.o ${LIB}
	rm version.o

tar:
	tar zcf /home/am/c4p`date '+%y%m%d'`.tgz \
	   Makefile options.h $(LC). $(PC). $(MC). $(UC).

TAGS:	$(SOURCES) ${LH}fevsn.h ${LH}lex.h ${LH}pp.h ${LH}sem.h \
	${LH}simplify.h ${LH}syn.h ${LH}vargen.h $(MH)mcdpriv.h \
	$(MH)mcvsn.h $(MH)ops.h $(MH)target.h $(PH)aeops.h \
	$(PH)aetree.h $(PH)aout.h $(PH)bind.h $(PH)builtin.h \
	$(PH)cg.h $(PH)cgdefs.h $(PH)codebuf.h $(PH)compiler.h \
	$(PH)cse.h $(PH)cseguts.h $(PH)defaults.h $(PH)defs.h \
	$(PH)drivhelp.h $(PH)flowgraf.h $(PH)fname.h $(PH)globals.h \
	$(PH)host.h $(PH)jopcode.h $(PH)mcdep.h $(PH)mipvsn.h \
	$(PH)regalloc.h $(PH)regsets.h $(PH)sr.h $(PH)store.h $(PH)util.h \
	$(PH)version.h $(PH)xrefs.h errors.h options.h
	etags -t $(SOURCES) \
		${LH}fevsn.h ${LH}lex.h ${LH}pp.h ${LH}sem.h \
		${LH}simplify.h ${LH}syn.h ${LH}vargen.h $(MH)mcdpriv.h \
		$(MH)mcvsn.h $(MH)ops.h $(MH)target.h $(PH)aeops.h \
		$(PH)aetree.h $(PH)aout.h $(PH)bind.h $(PH)builtin.h \
		$(PH)cg.h $(PH)cgdefs.h $(PH)codebuf.h $(PH)compiler.h \
		$(PH)cse.h $(PH)cseguts.h $(PH)defaults.h $(PH)defs.h \
		$(PH)drivhelp.h $(PH)flowgraf.h $(PH)fname.h $(PH)globals.h \
		$(PH)host.h $(PH)jopcode.h $(PH)mcdep.h $(PH)mipvsn.h \
		$(PH)regalloc.h $(PH)regsets.h $(PH)sr.h $(PH)store.h \
		$(PH)util.h $(PH)version.h $(PH)xrefs.h errors.h options.h

#
# Machine-specific things...
#

elfobj.o:
		${CC} ${CFLAGS} ${PC}elfobj.c

coffobj.o:
		${CC} ${CFLAGS} ${PC}coffobj.c

asm.o:
		${CC} ${CFLAGS} ${MC}asm.c

gen.o:
		${CC} ${CFLAGS} ${MC}gen.c

mcdep.o:
		${CC} ${CFLAGS} ${MC}mcdep.c

dbx.o:
		${CC} ${CFLAGS} ${PC}dbx.c


#
# Machine- and language- independent things
#

aetree.o:
		${CC} ${CFLAGS} ${PC}aetree.c

bind.o:
		${CC} ${CFLAGS} ${PC}bind.c

builtin.o:
		${CC} ${CFLAGS} ${PC}builtin.c

cg.o:
		${CC} ${CFLAGS} ${PC}cg.c

codebuf.o:
		${CC} ${CFLAGS} ${PC}codebuf.c

cse.o:
		${CC} ${CFLAGS} ${PC}cse.c

csescan.o:
		${CC} ${CFLAGS} ${PC}csescan.c

compiler.o:
		${CC} ${CFLAGS} ${PC}compiler.c

driver.o:
		${CC} ${CFLAGS} ${PC}driver.c

fname.o:
		${CC} ${CFLAGS} ${PC}fname.c

flowgraf.o:
		${CC} ${CFLAGS} ${PC}flowgraf.c

ieeeflt.o:
		${CC} ${CFLAGS} ${PC}ieeeflt.c

jopprint.o:
		${CC} ${CFLAGS} ${PC}jopprint.c

misc.o:
		${CC} ${CFLAGS} ${PC}misc.c

regalloc.o:
		${CC} ${CFLAGS} ${PC}regalloc.c

regsets.o:
		${CC} ${CFLAGS} ${PC}regsets.c

sr.o:
		${CC} ${CFLAGS} ${PC}sr.c

store.o:
		${CC} ${CFLAGS} ${PC}store.c

version.o:
		${CC} ${CFLAGS} ${PC}version.c

#
# Language-dependent things
#

lex.o:
		${CC} ${CFLAGS} ${LC}lex.c

pp.o:
		${CC} ${CFLAGS} ${LC}pp.c

sem.o:
		${CC} ${CFLAGS} ${LC}sem.c

simplify.o:
		${CC} ${CFLAGS} ${LC}simplify.c

syn.o:
		${CC} ${CFLAGS} ${LC}syn.c

vargen.o:
		${CC} ${CFLAGS} ${LC}vargen.c

#------------------------------------------------------------------------

haldanec: ${SOURCES} ${PH}*.h ${MH}*.h ${LH}*.h ${XH}*.h
	rcp ${SOURCES} ${PH}*.h ${MH}*.h ${LH}*.h ${XH}*.h haldane:SUN/Norcroft

#------------------------------------------------------------------------

headers.c:	$(ANSIHDRS) genhdrs
		genhdrs -oheaders.c -I$(AH) $(HDRFILES)

errors.h:	genhdrs ${PH}miperrs.h ${LPPH}feerrs.h ${MH}mcerrs.h
		genhdrs -eerrors.h -s \
			-q${PH}miperrs.h -q${LPPH}feerrs.h -q${MH}mcerrs.h

genhdrs:	${UC}genhdrs.c
		${CC} -c -o genhdrs.o ${UC}genhdrs.c
		${LD} -o genhdrs genhdrs.o

#	   /***************************************************/
#	   /*						      */
#	   /*	 Define UN*Xmake dependencies		      */
#	   /*						      */
#	   /***************************************************/

depend:
#	  for j in ${ARMSRC}/*.[ch]; do \
#	      echo $$j :  ${REMOTE}/arm/`basename $$j` >> makedep ;\
#	      echo -n '	cp -i ${REMOTE}/arm/' >> makedep ;\
#	      echo `basename $$j` $$j  >> makedep; \
#	   done
#	  for j in ${PC}/*.[ch]; do \
#	      echo $$j :  ${REMOTE}/mip/`basename $$j` >> makedep ;\
#	      echo -n '	cp -i ${REMOTE}/mip/' >> makedep ;\
#	      echo `basename $$j` $$j  >> makedep; \
#	  done
#	  for j in ${LC}/*.[ch]; do \
#	      echo $$j :  ${REMOTE}/cfe/`basename $$j` >> makedep ;\
#	      echo -n '	cp -i ${REMOTE}/cfe/' >> makedep ;\
#	      echo `basename $$j` $$j  >> makedep; \
#	  done
#	  for j in ${OPTS}/*.[ch]; do \
#	      echo $$j :  ${REMOTE}/cchelios/`basename $$j` >> makedep ;\
#	      echo -n '	cp -i ${REMOTE}/cchelios/' >> makedep ;\
#	      echo `basename $$j` $$j  >> makedep; \
#	  done
	  rm -f makedep
	  for i in ${SOURCES}; do \
	      ${CC} -MX ${CFLAGS} $$i | \
	      awk ' { if ($$1 != prev) \
		  { if (rec != "") print rec; rec = $$0; prev = $$1; } \
		  else { if (length(rec $$2) > 78) { print rec; rec = $$0; } \
		  else rec = rec " " $$2 } } \
		  END { print rec } ' >> makedep; done
	  cp Makefile makefile
	  echo '# DO NOT DELETE THIS LINE' >> makefile
	  echo ' ' >> makefile
	  cat makedep >> makefile
	  echo '# DEPENDENCIES MUST END AT END OF FILE' >> makefile
	  echo '# IF YOU PUT STUFF HERE IT WILL GO AWAY' >> makefile
	  echo '# see make depend above' >> makefile

# DO NOT DELETE THIS LINE -- make depend uses it

gen.o:	../c4p/gen.c ../mip/globals.h ../mip/host.h options.h ../c4p/target.h
gen.o:	../mip/defaults.h ../mip/defs.h ../mip/builtin.h ../mip/mcdep.h
gen.o:	../mip/cgdefs.h ../mip/jopcode.h ../c4p/mcdpriv.h ../mip/xrefs.h
gen.o:	../c4p/ops.h ../mip/jopcode.h ../mip/store.h ../mip/codebuf.h
gen.o:	../mip/xrefs.h ../mip/regalloc.h ../mip/cg.h ../mip/bind.h ../mip/util.h
gen.o:	errors.h
mcdep.o:	../c4p/mcdep.c ../mip/globals.h ../mip/host.h options.h
mcdep.o:	../c4p/target.h ../mip/defaults.h ../mip/defs.h ../mip/mcdep.h
mcdep.o:	../mip/cgdefs.h ../mip/jopcode.h ../c4p/mcdpriv.h
asm.o:	../c4p/asm.c ../mip/globals.h ../mip/host.h options.h ../c4p/target.h
asm.o:	../mip/defaults.h ../mip/defs.h ../mip/mcdep.h ../mip/cgdefs.h
asm.o:	../mip/jopcode.h ../mip/xrefs.h ../mip/store.h ../mip/codebuf.h
asm.o:	../mip/xrefs.h ../c4p/ops.h ../c4p/mcdpriv.h ../mip/builtin.h
asm.o:	../mip/bind.h ../mip/aeops.h ../cfe/sem.h ../mip/version.h errors.h
asm.o:	../mip/regalloc.h
elfobj.o:	../mip/elfobj.c ../mip/globals.h ../mip/host.h options.h
elfobj.o:	../c4p/target.h ../mip/defaults.h ../mip/defs.h ../mip/mcdep.h
elfobj.o:	../mip/cgdefs.h ../mip/jopcode.h ../mip/store.h ../mip/codebuf.h
elfobj.o:	../mip/xrefs.h ../mip/elf.h
coffobj.o:	../mip/coffobj.c ../mip/globals.h ../mip/host.h options.h
coffobj.o:	../c4p/target.h ../mip/defaults.h ../mip/defs.h ../mip/mcdep.h
coffobj.o:	../mip/cgdefs.h ../mip/jopcode.h ../mip/store.h ../mip/codebuf.h
coffobj.o:	../mip/xrefs.h ../mip/xrefs.h
aetree.o:	../mip/aetree.c ../mip/globals.h ../mip/host.h options.h
aetree.o:	../c4p/target.h ../mip/defaults.h ../mip/defs.h ../mip/aetree.h
aetree.o:	../mip/defs.h ../mip/store.h ../mip/aeops.h ../mip/builtin.h
bind.o:	../mip/bind.c ../mip/globals.h ../mip/host.h options.h ../c4p/target.h
bind.o:	../mip/defaults.h ../mip/defs.h ../mip/defs.h ../mip/aetree.h
bind.o:	../mip/util.h ../mip/codebuf.h ../mip/xrefs.h ../mip/cgdefs.h
bind.o:	../mip/bind.h ../mip/builtin.h ../cfe/lex.h ../cfe/sem.h ../mip/store.h
bind.o:	../cfe/vargen.h ../mip/xrefs.h errors.h ../mip/aeops.h
builtin.o:	../mip/builtin.c ../mip/globals.h ../mip/host.h options.h
builtin.o:	../c4p/target.h ../mip/defaults.h ../mip/defs.h ../mip/defs.h
builtin.o:	../mip/builtin.h ../cfe/sem.h ../mip/bind.h ../mip/store.h
builtin.o:	../mip/aeops.h ../mip/aetree.h
cg.o:	../mip/cg.c ../mip/globals.h ../mip/host.h options.h ../c4p/target.h
cg.o:	../mip/defaults.h ../mip/defs.h ../mip/cg.h ../mip/cgdefs.h
cg.o:	../mip/jopcode.h ../mip/store.h ../mip/codebuf.h ../mip/xrefs.h
cg.o:	../mip/aeops.h ../mip/util.h ../mip/xrefs.h ../mip/jopcode.h
cg.o:	../mip/regalloc.h ../mip/regsets.h ../mip/cse.h ../mip/sr.h
cg.o:	../mip/flowgraf.h ../mip/mcdep.h ../mip/aetree.h ../mip/builtin.h
cg.o:	../cfe/sem.h ../cfe/simplify.h ../mip/bind.h errors.h
codebuf.o:	../mip/codebuf.c ../mip/globals.h ../mip/host.h options.h
codebuf.o:	../c4p/target.h ../mip/defaults.h ../mip/defs.h ../mip/codebuf.h
codebuf.o:	../mip/xrefs.h ../mip/cgdefs.h ../mip/store.h ../mip/xrefs.h
codebuf.o:	../mip/bind.h ../mip/builtin.h ../cfe/sem.h ../mip/util.h
codebuf.o:	../mip/mcdep.h ../mip/jopcode.h errors.h ../mip/aeops.h
compiler.o:	../mip/compiler.c ../mip/globals.h ../mip/host.h options.h
compiler.o:	../c4p/target.h ../mip/defaults.h ../mip/defs.h ../mip/compiler.h
compiler.o:	../mip/fname.h ../cfe/pp.h ../cfe/lex.h ../cfe/syn.h ../cfe/sem.h
compiler.o:	../mip/bind.h ../mip/builtin.h ../mip/aetree.h ../mip/cg.h
compiler.o:	../mip/cgdefs.h ../mip/jopcode.h ../mip/mcdep.h ../mip/aeops.h
compiler.o:	../mip/xrefs.h ../mip/store.h ../mip/version.h errors.h
cse.o:	../mip/cse.c ../mip/globals.h ../mip/host.h options.h ../c4p/target.h
cse.o:	../mip/defaults.h ../mip/defs.h ../mip/cse.h ../mip/cgdefs.h
cse.o:	../mip/cseguts.h ../mip/regsets.h ../mip/jopcode.h ../mip/store.h
cse.o:	../mip/regalloc.h ../mip/cg.h ../mip/flowgraf.h ../mip/mcdep.h
cse.o:	../mip/builtin.h errors.h
csescan.o:	../mip/csescan.c ../mip/globals.h ../mip/host.h options.h
csescan.o:	../c4p/target.h ../mip/defaults.h ../mip/defs.h ../mip/cse.h
csescan.o:	../mip/cgdefs.h ../mip/cseguts.h ../mip/regsets.h ../mip/aeops.h
csescan.o:	../mip/jopcode.h ../mip/store.h ../mip/cgdefs.h ../mip/builtin.h
csescan.o:	../mip/mcdep.h ../mip/flowgraf.h ../mip/regalloc.h errors.h
csescan.o:	../mip/cg.h
dbx.o:	../mip/dbx.c ../mip/globals.h ../mip/host.h options.h ../c4p/target.h
dbx.o:	../mip/defaults.h ../mip/defs.h ../mip/mcdep.h ../mip/cgdefs.h
dbx.o:	../mip/jopcode.h ../c4p/mcdpriv.h ../mip/store.h ../mip/codebuf.h
dbx.o:	../mip/xrefs.h ../mip/aeops.h ../mip/xrefs.h ../mip/util.h
dbx.o:	../mip/regalloc.h ../mip/bind.h ../cfe/sem.h errors.h
driver.o:	../mip/driver.c ../mip/globals.h ../mip/host.h options.h
driver.o:	../c4p/target.h ../mip/defaults.h ../mip/defs.h ../mip/compiler.h
driver.o:	../mip/fname.h ../mip/version.h ../mip/drivhelp.h ../mip/mcdep.h
driver.o:	../mip/cgdefs.h ../mip/jopcode.h ../mip/prgname.h
flowgraf.o:	../mip/flowgraf.c ../mip/globals.h ../mip/host.h options.h
flowgraf.o:	../c4p/target.h ../mip/defaults.h ../mip/defs.h ../mip/flowgraf.h
flowgraf.o:	../mip/cgdefs.h ../mip/jopcode.h ../mip/store.h ../mip/cg.h
flowgraf.o:	../mip/codebuf.h ../mip/xrefs.h ../mip/regalloc.h ../mip/regsets.h
flowgraf.o:	../mip/aeops.h ../mip/util.h ../mip/jopcode.h ../mip/mcdep.h
flowgraf.o:	../mip/builtin.h ../cfe/simplify.h ../mip/xrefs.h errors.h
flowgraf.o:	../mip/sr.h
fname.o:	../mip/fname.c ../mip/fname.h
ieeeflt.o:	../mip/ieeeflt.c ../mip/globals.h ../mip/host.h options.h
ieeeflt.o:	../c4p/target.h ../mip/defaults.h ../mip/defs.h errors.h
ieeeflt.o:	../mip/util.h
jopprint.o:	../mip/jopprint.c ../mip/globals.h ../mip/host.h options.h
jopprint.o:	../c4p/target.h ../mip/defaults.h ../mip/defs.h errors.h
jopprint.o:	../mip/cgdefs.h ../mip/jopcode.h ../mip/aeops.h ../mip/aetree.h
jopprint.o:	../mip/flowgraf.h ../mip/regalloc.h
misc.o:	../mip/misc.c ../mip/globals.h ../mip/host.h options.h ../c4p/target.h
misc.o:	../mip/defaults.h ../mip/defs.h ../mip/store.h ../mip/aeops.h
misc.o:	../mip/aetree.h ../cfe/lex.h ../mip/util.h errors.h
regalloc.o:	../mip/regalloc.c ../mip/globals.h ../mip/host.h options.h
regalloc.o:	../c4p/target.h ../mip/defaults.h ../mip/defs.h ../mip/regalloc.h
regalloc.o:	../mip/cgdefs.h errors.h ../cfe/sem.h ../mip/jopcode.h
regalloc.o:	../mip/regsets.h ../mip/store.h ../mip/cg.h ../mip/mcdep.h
regalloc.o:	../mip/flowgraf.h ../mip/builtin.h ../mip/aeops.h ../mip/sr.h
regsets.o:	../mip/regsets.c ../mip/globals.h ../mip/host.h options.h
regsets.o:	../c4p/target.h ../mip/defaults.h ../mip/defs.h ../mip/regsets.h
regsets.o:	../mip/cgdefs.h ../mip/store.h
sr.o:	../mip/sr.c ../mip/globals.h ../mip/host.h options.h ../c4p/target.h
sr.o:	../mip/defaults.h ../mip/defs.h ../mip/aeops.h ../mip/cgdefs.h
sr.o:	../mip/jopcode.h ../mip/regsets.h ../mip/sr.h ../mip/store.h
sr.o:	../mip/flowgraf.h ../mip/builtin.h ../mip/cg.h ../mip/regalloc.h
sr.o:	../mip/bind.h ../mip/mcdep.h errors.h
store.o:	../mip/store.c ../mip/globals.h ../mip/host.h options.h
store.o:	../c4p/target.h ../mip/defaults.h ../mip/defs.h ../mip/store.h
store.o:	../mip/defs.h ../mip/mcdep.h ../mip/cgdefs.h ../mip/jopcode.h errors.h
version.o:	../mip/version.c ../mip/globals.h ../mip/host.h options.h
version.o:	../c4p/target.h ../mip/defaults.h ../mip/defs.h ../mip/version.h
version.o:	../cfe/fevsn.h ../mip/mipvsn.h ../c4p/mcvsn.h
lex.o:	../cfe/lex.c ../mip/globals.h ../mip/host.h options.h ../c4p/target.h
lex.o:	../mip/defaults.h ../mip/defs.h ../cfe/lex.h ../mip/bind.h ../cfe/pp.h
lex.o:	../mip/store.h ../mip/util.h ../mip/aeops.h errors.h
pp.o:	../cfe/pp.c ../mip/globals.h ../mip/host.h options.h ../c4p/target.h
pp.o:	../mip/defaults.h ../mip/defs.h ../cfe/pp.h ../cfe/syn.h ../mip/store.h
pp.o:	errors.h ../mip/mcdep.h ../mip/cgdefs.h ../mip/jopcode.h
sem.o:	../cfe/sem.c ../mip/globals.h ../mip/host.h options.h ../c4p/target.h
sem.o:	../mip/defaults.h ../mip/defs.h ../cfe/sem.h ../mip/bind.h
sem.o:	../mip/aetree.h ../mip/builtin.h ../mip/aeops.h ../mip/store.h errors.h
sem.o:	../mip/util.h
simplify.o:	../cfe/simplify.c ../mip/globals.h ../mip/host.h options.h
simplify.o:	../c4p/target.h ../mip/defaults.h ../mip/defs.h ../cfe/simplify.h
simplify.o:	../cfe/sem.h ../mip/bind.h ../mip/aetree.h ../mip/builtin.h
simplify.o:	../mip/store.h ../mip/aeops.h errors.h
syn.o:	../cfe/syn.c ../mip/globals.h ../mip/host.h options.h ../c4p/target.h
syn.o:	../mip/defaults.h ../mip/defs.h ../cfe/syn.h ../cfe/pp.h ../cfe/lex.h
syn.o:	../cfe/simplify.h ../mip/bind.h ../cfe/sem.h ../mip/aetree.h
syn.o:	../mip/builtin.h ../cfe/vargen.h ../mip/mcdep.h ../mip/cgdefs.h
syn.o:	../mip/jopcode.h ../mip/store.h errors.h ../mip/aeops.h ../mip/codebuf.h
syn.o:	../mip/xrefs.h
vargen.o:	../cfe/vargen.c ../mip/globals.h ../mip/host.h options.h
vargen.o:	../c4p/target.h ../mip/defaults.h ../mip/defs.h ../cfe/vargen.h
vargen.o:	../cfe/lex.h ../cfe/syn.h ../cfe/sem.h ../cfe/simplify.h
vargen.o:	../mip/bind.h ../mip/builtin.h ../mip/aetree.h ../mip/codebuf.h
vargen.o:	../mip/xrefs.h ../mip/regalloc.h ../mip/cgdefs.h ../mip/mcdep.h
vargen.o:	../mip/jopcode.h ../mip/store.h ../mip/aeops.h ../mip/util.h
vargen.o:	../mip/xrefs.h errors.h

# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY
# see make depend above
