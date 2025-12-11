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

CC = gcc
LD = gcc
NEW = ./ncc.i386
LIB=
CFLAGS = -DNON_RELEASE_VSN='"0.00 (experimental)"' \
	 -D__unix -I. -I../i386 -I../mip -I../cfe -g -c
AFLAGS = -DNON_RELEASE_VSN='"0.00 (experimental)"' \
	 -D__unix -I. -I../i386 -I../mip -I../cfe -g -S
LINKFLAGS=
AH=/usr/include/
PH=../mip/
PC=../mip/
UC=../util/
MH=../i386/
MC=../i386/
LH=../cfe/
LPPH=../cfe/
LC=../cfe/
XH=
XC=

ANSIHDRS = \
$(AH)assert.h $(AH)ctype.h  $(AH)errno.h  $(AH)float.h  $(AH)limits.h \
$(AH)locale.h $(AH)math.h   $(AH)setjmp.h $(AH)signal.h $(AH)stdarg.h \
$(AH)stddef.h $(AH)stdio.h  $(AH)stdlib.h $(AH)string.h $(AH)time.h

HDRFILES = \
# HDRFILES = \
assert.h ctype.h  errno.h  float.h limits.h locale.h math.h setjmp.h \
signal.h stdarg.h stddef.h stdio.h stdlib.h string.h time.h

	    #${PC}elfobj.c \
	    
SOURCES =   ${MC}gen.c \
	    ${MC}isched.c \
	    ${MC}mcdep.c \
	    ${MC}asm.c \
	    ${MC}objstub.c \
	    ${PC}nullobj.c \
	    ${PC}aetree.c \
	    ${PC}bind.c \
	    ${PC}builtin.c \
	    ${PC}cg.c \
	    ${PC}codebuf.c \
	    ${PC}compiler.c \
	    ${PC}cse.c \
	    ${PC}csescan.c \
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

#	#asm.o gen.o isched.o mcdep.o \

OBJS = 	gen.o mcdep.o asm.o isched.o objstub.o nullobj.o \
	bind.o builtin.o cg.o codebuf.o cse.o csescan.o driver.o fname.o \
	flowgraf.o ieeeflt.o jopprint.o lex.o \
	misc.o pp.o regalloc.o regsets.o sem.o simplify.o sr.o store.o \
	compiler.o syn.o aetree.o vargen.o version.o

#	#asm.s gen.s isched.s mcdep.s \

ASMS =	gen.s mcdep.s asm.s isched.s objstub.s nullobj.s \
	bind.s builtin.s cg.s codebuf.s cse.s csescan.s driver.s fname.s \
	flowgraf.s ieeeflt.s jopprint.s lex.s \
	misc.s pp.s regalloc.s regsets.s sem.s simplify.s sr.s store.s \
	compiler.s syn.s aetree.s vargen.s version.s #elfobj.s

${NEW}:	${OBJS}
# did include elfobj.o
	${LD} -o ${NEW} ${LINKFLAGS} ${OBJS} ${LIB}
	# was: elfobj.o ${LIB}
#	rm version.o

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
	/users/GNU/emacs-18.57/etc/etags -t $(SOURCES) \
		${LH}fevsn.h ${LH}lex.h ${LH}pp.h ${LH}sem.h \
		${LH}simplify.h ${LH}syn.h ${LH}vargen.h $(MH)mcdpriv.h \
		$(MH)mcvsn.h $(MH)ops.h $(MH)target.h $(PH)aeops.h \
		$(PH)aetree.h $(PH)aout.h $(PH)bind.h $(PH)builtin.h \
		$(PH)cg.h $(PH)cgdefs.h $(PH)codebuf.h $(PH)compiler.h \
		$(PH)cse.h $(PH)cseguts.h $(PH)defaults.h $(PH)defs.h \
		$(PH)drivhelp.h $(PH)flowgraf.h $(PH)fname.h $(PH)globals.h \
		$(PH)host.h $(PH)jopcode.h $(PH)mcdep.h $(PH)mipvsn.h \
		$(PH)regalloc.h $(PH)regsets.h $(PH}sr.h $(PH)store.h \
		$(PH)util.h $(PH)version.h $(PH)xrefs.h errors.h options.h

#
# Machine-specific things...
#

aoutobj.o:	
		${CC} ${CFLAGS} ${PC}aoutobj.c

elfobj.o:	
		${CC} ${CFLAGS} ${PC}elfobj.c

nullobj.o:	
		${CC} ${CFLAGS} ${PC}nullobj.c

asm.o:		
		${CC} ${CFLAGS} ${MC}asm.c

gen.o:		
		${CC} ${CFLAGS} ${MC}gen.c

objstub.o:		
		${CC} ${CFLAGS} ${MC}objstub.c

isched.o:		
		${CC} ${CFLAGS} ${MC}isched.c

mcdep.o:	
		${CC} ${CFLAGS} ${MC}mcdep.c


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
#
# Machine-specific things...
#

aoutobj.s:	${NEW} ${PC}aoutobj.c
		-mv aoutobj.s aoutobj.s.old
		${NEW} ${AFLAGS} ${PC}aoutobj.c

elfobj.s:	${NEW} ${PC}elfobj.c
		-mv elfobj.s elfobj.s.old
		${NEW} ${AFLAGS} ${PC}elfobj.c

asm.s:		${NEW} ${MC}asm.c
		-mv asm.s asm.s.old
		${NEW} ${AFLAGS} ${MC}asm.c

gen.s:		${NEW} ${MC}gen.c
		-mv gen.s gen.s.old
		${NEW} ${AFLAGS} ${MC}gen.c

isched.s:	${NEW} ${MC}asm.c
		-mv isched.s isched.s.old
		${NEW} ${AFLAGS} ${MC}isched.c

mcdep.s:	${NEW} ${MC}mcdep.c
		-mv mcdep.s mcdep.s.old
		${NEW} ${AFLAGS} ${MC}mcdep.c

#
# Machine- and language- independent things
#

aetree.s:	${NEW} ${PC}aetree.c
		-mv aetree.s aetree.s.old
		${NEW} ${AFLAGS} ${PC}aetree.c

bind.s:		${NEW} ${PC}bind.c
		-mv bind.s bind.s.old
		${NEW} ${AFLAGS} ${PC}bind.c

builtin.s:	${NEW} ${PC}builtin.c
		-mv builtin.s builtin.s.old
		${NEW} ${AFLAGS} ${PC}builtin.c

cg.s:		${NEW} ${PC}cg.c
		-mv cg.s cg.s.old
		${NEW} ${AFLAGS} ${PC}cg.c

codebuf.s:	${NEW} ${PC}codebuf.c
		-mv codebuf.s codebuf.s.old
		${NEW} ${AFLAGS} ${PC}codebuf.c

cse.s:		${NEW} ${PC}cse.c
		-mv cse.s cse.s.old
		${NEW} ${AFLAGS} ${PC}cse.c

csescan.s:	${NEW} ${PC}csescan.c
		-mv csescan.s csescan.s.old
		${NEW} ${AFLAGS} ${PC}csescan.c

compiler.s:	${NEW} ${PC}compiler.c
		-mv compiler.s compiler.s.old
		${NEW} ${AFLAGS} ${PC}compiler.c

driver.s:	${NEW} ${PC}driver.c
		-mv driver.s driver.s.old
		${NEW} ${AFLAGS} ${PC}driver.c

fname.s:	${NEW} ${PC}fname.c
		-mv fname.s fname.s.old
		${NEW} ${AFLAGS} ${PC}fname.c

flowgraf.s:	${NEW} ${PC}flowgraf.c
		-mv flowgraf.s flowgraf.s.old
		${NEW} ${AFLAGS} ${PC}flowgraf.c

ieeeflt.s:	${NEW} ${PC}ieeeflt.c
		-mv ieeeflt.s ieeeflt.s.old
		${NEW} ${AFLAGS} ${PC}ieeeflt.c

jopprint.s:	${NEW} ${PC}jopprint.c
		-mv jopprint.s jopprint.s.old
		${NEW} ${AFLAGS} ${PC}jopprint.c

misc.s:		${NEW} ${PC}misc.c
		-mv misc.s misc.s.old
		${NEW} ${AFLAGS} ${PC}misc.c

regalloc.s:	${NEW} ${PC}regalloc.c
		-mv regalloc.s regalloc.s.old
		${NEW} ${AFLAGS} ${PC}regalloc.c

regsets.s:	${NEW} ${PC}regsets.c
		-mv regsets.s regsets.s.old
		${NEW} ${AFLAGS} ${PC}regsets.c

sr.s:		${NEW} ${PC}regsets.c
		-mv sr.s sr.s.old
		${NEW} ${AFLAGS} ${PC}sr.c

store.s:	${NEW} ${PC}store.c
		-mv store.s store.s.old
		${NEW} ${AFLAGS} ${PC}store.c

version.s:	${NEW} ${PC}version.c
		-mv version.s version.s.old
		${NEW} ${AFLAGS} ${PC}version.c

#
# Language-dependent things
#

lex.s:		${NEW} ${LC}lex.c
		-mv lex.s lex.s.old
		${NEW} ${AFLAGS} ${LC}lex.c

pp.s:		${NEW} ${LC}pp.c
		-mv pp.s pp.s.old
		${NEW} ${AFLAGS} ${LC}pp.c

sem.s:		${NEW} ${LC}sem.c
		-mv sem.s sem.s.old
		${NEW} ${AFLAGS} ${LC}sem.c

simplify.s:	${NEW} ${LC}simplify.c
		-mv simplify.s simplify.s.old
		${NEW} ${AFLAGS} ${LC}simplify.c

syn.s:		${NEW} ${LC}syn.c
		-mv syn.s syn.s.old
		${NEW} ${AFLAGS} ${LC}syn.c

vargen.s:	${NEW} ${LC}vargen.c
		-mv vargen.s vargen.s.old
		${NEW} ${AFLAGS} ${LC}vargen.c

assem:	${ASMS}
	echo done

haldane: assem
	rcp ${ASMS} haldane:SUN/Norcroft

haldanec: ${SOURCES} ${PH}*.h ${MH}*.h ${LH}*.h ${XH}*.h
	rcp ${SOURCES} ${PH}*.h ${MH}*.h ${LH}*.h ${XH}*.h haldane:SUN/Norcroft

#------------------------------------------------------------------------

headers.c:	$(ANSIHDRS) genhdrs
		./genhdrs -oheaders.c -I$(AH) $(HDRFILES)

errors.h:	genhdrs ${PH}miperrs.h ${LPPH}feerrs.h ${MH}mcerrs.h
		./genhdrs -eerrors.h -s \
			-q${PH}miperrs.h -q${LPPH}feerrs.h -q${MH}mcerrs.h

genhdrs:	${UC}genhdrs.c
		${CC} -o genhdrs ${UC}genhdrs.c

#          /***************************************************/
#          /*                                                 */
#          /*    Define UN*Xmake dependencies                 */
#          /*                                                 */
#          /***************************************************/

depend:
#	  for j in ${ARMSRC}/*.[ch]; do \
#	      echo $$j :  ${REMOTE}/arm/`basename $$j` >> makedep ;\
#	      echo -n '	cp -i ${REMOTE}/arm/' >> makedep ;\
#	      echo `basename $$j` $$j  >> makedep; \
#          done
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
	      ${CC} -MM ${CFLAGS} $$i | \
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
