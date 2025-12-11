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

CC = ncc -fah
LD = ncc -verify
NEW = ./ncc.sparc
LIB=
CFLAGS = -DNON_RELEASE_VSN='"5.00 (Codemist Ltd)"' \
	 -D__unix -I. -I../sparc -I../mip -I../cfe -c
AFLAGS = -DNON_RELEASE_VSN='"5.00 (Codemist Ltd)"' \
	 -D__unix -I. -I../sparc -I../mip -I../cfe -S
	 
LINKFLAGS=
AH=/usr/local/ncc.include/
PH=../mip/
PC=../mip/
UC=../util/
MH=../sparc/
MC=../sparc/
LH=../cfe/
LPPH=../cfe/
LC=../cfe/
XH=
XC=

ANSIHDRS = \
$(AH)assert.h $(AH)ctype.h  $(AH)errno.h  $(AH)float.h  $(AH)limits.h \
$(AH)locale.h $(AH)math.h   $(AH)setjmp.h $(AH)signal.h $(AH)stdarg.h \
$(AH)stddef.h $(AH)stdio.h  $(AH)stdlib.h $(AH)string.h $(AH)time.h

HDRFILES =
# HDRFILES = \
# assert.h ctype.h  errno.h  float.h limits.h locale.h math.h setjmp.h \
# signal.h stdarg.h stddef.h stdio.h stdlib.h string.h time.h

# Note the following has aout+coff+elf for 'make depend'.
SOURCES =   ${MC}gen.c \
	    ${MC}mcdep.c \
	    ${MC}asm.c \
	    ${PC}aoutobj.c \
	    ${PC}coffobj.c \
	    ${PC}elfobj.c \
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

ASMS = asm.s gen.s mcdep.s dbx.s \
	bind.s builtin.s cg.s codebuf.s cse.s csescan.s driver.s fname.s \
	flowgraf.s ieeeflt.s jopprint.s lex.s \
	misc.s pp.s regalloc.s regsets.s sem.s simplify.s sr.s store.s \
	compiler.s syn.s aetree.s vargen.s version.s aoutobj.s

${NEW}:	${OBJS} aoutobj.o
	${LD} -o ${NEW} ${LINKFLAGS} ${OBJS} aoutobj.o ${LIB}
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

coffobj.o:	
		${CC} ${CFLAGS} ${PC}coffobj.c

elfobj.o:	
		${CC} ${CFLAGS} ${PC}elfobj.c

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
#
# Machine-specific things...
#

aoutobj.s:	${NEW} ${PC}aoutobj.c
		-mv aoutobj.s aoutobj.s.old
		${NEW} ${AFLAGS} ${PC}aoutobj.c

coffobj.s:	${NEW} ${PC}coffobj.c
		-mv coffobj.s coffobj.s.old
		${NEW} ${AFLAGS} ${PC}coffobj.c

elfobj.s:	${NEW} ${PC}elfobj.c
		-mv elfobj.s elfobj.s.old
		${NEW} ${AFLAGS} ${PC}elfobj.c

asm.s:		${NEW} ${MC}asm.c
		-mv asm.s asm.s.old
		${NEW} ${AFLAGS} ${MC}asm.c

gen.s:		${NEW} ${MC}gen.c
		-mv gen.s gen.s.old
		${NEW} ${AFLAGS} ${MC}gen.c

mcdep.s:	${NEW} ${MC}mcdep.c
		-mv mcdep.s mcdep.s.old
		${NEW} ${AFLAGS} ${MC}mcdep.c

dbx.s:		${NEW} ${PC}dbx.c
		-mv dbx.s dbx.s.old
		${NEW} ${AFLAGS} ${PC}dbx.c


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
		genhdrs -oheaders.c -I$(AH) $(HDRFILES)

errors.h:	genhdrs ${PH}miperrs.h ${LPPH}feerrs.h ${MH}mcerrs.h
		genhdrs -eerrors.h -s \
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
# DO NOT DELETE THIS LINE
 
gen.o:	../sparc/gen.c ../mip/globals.h ../mip/host.h options.h
gen.o:	../sparc/target.h ../mip/defaults.h ../mip/defs.h ../mip/builtin.h
gen.o:	../mip/mcdep.h ../mip/cgdefs.h ../mip/jopcode.h ../sparc/mcdpriv.h
gen.o:	../mip/xrefs.h ../sparc/ops.h ../mip/jopcode.h ../mip/store.h
gen.o:	../mip/codebuf.h ../mip/xrefs.h ../mip/regalloc.h ../mip/cg.h
gen.o:	../mip/util.h errors.h
mcdep.o:	../sparc/mcdep.c ../mip/globals.h ../mip/host.h options.h
mcdep.o:	../sparc/target.h ../mip/defaults.h ../mip/defs.h ../mip/mcdep.h
mcdep.o:	../mip/cgdefs.h ../mip/jopcode.h ../sparc/mcdpriv.h
asm.o:	../sparc/asm.c ../mip/globals.h ../mip/host.h options.h
asm.o:	../sparc/target.h ../mip/defaults.h ../mip/defs.h ../mip/mcdep.h
asm.o:	../mip/cgdefs.h ../mip/jopcode.h ../mip/xrefs.h ../mip/store.h
asm.o:	../mip/codebuf.h ../mip/xrefs.h ../sparc/ops.h ../sparc/mcdpriv.h
asm.o:	../mip/builtin.h ../mip/version.h errors.h
aoutobj.o:	../mip/aoutobj.c ../mip/globals.h ../mip/host.h options.h
aoutobj.o:	../sparc/target.h ../mip/defaults.h ../mip/defs.h ../mip/aout.h
aoutobj.o:	../mip/mcdep.h ../mip/cgdefs.h ../mip/jopcode.h ../mip/store.h
aoutobj.o:	../mip/codebuf.h ../mip/xrefs.h ../mip/xrefs.h errors.h
elfobj.o:	../mip/elfobj.c ../mip/globals.h ../mip/host.h options.h
elfobj.o:	../sparc/target.h ../mip/defaults.h ../mip/defs.h ../mip/mcdep.h
elfobj.o:	../mip/cgdefs.h ../mip/jopcode.h ../mip/store.h ../mip/codebuf.h
elfobj.o:	../mip/xrefs.h ../mip/builtin.h ../mip/xrefs.h errors.h ../cfe/sem.h
elfobj.o:	../mip/version.h ../mip/elf.h
aetree.o:	../mip/aetree.c ../mip/globals.h ../mip/host.h options.h
aetree.o:	../sparc/target.h ../mip/defaults.h ../mip/defs.h ../mip/aetree.h
aetree.o:	../mip/defs.h ../mip/store.h ../mip/aeops.h ../mip/builtin.h
bind.o:	../mip/bind.c ../mip/globals.h ../mip/host.h options.h
bind.o:	../sparc/target.h ../mip/defaults.h ../mip/defs.h ../mip/defs.h
bind.o:	../mip/aetree.h ../mip/util.h ../mip/codebuf.h ../mip/xrefs.h
bind.o:	../mip/cgdefs.h ../mip/bind.h ../mip/builtin.h ../cfe/lex.h
bind.o:	../cfe/sem.h ../mip/store.h ../cfe/vargen.h ../mip/xrefs.h errors.h
bind.o:	../mip/aeops.h
builtin.o:	../mip/builtin.c ../mip/globals.h ../mip/host.h options.h
builtin.o:	../sparc/target.h ../mip/defaults.h ../mip/defs.h ../mip/defs.h
builtin.o:	../mip/builtin.h ../cfe/sem.h ../mip/bind.h ../mip/store.h
builtin.o:	../mip/aeops.h ../mip/aetree.h
cg.o:	../mip/cg.c ../mip/globals.h ../mip/host.h options.h ../sparc/target.h
cg.o:	../mip/defaults.h ../mip/defs.h ../mip/cg.h ../mip/cgdefs.h
cg.o:	../mip/jopcode.h ../mip/store.h ../mip/codebuf.h ../mip/xrefs.h
cg.o:	../mip/aeops.h ../mip/util.h ../mip/xrefs.h ../mip/jopcode.h
cg.o:	../mip/regalloc.h ../mip/regsets.h ../mip/cse.h ../mip/sr.h
cg.o:	../mip/flowgraf.h ../mip/mcdep.h ../mip/aetree.h ../mip/builtin.h
cg.o:	../cfe/sem.h ../cfe/simplify.h ../mip/bind.h errors.h
codebuf.o:	../mip/codebuf.c ../mip/globals.h ../mip/host.h options.h
codebuf.o:	../sparc/target.h ../mip/defaults.h ../mip/defs.h ../mip/codebuf.h
codebuf.o:	../mip/xrefs.h ../mip/cgdefs.h ../mip/store.h ../mip/xrefs.h
codebuf.o:	../mip/bind.h ../mip/builtin.h ../mip/util.h ../mip/mcdep.h
codebuf.o:	../mip/jopcode.h errors.h ../mip/aeops.h
compiler.o:	../mip/compiler.c ../mip/globals.h ../mip/host.h options.h
compiler.o:	../sparc/target.h ../mip/defaults.h ../mip/defs.h ../mip/compiler.h
compiler.o:	../mip/fname.h ../cfe/pp.h ../cfe/lex.h ../cfe/syn.h ../cfe/sem.h
compiler.o:	../mip/bind.h ../mip/builtin.h ../cfe/vargen.h ../mip/aetree.h
compiler.o:	../mip/cg.h ../mip/cgdefs.h ../mip/jopcode.h ../mip/mcdep.h
compiler.o:	../mip/aeops.h ../mip/xrefs.h ../mip/store.h ../mip/version.h
compiler.o:	errors.h
cse.o:	../mip/cse.c ../mip/globals.h ../mip/host.h options.h ../sparc/target.h
cse.o:	../mip/defaults.h ../mip/defs.h ../mip/cse.h ../mip/cgdefs.h
cse.o:	../mip/cseguts.h ../mip/regsets.h ../mip/jopcode.h ../mip/store.h
cse.o:	../mip/regalloc.h ../mip/cg.h ../mip/flowgraf.h ../mip/mcdep.h
cse.o:	../mip/builtin.h errors.h
csescan.o:	../mip/csescan.c ../mip/globals.h ../mip/host.h options.h
csescan.o:	../sparc/target.h ../mip/defaults.h ../mip/defs.h ../mip/cse.h
csescan.o:	../mip/cgdefs.h ../mip/cseguts.h ../mip/regsets.h ../mip/aeops.h
csescan.o:	../mip/jopcode.h ../mip/store.h ../mip/cgdefs.h ../mip/builtin.h
csescan.o:	../mip/mcdep.h ../mip/flowgraf.h ../mip/regalloc.h errors.h
csescan.o:	../mip/cg.h
dbx.o:	../mip/dbx.c ../mip/aout.h ../mip/globals.h ../mip/host.h options.h
dbx.o:	../sparc/target.h ../mip/defaults.h ../mip/defs.h ../mip/mcdep.h
dbx.o:	../mip/cgdefs.h ../mip/jopcode.h ../sparc/mcdpriv.h ../mip/store.h
dbx.o:	../mip/codebuf.h ../mip/xrefs.h ../mip/aeops.h ../mip/xrefs.h
dbx.o:	../mip/util.h ../mip/regalloc.h ../mip/bind.h ../cfe/sem.h errors.h
driver.o:	../mip/driver.c ../mip/globals.h ../mip/host.h options.h
driver.o:	../sparc/target.h ../mip/defaults.h ../mip/defs.h ../mip/compiler.h
driver.o:	../mip/fname.h ../mip/version.h ../mip/drivhelp.h ../mip/mcdep.h
driver.o:	../mip/cgdefs.h ../mip/jopcode.h ../mip/prgname.h
flowgraf.o:	../mip/flowgraf.c ../mip/globals.h ../mip/host.h options.h
flowgraf.o:	../sparc/target.h ../mip/defaults.h ../mip/defs.h ../mip/flowgraf.h
flowgraf.o:	../mip/cgdefs.h ../mip/jopcode.h ../mip/store.h ../mip/cg.h
flowgraf.o:	../mip/codebuf.h ../mip/xrefs.h ../mip/regalloc.h ../mip/regsets.h
flowgraf.o:	../mip/aeops.h ../mip/util.h ../mip/jopcode.h ../mip/mcdep.h
flowgraf.o:	../mip/builtin.h ../cfe/simplify.h ../mip/xrefs.h errors.h
flowgraf.o:	../mip/sr.h
fname.o:	../mip/fname.c ../mip/fname.h
ieeeflt.o:	../mip/ieeeflt.c ../mip/globals.h ../mip/host.h options.h
ieeeflt.o:	../sparc/target.h ../mip/defaults.h ../mip/defs.h errors.h
ieeeflt.o:	../mip/util.h
jopprint.o:	../mip/jopprint.c ../mip/globals.h ../mip/host.h options.h
jopprint.o:	../sparc/target.h ../mip/defaults.h ../mip/defs.h ../mip/cgdefs.h
jopprint.o:	../mip/jopcode.h ../mip/aeops.h ../mip/aetree.h ../mip/flowgraf.h
misc.o:	../mip/misc.c ../mip/globals.h ../mip/host.h options.h
misc.o:	../sparc/target.h ../mip/defaults.h ../mip/defs.h ../mip/store.h
misc.o:	../mip/aeops.h ../mip/aetree.h ../cfe/lex.h ../mip/util.h errors.h
regalloc.o:	../mip/regalloc.c ../mip/globals.h ../mip/host.h options.h
regalloc.o:	../sparc/target.h ../mip/defaults.h ../mip/defs.h ../mip/regalloc.h
regalloc.o:	../mip/cgdefs.h errors.h ../cfe/sem.h ../mip/jopcode.h
regalloc.o:	../mip/regsets.h ../mip/store.h ../mip/cg.h ../mip/mcdep.h
regalloc.o:	../mip/flowgraf.h ../mip/builtin.h ../mip/aeops.h ../mip/sr.h
regsets.o:	../mip/regsets.c ../mip/globals.h ../mip/host.h options.h
regsets.o:	../sparc/target.h ../mip/defaults.h ../mip/defs.h ../mip/regsets.h
regsets.o:	../mip/cgdefs.h ../mip/store.h
sr.o:	../mip/sr.c ../mip/globals.h ../mip/host.h options.h ../sparc/target.h
sr.o:	../mip/defaults.h ../mip/defs.h ../mip/aeops.h ../mip/cgdefs.h
sr.o:	../mip/jopcode.h ../mip/regsets.h ../mip/sr.h ../mip/store.h
sr.o:	../mip/flowgraf.h ../mip/builtin.h ../mip/cg.h ../mip/regalloc.h
sr.o:	../mip/bind.h ../mip/mcdep.h errors.h
store.o:	../mip/store.c ../mip/globals.h ../mip/host.h options.h
store.o:	../sparc/target.h ../mip/defaults.h ../mip/defs.h ../mip/store.h
store.o:	../mip/defs.h ../mip/mcdep.h ../mip/cgdefs.h ../mip/jopcode.h errors.h
version.o:	../mip/version.c ../mip/globals.h ../mip/host.h options.h
version.o:	../sparc/target.h ../mip/defaults.h ../mip/defs.h ../mip/version.h
version.o:	../cfe/fevsn.h ../mip/mipvsn.h ../sparc/mcvsn.h
lex.o:	../cfe/lex.c ../mip/globals.h ../mip/host.h options.h ../sparc/target.h
lex.o:	../mip/defaults.h ../mip/defs.h ../cfe/lex.h ../mip/bind.h ../cfe/pp.h
lex.o:	../mip/store.h ../mip/util.h ../mip/aeops.h errors.h
pp.o:	../cfe/pp.c ../mip/globals.h ../mip/host.h options.h ../sparc/target.h
pp.o:	../mip/defaults.h ../mip/defs.h ../cfe/pp.h ../cfe/syn.h ../mip/store.h
pp.o:	errors.h
sem.o:	../cfe/sem.c ../mip/globals.h ../mip/host.h options.h ../sparc/target.h
sem.o:	../mip/defaults.h ../mip/defs.h ../cfe/sem.h ../mip/bind.h
sem.o:	../mip/aetree.h ../mip/builtin.h ../mip/aeops.h ../mip/store.h errors.h
sem.o:	../mip/util.h
simplify.o:	../cfe/simplify.c ../mip/globals.h ../mip/host.h options.h
simplify.o:	../sparc/target.h ../mip/defaults.h ../mip/defs.h ../cfe/simplify.h
simplify.o:	../cfe/sem.h ../mip/bind.h ../mip/aetree.h ../mip/builtin.h
simplify.o:	../mip/store.h ../mip/aeops.h errors.h
syn.o:	../cfe/syn.c ../mip/globals.h ../mip/host.h options.h ../sparc/target.h
syn.o:	../mip/defaults.h ../mip/defs.h ../cfe/syn.h ../cfe/pp.h ../cfe/lex.h
syn.o:	../cfe/simplify.h ../mip/bind.h ../cfe/sem.h ../mip/aetree.h
syn.o:	../mip/builtin.h ../cfe/vargen.h ../mip/mcdep.h ../mip/cgdefs.h
syn.o:	../mip/jopcode.h ../mip/store.h errors.h ../mip/aeops.h ../mip/codebuf.h
syn.o:	../mip/xrefs.h
vargen.o:	../cfe/vargen.c ../mip/globals.h ../mip/host.h options.h
vargen.o:	../sparc/target.h ../mip/defaults.h ../mip/defs.h ../cfe/vargen.h
vargen.o:	../cfe/lex.h ../cfe/syn.h ../cfe/sem.h ../cfe/simplify.h
vargen.o:	../mip/bind.h ../mip/builtin.h ../mip/aetree.h ../mip/codebuf.h
vargen.o:	../mip/xrefs.h ../mip/regalloc.h ../mip/cgdefs.h ../mip/mcdep.h
vargen.o:	../mip/jopcode.h ../mip/store.h ../mip/aeops.h ../mip/util.h
vargen.o:	../mip/xrefs.h errors.h
# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY
# see make depend above
