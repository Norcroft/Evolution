# This file was created by Megamake-2
# Copyright (C) Advanced RISC Machines 1996. All rights reserved

SRCDIR1=.
SRCDIR2=../mips
SRCDIR3=../cfe
SRCDIR5=../mip
SRCDIR6=../util
EXTDIR1=../clx
DERDIR=.
CFLAGS= -c -O2 -ansi -pedantic -Dunix  -I$(DERDIR) $(CFL) -I$(SRCDIR1) -I$(SRCDIR2) -I$(SRCDIR3) -I$(SRCDIR5) -I$(SRCDIR6) -I$(EXTDIR1)
#CFLAGS= -c -g -O2 -ansi -pedantic -Dunix  -I$(DERDIR) $(CFL) -I$(SRCDIR1) -I$(SRCDIR2) -I$(SRCDIR3) -I$(SRCDIR5) -I$(SRCDIR6) -I$(EXTDIR1)
#CFLAGS= -c -g -O2 -ansi -pedantic -Wmissing-prototypes -Wchar-subscripts -Wunused -Wreturn-type -W -Wpointer-arith -Wcast-qual -Wstrict-prototypes -Wcomment -Dunix  -I$(DERDIR) $(CFL) -I$(SRCDIR1) -I$(SRCDIR2) -I$(SRCDIR3) -I$(SRCDIR5) -I$(SRCDIR6) -I$(EXTDIR1)
CFLAGS2= -g -O2 -ansi -pedantic -Wmissing-prototypes -Wchar-subscripts -Wunused -Wreturn-type -W -Wpointer-arith -Wcast-qual -Wstrict-prototypes -Wcomment -Dunix
CLXFLAGS= -c -O2 -ansi -pedantic -Wmissing-prototypes -Wchar-subscripts -Wunused -Wreturn-type -W -Wpointer-arith -Wcast-qual -Wstrict-prototypes -Wcomment -Dunix $(CFL) -I$(EXTDIR1)
CC=gcc
CL=gcc
AR=ar
LINK=gcc
MKTAG=${DERDIR}/mktag
MKMSG=${DERDIR}/mkmsg
DESTNAME=/bb/xdevt/release/gcclinux/mipsncc

OBJALL=asm.o elfobj.o dbx.o gen.o mcdep.o aetree.o \
bind.o builtin.o cg.o codebuf.o compiler.o cse.o csescan.o sr.o \
flowgraf.o inline.o jopprint.o misc.o regalloc.o \
regsets.o store.o lex.o pp.o sem.o simplify.o syn.o vargen.o \
fname.o prgname.o ieeeflt.o \
version.o driver.o 

mipsncc: $(OBJALL)
	- mv mipsncc mipsncc.old
	$(LINK) -o mipsncc $(OBJALL)
	rm version.o
	echo "Made mipsncc"

release:
	- chmod +w $(DESTNAME)
	cp -p mipsncc $(DESTNAME)
	strip $(DESTNAME)
	chmod -w $(DESTNAME)

tidy:
	rm *.o genhdrs errors.h headers.c tags.h *.err

HDRFILES=
HDRPATHS=
# HDRFILES=assert.h ctype.h errno.h float.h limits.h locale.h math.h setjmp.h \
# signal.h stdarg.h stddef.h stdio.h stdlib.h string.h time.h 
# HDRPATHS=../cl/stdh/assert.h ../cl/stdh/ctype.h ../cl/stdh/errno.h \
# ../cl/stdh/float.h ../cl/stdh/limits.h ../cl/stdh/locale.h \
# ../cl/stdh/math.h ../cl/stdh/setjmp.h ../cl/stdh/signal.h \
# ../cl/stdh/stdarg.h ../cl/stdh/stddef.h ../cl/stdh/stdio.h \
# ../cl/stdh/stdlib.h ../cl/stdh/string.h ../cl/stdh/time.h 

$(DERDIR)/errors.h: $(DERDIR)/genhdrs ../mip/miperrs.h ../cfe/feerrs.h ../mips/mcerrs.h
	$(DERDIR)/genhdrs -e$(DERDIR)/errors.h -q../mip/miperrs.h -q../cfe/feerrs.h -q../mips/mcerrs.h -s


$(DERDIR)/tags.h: mktag mkmsg ../mip/miperrs.h ../cfe/feerrs.h ../mips/mcerrs.h
	$(MKTAG) -pz ../mip/miperrs.h ../cfe/feerrs.h ../mips/mcerrs.h $(DERDIR)/tags.h
	$(MKMSG) -pqz ../mip/miperrs.h ../cfe/feerrs.h ../mips/mcerrs.h mipsncc.err

headers.c: $(DERDIR)/genhdrs $(HDRPATHS)
	$(DERDIR)/genhdrs -oheaders.c -I../cl/stdh/ $(HDRFILES)

genhdrs: ../util/genhdrs.c
	$(CL) $(CFLAGS2) -o genhdrs ../util/genhdrs.c

mktag:	../util/mktag.c
	$(CL) $(CFLAGS2) -o mktag ../util/mktag.c

mkmsg:	../util/mkmsg.c
	$(CL) $(CFLAGS2) -o mkmsg ../util/mkmsg.c

# Generated dependencies

fname.o: $(EXTDIR1)/fname.c
fname.o: $(EXTDIR1)/fname.h
	$(CC) $(CLXFLAGS) $(CFLfname) -o fname.o $(EXTDIR1)/fname.c
ieeeflt.o: $(EXTDIR1)/ieeeflt.c
ieeeflt.o: $(EXTDIR1)/ieeeflt.h
ieeeflt.o: $(EXTDIR1)/host.h
	$(CC) $(CLXFLAGS) $(CFLieeeflt) -o ieeeflt.o $(EXTDIR1)/ieeeflt.c
prgname.o: $(EXTDIR1)/prgname.c
prgname.o: $(EXTDIR1)/prgname.h
	$(CC) $(CLXFLAGS) $(CFLprgname) -o prgname.o $(EXTDIR1)/prgname.c

aetree.o: $(SRCDIR5)/aetree.c
aetree.o: $(SRCDIR5)/globals.h
aetree.o: $(EXTDIR1)/host.h
aetree.o: $(SRCDIR1)/options.h
aetree.o: $(SRCDIR2)/target.h
aetree.o: $(SRCDIR5)/defaults.h
aetree.o: $(SRCDIR5)/defs.h
aetree.o: $(EXTDIR1)/ieeeflt.h
aetree.o: $(EXTDIR1)/host.h
aetree.o: $(EXTDIR1)/msg.h
aetree.o: $(SRCDIR5)/aetree.h
aetree.o: $(SRCDIR5)/defs.h
aetree.o: $(SRCDIR5)/store.h
aetree.o: $(SRCDIR5)/aeops.h
aetree.o: $(SRCDIR5)/bind.h
aetree.o: $(SRCDIR5)/builtin.h
aetree.o: $(SRCDIR3)/sem.h
aetree.o: $(DERDIR)/errors.h
aetree.o: $(EXTDIR1)/msg.h
aetree.o: $(DERDIR)/tags.h
	$(CC) $(CFLAGS) $(CFLaetree) -o aetree.o $(SRCDIR5)/aetree.c
asm.o: $(SRCDIR2)/asm.c
asm.o: $(SRCDIR5)/globals.h
asm.o: $(EXTDIR1)/host.h
asm.o: $(SRCDIR1)/options.h
asm.o: $(SRCDIR2)/target.h
asm.o: $(SRCDIR5)/defaults.h
asm.o: $(SRCDIR5)/defs.h
asm.o: $(EXTDIR1)/ieeeflt.h
asm.o: $(EXTDIR1)/host.h
asm.o: $(EXTDIR1)/msg.h
asm.o: $(SRCDIR5)/mcdep.h
asm.o: $(SRCDIR5)/cgdefs.h
asm.o: $(SRCDIR5)/jopcode.h
asm.o: $(SRCDIR5)/xrefs.h
asm.o: $(SRCDIR2)/mcdpriv.h
asm.o: $(SRCDIR5)/codebuf.h
asm.o: $(SRCDIR5)/xrefs.h
asm.o: $(SRCDIR5)/xrefs.h
asm.o: $(SRCDIR5)/store.h
asm.o: $(SRCDIR5)/codebuf.h
asm.o: $(SRCDIR2)/ops.h
asm.o: $(SRCDIR5)/version.h
asm.o: $(SRCDIR5)/builtin.h
asm.o: $(DERDIR)/errors.h
asm.o: $(EXTDIR1)/msg.h
asm.o: $(DERDIR)/tags.h
asm.o: $(EXTDIR1)/host.h
	$(CC) $(CFLAGS) $(CFLasm) -o asm.o $(SRCDIR2)/asm.c
elfobj.o: $(SRCDIR2)/elfobj.c
	$(CC) $(CFLAGS) $(CFLelfobj) -o elfobj.o $(SRCDIR2)/elfobj.c
dbx.o: $(SRCDIR2)/dbx.c
	$(CC) $(CFLAGS) $(CFLdbx) -o dbx.o $(SRCDIR2)/dbx.c
bind.o: $(SRCDIR5)/bind.c
bind.o: $(SRCDIR5)/globals.h
bind.o: $(EXTDIR1)/host.h
bind.o: $(SRCDIR1)/options.h
bind.o: $(SRCDIR2)/target.h
bind.o: $(SRCDIR5)/defaults.h
bind.o: $(SRCDIR5)/defs.h
bind.o: $(EXTDIR1)/ieeeflt.h
bind.o: $(EXTDIR1)/host.h
bind.o: $(EXTDIR1)/msg.h
bind.o: $(SRCDIR5)/defs.h
bind.o: $(SRCDIR5)/aetree.h
bind.o: $(SRCDIR5)/util.h
bind.o: $(SRCDIR5)/codebuf.h
bind.o: $(SRCDIR5)/xrefs.h
bind.o: $(SRCDIR5)/cgdefs.h
bind.o: $(SRCDIR5)/bind.h
bind.o: $(SRCDIR5)/builtin.h
bind.o: $(SRCDIR3)/lex.h
bind.o: $(SRCDIR3)/sem.h
bind.o: $(SRCDIR5)/store.h
bind.o: $(SRCDIR3)/vargen.h
bind.o: $(SRCDIR5)/xrefs.h
bind.o: $(DERDIR)/errors.h
bind.o: $(EXTDIR1)/msg.h
bind.o: $(DERDIR)/tags.h
bind.o: $(SRCDIR5)/aeops.h
	$(CC) $(CFLAGS) $(CFLbind) -o bind.o $(SRCDIR5)/bind.c
builtin.o: $(SRCDIR5)/builtin.c
builtin.o: $(SRCDIR5)/globals.h
builtin.o: $(EXTDIR1)/host.h
builtin.o: $(SRCDIR1)/options.h
builtin.o: $(SRCDIR2)/target.h
builtin.o: $(SRCDIR5)/defaults.h
builtin.o: $(SRCDIR5)/defs.h
builtin.o: $(EXTDIR1)/ieeeflt.h
builtin.o: $(EXTDIR1)/host.h
builtin.o: $(EXTDIR1)/msg.h
builtin.o: $(SRCDIR5)/defs.h
builtin.o: $(SRCDIR5)/builtin.h
builtin.o: $(SRCDIR5)/bind.h
builtin.o: $(SRCDIR5)/store.h
builtin.o: $(SRCDIR5)/aeops.h
builtin.o: $(SRCDIR5)/aetree.h
	$(CC) $(CFLAGS) $(CFLbuiltin) -o builtin.o $(SRCDIR5)/builtin.c
cg.o: $(SRCDIR5)/cg.c
cg.o: $(SRCDIR5)/globals.h
cg.o: $(EXTDIR1)/host.h
cg.o: $(SRCDIR1)/options.h
cg.o: $(SRCDIR2)/target.h
cg.o: $(SRCDIR5)/defaults.h
cg.o: $(SRCDIR5)/defs.h
cg.o: $(EXTDIR1)/ieeeflt.h
cg.o: $(EXTDIR1)/host.h
cg.o: $(EXTDIR1)/msg.h
cg.o: $(SRCDIR5)/cg.h
cg.o: $(SRCDIR5)/cgdefs.h
cg.o: $(SRCDIR5)/jopcode.h
cg.o: $(SRCDIR5)/store.h
cg.o: $(SRCDIR5)/codebuf.h
cg.o: $(SRCDIR5)/xrefs.h
cg.o: $(SRCDIR5)/aeops.h
cg.o: $(SRCDIR5)/util.h
cg.o: $(SRCDIR5)/xrefs.h
cg.o: $(SRCDIR5)/jopcode.h
cg.o: $(SRCDIR5)/regalloc.h
cg.o: $(SRCDIR5)/regsets.h
cg.o: $(SRCDIR5)/cse.h
cg.o: $(SRCDIR5)/sr.h
cg.o: $(SRCDIR5)/flowgraf.h
cg.o: $(SRCDIR5)/mcdep.h
cg.o: $(SRCDIR5)/xrefs.h
cg.o: $(SRCDIR5)/aetree.h
cg.o: $(SRCDIR5)/builtin.h
cg.o: $(SRCDIR3)/sem.h
cg.o: $(SRCDIR3)/simplify.h
cg.o: $(SRCDIR5)/bind.h
cg.o: $(DERDIR)/errors.h
cg.o: $(EXTDIR1)/msg.h
cg.o: $(DERDIR)/tags.h
cg.o: $(SRCDIR5)/inline.h
cg.o: $(SRCDIR5)/cgdefs.h
cg.o: $(SRCDIR5)/jopcode.h
	$(CC) $(CFLAGS) $(CFLcg) -o cg.o $(SRCDIR5)/cg.c
codebuf.o: $(SRCDIR5)/codebuf.c
codebuf.o: $(SRCDIR5)/globals.h
codebuf.o: $(EXTDIR1)/host.h
codebuf.o: $(SRCDIR1)/options.h
codebuf.o: $(SRCDIR2)/target.h
codebuf.o: $(SRCDIR5)/defaults.h
codebuf.o: $(SRCDIR5)/defs.h
codebuf.o: $(EXTDIR1)/ieeeflt.h
codebuf.o: $(EXTDIR1)/host.h
codebuf.o: $(EXTDIR1)/msg.h
codebuf.o: $(SRCDIR5)/codebuf.h
codebuf.o: $(SRCDIR5)/xrefs.h
codebuf.o: $(SRCDIR5)/cgdefs.h
codebuf.o: $(SRCDIR5)/store.h
codebuf.o: $(SRCDIR5)/xrefs.h
codebuf.o: $(SRCDIR5)/bind.h
codebuf.o: $(SRCDIR5)/builtin.h
codebuf.o: $(SRCDIR5)/util.h
codebuf.o: $(SRCDIR5)/mcdep.h
codebuf.o: $(SRCDIR5)/jopcode.h
codebuf.o: $(SRCDIR5)/xrefs.h
codebuf.o: $(DERDIR)/errors.h
codebuf.o: $(EXTDIR1)/msg.h
codebuf.o: $(DERDIR)/tags.h
codebuf.o: $(SRCDIR5)/aeops.h
	$(CC) $(CFLAGS) $(CFLcodebuf) -o codebuf.o $(SRCDIR5)/codebuf.c
compiler.o: $(SRCDIR5)/compiler.c
compiler.o: $(SRCDIR5)/globals.h
compiler.o: $(EXTDIR1)/host.h
compiler.o: $(SRCDIR1)/options.h
compiler.o: $(SRCDIR2)/target.h
compiler.o: $(SRCDIR5)/defaults.h
compiler.o: $(SRCDIR5)/defs.h
compiler.o: $(EXTDIR1)/ieeeflt.h
compiler.o: $(EXTDIR1)/host.h
compiler.o: $(EXTDIR1)/msg.h
compiler.o: $(SRCDIR5)/compiler.h
compiler.o: $(EXTDIR1)/fname.h
compiler.o: $(SRCDIR3)/pp.h
compiler.o: $(SRCDIR3)/lex.h
compiler.o: $(SRCDIR3)/syn.h
compiler.o: $(SRCDIR3)/sem.h
compiler.o: $(SRCDIR5)/bind.h
compiler.o: $(SRCDIR5)/builtin.h
compiler.o: $(SRCDIR3)/vargen.h
compiler.o: $(SRCDIR5)/aetree.h
compiler.o: $(SRCDIR5)/cg.h
compiler.o: $(SRCDIR5)/cgdefs.h
compiler.o: $(SRCDIR5)/jopcode.h
compiler.o: $(SRCDIR5)/mcdep.h
compiler.o: $(SRCDIR5)/xrefs.h
compiler.o: $(SRCDIR5)/aeops.h
compiler.o: $(SRCDIR5)/xrefs.h
compiler.o: $(SRCDIR5)/store.h
compiler.o: $(SRCDIR5)/version.h
compiler.o: $(DERDIR)/errors.h
compiler.o: $(EXTDIR1)/msg.h
compiler.o: $(DERDIR)/tags.h
	$(CC) $(CFLAGS) $(CFLcompiler) -o compiler.o $(SRCDIR5)/compiler.c
cse.o: $(SRCDIR5)/cse.c
cse.o: $(SRCDIR5)/globals.h
cse.o: $(EXTDIR1)/host.h
cse.o: $(SRCDIR1)/options.h
cse.o: $(SRCDIR2)/target.h
cse.o: $(SRCDIR5)/defaults.h
cse.o: $(SRCDIR5)/defs.h
cse.o: $(EXTDIR1)/ieeeflt.h
cse.o: $(EXTDIR1)/host.h
cse.o: $(EXTDIR1)/msg.h
cse.o: $(SRCDIR5)/cse.h
cse.o: $(SRCDIR5)/cgdefs.h
cse.o: $(SRCDIR5)/cseguts.h
cse.o: $(SRCDIR5)/regsets.h
cse.o: $(SRCDIR5)/jopcode.h
cse.o: $(SRCDIR5)/store.h
cse.o: $(SRCDIR5)/regalloc.h
cse.o: $(SRCDIR5)/cg.h
cse.o: $(SRCDIR5)/flowgraf.h
cse.o: $(SRCDIR5)/mcdep.h
cse.o: $(SRCDIR5)/xrefs.h
cse.o: $(SRCDIR5)/builtin.h
cse.o: $(DERDIR)/errors.h
cse.o: $(EXTDIR1)/msg.h
cse.o: $(DERDIR)/tags.h
cse.o: $(SRCDIR5)/aeops.h
	$(CC) $(CFLAGS) $(CFLcse) -o cse.o $(SRCDIR5)/cse.c
csescan.o: $(SRCDIR5)/csescan.c
csescan.o: $(SRCDIR5)/globals.h
csescan.o: $(EXTDIR1)/host.h
csescan.o: $(SRCDIR1)/options.h
csescan.o: $(SRCDIR2)/target.h
csescan.o: $(SRCDIR5)/defaults.h
csescan.o: $(SRCDIR5)/defs.h
csescan.o: $(EXTDIR1)/ieeeflt.h
csescan.o: $(EXTDIR1)/host.h
csescan.o: $(EXTDIR1)/msg.h
csescan.o: $(SRCDIR5)/cse.h
csescan.o: $(SRCDIR5)/cgdefs.h
csescan.o: $(SRCDIR5)/cseguts.h
csescan.o: $(SRCDIR5)/regsets.h
csescan.o: $(SRCDIR5)/aeops.h
csescan.o: $(SRCDIR5)/jopcode.h
csescan.o: $(SRCDIR5)/store.h
csescan.o: $(SRCDIR5)/cgdefs.h
csescan.o: $(SRCDIR5)/builtin.h
csescan.o: $(SRCDIR5)/mcdep.h
csescan.o: $(SRCDIR5)/xrefs.h
csescan.o: $(SRCDIR5)/flowgraf.h
csescan.o: $(SRCDIR5)/regalloc.h
csescan.o: $(DERDIR)/errors.h
csescan.o: $(EXTDIR1)/msg.h
csescan.o: $(DERDIR)/tags.h
csescan.o: $(SRCDIR5)/cg.h
	$(CC) $(CFLAGS) $(CFLcsescan) -o csescan.o $(SRCDIR5)/csescan.c
driver.o: $(SRCDIR5)/driver.c
driver.o: $(SRCDIR5)/globals.h
driver.o: $(EXTDIR1)/host.h
driver.o: $(SRCDIR1)/options.h
driver.o: $(SRCDIR2)/target.h
driver.o: $(SRCDIR5)/defaults.h
driver.o: $(SRCDIR5)/defs.h
driver.o: $(EXTDIR1)/ieeeflt.h
driver.o: $(EXTDIR1)/host.h
driver.o: $(EXTDIR1)/msg.h
driver.o: $(DERDIR)/errors.h
driver.o: $(EXTDIR1)/msg.h
driver.o: $(DERDIR)/tags.h
driver.o: $(SRCDIR5)/compiler.h
driver.o: $(EXTDIR1)/fname.h
driver.o: $(SRCDIR5)/version.h
driver.o: $(SRCDIR5)/drivhelp.h
driver.o: $(SRCDIR5)/mcdep.h
driver.o: $(SRCDIR5)/cgdefs.h
driver.o: $(SRCDIR5)/jopcode.h
driver.o: $(SRCDIR5)/xrefs.h
driver.o: $(EXTDIR1)/prgname.h
	$(CC) $(CFLAGS) $(CFLdriver) -o driver.o $(SRCDIR5)/driver.c
flowgraf.o: $(SRCDIR5)/flowgraf.c
flowgraf.o: $(SRCDIR5)/globals.h
flowgraf.o: $(EXTDIR1)/host.h
flowgraf.o: $(SRCDIR1)/options.h
flowgraf.o: $(SRCDIR2)/target.h
flowgraf.o: $(SRCDIR5)/defaults.h
flowgraf.o: $(SRCDIR5)/defs.h
flowgraf.o: $(EXTDIR1)/ieeeflt.h
flowgraf.o: $(EXTDIR1)/host.h
flowgraf.o: $(EXTDIR1)/msg.h
flowgraf.o: $(SRCDIR5)/flowgraf.h
flowgraf.o: $(SRCDIR5)/cgdefs.h
flowgraf.o: $(SRCDIR5)/jopcode.h
flowgraf.o: $(SRCDIR5)/store.h
flowgraf.o: $(SRCDIR5)/cg.h
flowgraf.o: $(SRCDIR5)/codebuf.h
flowgraf.o: $(SRCDIR5)/xrefs.h
flowgraf.o: $(SRCDIR5)/regalloc.h
flowgraf.o: $(SRCDIR5)/regsets.h
flowgraf.o: $(SRCDIR5)/aeops.h
flowgraf.o: $(SRCDIR5)/util.h
flowgraf.o: $(SRCDIR5)/jopcode.h
flowgraf.o: $(SRCDIR5)/mcdep.h
flowgraf.o: $(SRCDIR5)/xrefs.h
flowgraf.o: $(SRCDIR5)/builtin.h
flowgraf.o: $(SRCDIR3)/simplify.h
flowgraf.o: $(SRCDIR5)/xrefs.h
flowgraf.o: $(DERDIR)/errors.h
flowgraf.o: $(EXTDIR1)/msg.h
flowgraf.o: $(DERDIR)/tags.h
flowgraf.o: $(SRCDIR5)/sr.h
	$(CC) $(CFLAGS) $(CFLflowgraf) -o flowgraf.o $(SRCDIR5)/flowgraf.c
gen.o: $(SRCDIR2)/gen.c
gen.o: $(SRCDIR5)/globals.h
gen.o: $(EXTDIR1)/host.h
gen.o: $(SRCDIR1)/options.h
gen.o: $(SRCDIR2)/target.h
gen.o: $(SRCDIR5)/defaults.h
gen.o: $(SRCDIR5)/defs.h
gen.o: $(EXTDIR1)/ieeeflt.h
gen.o: $(EXTDIR1)/host.h
gen.o: $(EXTDIR1)/msg.h
gen.o: $(SRCDIR5)/mcdep.h
gen.o: $(SRCDIR5)/cgdefs.h
gen.o: $(SRCDIR5)/jopcode.h
gen.o: $(SRCDIR5)/xrefs.h
gen.o: $(SRCDIR2)/mcdpriv.h
gen.o: $(SRCDIR5)/codebuf.h
gen.o: $(SRCDIR5)/xrefs.h
gen.o: $(SRCDIR5)/aeops.h
gen.o: $(SRCDIR5)/xrefs.h
gen.o: $(SRCDIR2)/ops.h
gen.o: $(SRCDIR5)/jopcode.h
gen.o: $(SRCDIR5)/store.h
gen.o: $(SRCDIR5)/codebuf.h
gen.o: $(SRCDIR5)/regalloc.h
gen.o: $(SRCDIR5)/builtin.h
gen.o: $(SRCDIR5)/bind.h
gen.o: $(SRCDIR5)/flowgraf.h
gen.o: $(DERDIR)/errors.h
gen.o: $(EXTDIR1)/msg.h
gen.o: $(DERDIR)/tags.h
	$(CC) $(CFLAGS) $(CFLgen) -o gen.o $(SRCDIR2)/gen.c
genhdrs.o: $(SRCDIR6)/genhdrs.c
	$(CC) $(CFLAGS) $(CFLgenhdrs) -o genhdrs.o $(SRCDIR6)/genhdrs.c
inline.o: $(SRCDIR5)/inline.c
inline.o: $(SRCDIR5)/globals.h
inline.o: $(EXTDIR1)/host.h
inline.o: $(SRCDIR1)/options.h
inline.o: $(SRCDIR2)/target.h
inline.o: $(SRCDIR5)/defaults.h
inline.o: $(SRCDIR5)/defs.h
inline.o: $(EXTDIR1)/ieeeflt.h
inline.o: $(EXTDIR1)/host.h
inline.o: $(EXTDIR1)/msg.h
inline.o: $(SRCDIR5)/store.h
inline.o: $(SRCDIR5)/flowgraf.h
inline.o: $(SRCDIR5)/cgdefs.h
inline.o: $(SRCDIR5)/jopcode.h
inline.o: $(SRCDIR5)/codebuf.h
inline.o: $(SRCDIR5)/xrefs.h
inline.o: $(SRCDIR5)/regalloc.h
inline.o: $(SRCDIR5)/aeops.h
inline.o: $(SRCDIR5)/aetree.h
inline.o: $(SRCDIR5)/bind.h
inline.o: $(SRCDIR5)/jopcode.h
inline.o: $(SRCDIR5)/inline.h
inline.o: $(SRCDIR5)/cgdefs.h
inline.o: $(SRCDIR5)/jopcode.h
inline.o: $(SRCDIR5)/cg.h
inline.o: $(SRCDIR5)/regsets.h
inline.o: $(SRCDIR3)/simplify.h
inline.o: $(DERDIR)/errors.h
inline.o: $(EXTDIR1)/msg.h
inline.o: $(DERDIR)/tags.h
inline.o: $(SRCDIR5)/builtin.h
	$(CC) $(CFLAGS) $(CFLinline) -o inline.o $(SRCDIR5)/inline.c
jopprint.o: $(SRCDIR5)/jopprint.c
jopprint.o: $(SRCDIR5)/globals.h
jopprint.o: $(EXTDIR1)/host.h
jopprint.o: $(SRCDIR1)/options.h
jopprint.o: $(SRCDIR2)/target.h
jopprint.o: $(SRCDIR5)/defaults.h
jopprint.o: $(SRCDIR5)/defs.h
jopprint.o: $(EXTDIR1)/ieeeflt.h
jopprint.o: $(EXTDIR1)/host.h
jopprint.o: $(EXTDIR1)/msg.h
jopprint.o: $(SRCDIR5)/cgdefs.h
jopprint.o: $(SRCDIR5)/store.h
jopprint.o: $(SRCDIR5)/jopcode.h
jopprint.o: $(SRCDIR5)/aeops.h
jopprint.o: $(SRCDIR5)/aetree.h
jopprint.o: $(SRCDIR5)/flowgraf.h
	$(CC) $(CFLAGS) $(CFLjopprint) -o jopprint.o $(SRCDIR5)/jopprint.c
lex.o: $(SRCDIR3)/lex.c
lex.o: $(SRCDIR5)/globals.h
lex.o: $(EXTDIR1)/host.h
lex.o: $(SRCDIR1)/options.h
lex.o: $(SRCDIR2)/target.h
lex.o: $(SRCDIR5)/defaults.h
lex.o: $(SRCDIR5)/defs.h
lex.o: $(EXTDIR1)/ieeeflt.h
lex.o: $(EXTDIR1)/host.h
lex.o: $(EXTDIR1)/msg.h
lex.o: $(SRCDIR3)/lex.h
lex.o: $(SRCDIR5)/bind.h
lex.o: $(SRCDIR3)/pp.h
lex.o: $(SRCDIR5)/store.h
lex.o: $(SRCDIR5)/util.h
lex.o: $(SRCDIR5)/aeops.h
lex.o: $(DERDIR)/errors.h
lex.o: $(EXTDIR1)/msg.h
lex.o: $(DERDIR)/tags.h
	$(CC) $(CFLAGS) $(CFLlex) -o lex.o $(SRCDIR3)/lex.c
mcdep.o: $(SRCDIR2)/mcdep.c
mcdep.o: $(SRCDIR5)/globals.h
mcdep.o: $(EXTDIR1)/host.h
mcdep.o: $(SRCDIR1)/options.h
mcdep.o: $(SRCDIR2)/target.h
mcdep.o: $(SRCDIR5)/defaults.h
mcdep.o: $(SRCDIR5)/defs.h
mcdep.o: $(EXTDIR1)/ieeeflt.h
mcdep.o: $(EXTDIR1)/host.h
mcdep.o: $(EXTDIR1)/msg.h
mcdep.o: $(SRCDIR5)/mcdep.h
mcdep.o: $(SRCDIR5)/cgdefs.h
mcdep.o: $(SRCDIR5)/jopcode.h
mcdep.o: $(SRCDIR5)/xrefs.h
mcdep.o: $(SRCDIR2)/mcdpriv.h
mcdep.o: $(SRCDIR5)/codebuf.h
mcdep.o: $(SRCDIR5)/xrefs.h
mcdep.o: $(DERDIR)/errors.h
mcdep.o: $(EXTDIR1)/msg.h
mcdep.o: $(DERDIR)/tags.h
mcdep.o: $(SRCDIR5)/compiler.h
	$(CC) $(CFLAGS) $(CFLmcdep) -o mcdep.o $(SRCDIR2)/mcdep.c
misc.o: $(SRCDIR5)/misc.c
misc.o: $(SRCDIR5)/globals.h
misc.o: $(EXTDIR1)/host.h
misc.o: $(SRCDIR1)/options.h
misc.o: $(SRCDIR2)/target.h
misc.o: $(SRCDIR5)/defaults.h
misc.o: $(SRCDIR5)/defs.h
misc.o: $(EXTDIR1)/ieeeflt.h
misc.o: $(EXTDIR1)/host.h
misc.o: $(EXTDIR1)/msg.h
misc.o: $(SRCDIR5)/store.h
misc.o: $(SRCDIR5)/aeops.h
misc.o: $(SRCDIR5)/aetree.h
misc.o: $(SRCDIR3)/lex.h
misc.o: $(SRCDIR5)/bind.h
misc.o: $(SRCDIR5)/util.h
misc.o: $(EXTDIR1)/msg.h
misc.o: $(DERDIR)/errors.h
misc.o: $(EXTDIR1)/msg.h
misc.o: $(DERDIR)/tags.h
	$(CC) $(CFLAGS) $(CFLmisc) -o misc.o $(SRCDIR5)/misc.c
pp.o: $(SRCDIR3)/pp.c
pp.o: $(SRCDIR5)/globals.h
pp.o: $(EXTDIR1)/host.h
pp.o: $(SRCDIR1)/options.h
pp.o: $(SRCDIR2)/target.h
pp.o: $(SRCDIR5)/defaults.h
pp.o: $(SRCDIR5)/defs.h
pp.o: $(EXTDIR1)/ieeeflt.h
pp.o: $(EXTDIR1)/host.h
pp.o: $(EXTDIR1)/msg.h
pp.o: $(SRCDIR3)/pp.h
pp.o: $(SRCDIR3)/lex.h
pp.o: $(SRCDIR3)/syn.h
pp.o: $(SRCDIR5)/store.h
pp.o: $(DERDIR)/errors.h
pp.o: $(EXTDIR1)/msg.h
pp.o: $(DERDIR)/tags.h
pp.o: $(SRCDIR5)/mcdep.h
pp.o: $(SRCDIR5)/cgdefs.h
pp.o: $(SRCDIR5)/jopcode.h
pp.o: $(SRCDIR5)/xrefs.h
pp.o: $(DERDIR)/headers.c
	$(CC) $(CFLAGS) $(CFLpp) -o pp.o $(SRCDIR3)/pp.c
regalloc.o: $(SRCDIR5)/regalloc.c
regalloc.o: $(SRCDIR5)/globals.h
regalloc.o: $(EXTDIR1)/host.h
regalloc.o: $(SRCDIR1)/options.h
regalloc.o: $(SRCDIR2)/target.h
regalloc.o: $(SRCDIR5)/defaults.h
regalloc.o: $(SRCDIR5)/defs.h
regalloc.o: $(EXTDIR1)/ieeeflt.h
regalloc.o: $(EXTDIR1)/host.h
regalloc.o: $(EXTDIR1)/msg.h
regalloc.o: $(SRCDIR5)/regalloc.h
regalloc.o: $(SRCDIR5)/cgdefs.h
regalloc.o: $(DERDIR)/errors.h
regalloc.o: $(EXTDIR1)/msg.h
regalloc.o: $(DERDIR)/tags.h
regalloc.o: $(SRCDIR3)/sem.h
regalloc.o: $(SRCDIR5)/jopcode.h
regalloc.o: $(SRCDIR5)/regsets.h
regalloc.o: $(SRCDIR5)/store.h
regalloc.o: $(SRCDIR5)/cg.h
regalloc.o: $(SRCDIR5)/mcdep.h
regalloc.o: $(SRCDIR5)/xrefs.h
regalloc.o: $(SRCDIR5)/flowgraf.h
regalloc.o: $(SRCDIR5)/builtin.h
regalloc.o: $(SRCDIR5)/aeops.h
regalloc.o: $(SRCDIR5)/sr.h
	$(CC) $(CFLAGS) $(CFLregalloc) -o regalloc.o $(SRCDIR5)/regalloc.c
regsets.o: $(SRCDIR5)/regsets.c
regsets.o: $(SRCDIR5)/globals.h
regsets.o: $(EXTDIR1)/host.h
regsets.o: $(SRCDIR1)/options.h
regsets.o: $(SRCDIR2)/target.h
regsets.o: $(SRCDIR5)/defaults.h
regsets.o: $(SRCDIR5)/defs.h
regsets.o: $(EXTDIR1)/ieeeflt.h
regsets.o: $(EXTDIR1)/host.h
regsets.o: $(EXTDIR1)/msg.h
regsets.o: $(SRCDIR5)/regsets.h
regsets.o: $(SRCDIR5)/cgdefs.h
regsets.o: $(SRCDIR5)/store.h
	$(CC) $(CFLAGS) $(CFLregsets) -o regsets.o $(SRCDIR5)/regsets.c
sem.o: $(SRCDIR3)/sem.c
sem.o: $(SRCDIR5)/globals.h
sem.o: $(EXTDIR1)/host.h
sem.o: $(SRCDIR1)/options.h
sem.o: $(SRCDIR2)/target.h
sem.o: $(SRCDIR5)/defaults.h
sem.o: $(SRCDIR5)/defs.h
sem.o: $(EXTDIR1)/ieeeflt.h
sem.o: $(EXTDIR1)/host.h
sem.o: $(EXTDIR1)/msg.h
sem.o: $(SRCDIR3)/sem.h
sem.o: $(SRCDIR5)/bind.h
sem.o: $(SRCDIR5)/aetree.h
sem.o: $(SRCDIR5)/builtin.h
sem.o: $(SRCDIR5)/aeops.h
sem.o: $(SRCDIR5)/store.h
sem.o: $(DERDIR)/errors.h
sem.o: $(EXTDIR1)/msg.h
sem.o: $(DERDIR)/tags.h
sem.o: $(SRCDIR5)/util.h
sem.o: $(SRCDIR3)/simplify.h
	$(CC) $(CFLAGS) $(CFLsem) -o sem.o $(SRCDIR3)/sem.c
simplify.o: $(SRCDIR3)/simplify.c
simplify.o: $(SRCDIR5)/globals.h
simplify.o: $(EXTDIR1)/host.h
simplify.o: $(SRCDIR1)/options.h
simplify.o: $(SRCDIR2)/target.h
simplify.o: $(SRCDIR5)/defaults.h
simplify.o: $(SRCDIR5)/defs.h
simplify.o: $(EXTDIR1)/ieeeflt.h
simplify.o: $(EXTDIR1)/host.h
simplify.o: $(EXTDIR1)/msg.h
simplify.o: $(SRCDIR3)/simplify.h
simplify.o: $(SRCDIR3)/sem.h
simplify.o: $(SRCDIR5)/bind.h
simplify.o: $(SRCDIR5)/aetree.h
simplify.o: $(SRCDIR5)/builtin.h
simplify.o: $(SRCDIR5)/store.h
simplify.o: $(SRCDIR5)/aeops.h
simplify.o: $(DERDIR)/errors.h
simplify.o: $(EXTDIR1)/msg.h
simplify.o: $(DERDIR)/tags.h
	$(CC) $(CFLAGS) $(CFLsimplify) -o simplify.o $(SRCDIR3)/simplify.c
sr.o: $(SRCDIR5)/sr.c
sr.o: $(SRCDIR5)/globals.h
sr.o: $(EXTDIR1)/host.h
sr.o: $(SRCDIR1)/options.h
sr.o: $(SRCDIR2)/target.h
sr.o: $(SRCDIR5)/defaults.h
sr.o: $(SRCDIR5)/defs.h
sr.o: $(EXTDIR1)/ieeeflt.h
sr.o: $(EXTDIR1)/host.h
sr.o: $(EXTDIR1)/msg.h
sr.o: $(SRCDIR5)/aeops.h
sr.o: $(SRCDIR5)/cgdefs.h
sr.o: $(SRCDIR5)/jopcode.h
sr.o: $(SRCDIR5)/regsets.h
sr.o: $(SRCDIR5)/sr.h
sr.o: $(SRCDIR5)/store.h
sr.o: $(SRCDIR5)/flowgraf.h
sr.o: $(SRCDIR5)/builtin.h
sr.o: $(SRCDIR5)/cg.h
sr.o: $(SRCDIR5)/regalloc.h
sr.o: $(SRCDIR5)/bind.h
sr.o: $(SRCDIR5)/mcdep.h
sr.o: $(SRCDIR5)/xrefs.h
sr.o: $(SRCDIR3)/simplify.h
sr.o: $(DERDIR)/errors.h
sr.o: $(EXTDIR1)/msg.h
sr.o: $(DERDIR)/tags.h
	$(CC) $(CFLAGS) $(CFLsr) -o sr.o $(SRCDIR5)/sr.c
store.o: $(SRCDIR5)/store.c
store.o: $(SRCDIR5)/globals.h
store.o: $(EXTDIR1)/host.h
store.o: $(SRCDIR1)/options.h
store.o: $(SRCDIR2)/target.h
store.o: $(SRCDIR5)/defaults.h
store.o: $(SRCDIR5)/defs.h
store.o: $(EXTDIR1)/ieeeflt.h
store.o: $(EXTDIR1)/host.h
store.o: $(EXTDIR1)/msg.h
store.o: $(SRCDIR5)/store.h
store.o: $(SRCDIR5)/defs.h
store.o: $(SRCDIR5)/mcdep.h
store.o: $(SRCDIR5)/cgdefs.h
store.o: $(SRCDIR5)/jopcode.h
store.o: $(SRCDIR5)/xrefs.h
store.o: $(DERDIR)/errors.h
store.o: $(EXTDIR1)/msg.h
store.o: $(DERDIR)/tags.h
	$(CC) $(CFLAGS) $(CFLstore) -o store.o $(SRCDIR5)/store.c
syn.o: $(SRCDIR3)/syn.c
syn.o: $(SRCDIR5)/globals.h
syn.o: $(EXTDIR1)/host.h
syn.o: $(SRCDIR1)/options.h
syn.o: $(SRCDIR2)/target.h
syn.o: $(SRCDIR5)/defaults.h
syn.o: $(SRCDIR5)/defs.h
syn.o: $(EXTDIR1)/ieeeflt.h
syn.o: $(EXTDIR1)/host.h
syn.o: $(EXTDIR1)/msg.h
syn.o: $(SRCDIR3)/syn.h
syn.o: $(SRCDIR3)/pp.h
syn.o: $(SRCDIR3)/lex.h
syn.o: $(SRCDIR3)/simplify.h
syn.o: $(SRCDIR5)/bind.h
syn.o: $(SRCDIR3)/sem.h
syn.o: $(SRCDIR5)/aetree.h
syn.o: $(SRCDIR5)/builtin.h
syn.o: $(SRCDIR3)/vargen.h
syn.o: $(SRCDIR5)/mcdep.h
syn.o: $(SRCDIR5)/cgdefs.h
syn.o: $(SRCDIR5)/jopcode.h
syn.o: $(SRCDIR5)/xrefs.h
syn.o: $(SRCDIR5)/store.h
syn.o: $(DERDIR)/errors.h
syn.o: $(EXTDIR1)/msg.h
syn.o: $(DERDIR)/tags.h
syn.o: $(SRCDIR5)/aeops.h
syn.o: $(SRCDIR5)/codebuf.h
syn.o: $(SRCDIR5)/xrefs.h
	$(CC) $(CFLAGS) $(CFLsyn) -o syn.o $(SRCDIR3)/syn.c
vargen.o: $(SRCDIR3)/vargen.c
vargen.o: $(SRCDIR5)/globals.h
vargen.o: $(EXTDIR1)/host.h
vargen.o: $(SRCDIR1)/options.h
vargen.o: $(SRCDIR2)/target.h
vargen.o: $(SRCDIR5)/defaults.h
vargen.o: $(SRCDIR5)/defs.h
vargen.o: $(EXTDIR1)/ieeeflt.h
vargen.o: $(EXTDIR1)/host.h
vargen.o: $(EXTDIR1)/msg.h
vargen.o: $(SRCDIR3)/vargen.h
vargen.o: $(SRCDIR3)/lex.h
vargen.o: $(SRCDIR3)/syn.h
vargen.o: $(SRCDIR3)/sem.h
vargen.o: $(SRCDIR3)/simplify.h
vargen.o: $(SRCDIR5)/bind.h
vargen.o: $(SRCDIR5)/builtin.h
vargen.o: $(SRCDIR5)/aetree.h
vargen.o: $(SRCDIR5)/codebuf.h
vargen.o: $(SRCDIR5)/xrefs.h
vargen.o: $(SRCDIR5)/regalloc.h
vargen.o: $(SRCDIR5)/cgdefs.h
vargen.o: $(SRCDIR5)/mcdep.h
vargen.o: $(SRCDIR5)/jopcode.h
vargen.o: $(SRCDIR5)/xrefs.h
vargen.o: $(SRCDIR5)/store.h
vargen.o: $(SRCDIR5)/aeops.h
vargen.o: $(SRCDIR5)/util.h
vargen.o: $(SRCDIR5)/xrefs.h
vargen.o: $(DERDIR)/errors.h
vargen.o: $(EXTDIR1)/msg.h
vargen.o: $(DERDIR)/tags.h
	$(CC) $(CFLAGS) $(CFLvargen) -o vargen.o $(SRCDIR3)/vargen.c
version.o: $(SRCDIR5)/version.c
version.o: $(SRCDIR5)/globals.h
version.o: $(EXTDIR1)/host.h
version.o: $(SRCDIR1)/options.h
version.o: $(SRCDIR2)/target.h
version.o: $(SRCDIR5)/defaults.h
version.o: $(SRCDIR5)/defs.h
version.o: $(EXTDIR1)/ieeeflt.h
version.o: $(EXTDIR1)/host.h
version.o: $(EXTDIR1)/msg.h
version.o: $(SRCDIR5)/version.h
version.o: $(SRCDIR3)/fevsn.h
version.o: $(SRCDIR5)/mipvsn.h
version.o: $(SRCDIR2)/mcvsn.h
	$(CC) $(CFLAGS) $(CFLversion) -o version.o $(SRCDIR5)/version.c
