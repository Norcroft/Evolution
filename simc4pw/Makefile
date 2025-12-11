.SUFFIXES:
.SUFFIXES: .o .c

CC = gcc
LD = gcc
NCC = ../ccc4p/c4pncc -fah
NCCold = ../ccc4p/c4pncc.v105 -fah
#VSN= -DNON_RELEASE_VSN='"0.02 (Codemist Ltd)"' 
VSN=
CFLAGS = $(VSN) -D__unix -J../../c4phdr -I. -I../c4p -I../mip -I../cfe
PH=../mip/
LPPH=../cfe/
MH=../c4p/
UC=../util/

catncc.o: errors.h
	${NCC} ${CFLAGS} -c catncc.c
catncc.s: errors.h
	${NCC} ${CFLAGS} -S catncc.c
catncc.ss: errors.h
	${NCC} ${CFLAGS} -S catncc.c -fs -o catncc.ss
catncc.d: catncc.o
	objdump -fhrst catncc.o > catncc.d
ocatncc.s: errors.h
	${NCCold} ${CFLAGS} -S catncc.c -o catncc.s.105
ocatncc.o: errors.h
	${NCCold} ${CFLAGS} -c catncc.c -o catncc.o.105
ocatncc.d: ocatncc.o
	objdump -fhrst catncc.o.105 > catncc.d.105
tar:
	tar cf /export/home/am/cat`date '+%y%m%d'`.tar \
           Makefile catncc.c options.h
	gzip /export/home/am/cat`date '+%y%m%d'`.tar

errors.h:	genhdrs ${PH}miperrs.h ${LPPH}feerrs.h ${MH}mcerrs.h
		genhdrs -eerrors.h -s \
			-q${PH}miperrs.h -q${LPPH}feerrs.h -q${MH}mcerrs.h

genhdrs:	${UC}genhdrs.c
		${CC} -c -o genhdrs.o ${UC}genhdrs.c
		${LD} -o genhdrs genhdrs.o
