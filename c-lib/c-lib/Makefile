# Build Codemist C library in testing mode

CC = gcc
AR = ar

# The next big requires that I use GNU make.
ifdef CYGWIN
extra_cflags = -mno-cygwin
else
extra_cflags =
endif
CFLAGS = ${extra_cflags} -m32 -D__unix_lib -nostdinc -I../c-libhdr

SOURCES = simsyint.h hostsys.h simsys.c stdio.c \
	alloc.c ctype.c error.c fpprintf.c getenv.c \
	locale.c math.c printf.c raise.c remove.c \
	sbrk.c scanf.c signal.c sort.c stdlib.c \
	string.c system.c time.c \
	softfp.c __mul.c __div.c __rem.c __udiv.c __urem.c __remain.c \
	__shift.o __fixup.o

SOBJ =  lib-base.o \
	simsys.o stdio.o \
	alloc.o ctype.o error.o fpprintf.o getenv.o \
	locale.o math.o printf.o raise.o remove.o \
	sbrk.o scanf.o signal.o sort.o stdlib.o \
	string.o system.o time.o \
	softfp.o __mul.o __div.o __rem.o __udiv.o __urem.o __remain.o \
	__shift.o __fixup.o

liball.a: $(SOBJ) simsyint.h hostsys.h
	${AR} crv liball.a $(SOBJ)

liball.o: liball.c ${SOURCES}
	${CC} ${CFLAGS} -c liball.c

liball.s: liball.c ${SOURCES}
	${CC} ${CFLAGS} -S liball.c
# I will try to identify name-space pollution prospects here.
	grep : liball.s | grep -v \^L | grep -v \^__ | grep \^_ > names

lib-base.o:	lib-base.c
	$(CC) ${extra_cflags} -m32 -c lib-base.c

.c.o:	hostsys.h
	${CC} ${CFLAGS} -c $*.c

clean:
	-rm *.o *.a *.bak *~ *.s

# end of Makefile
