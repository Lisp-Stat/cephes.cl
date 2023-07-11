# Special functions from the Scipy version of the Cephes library

SRC = $(wildcard *.c)
HEADERS = $(wildcard *.h)
OBJS = $(patsubst %.c, %.o, $(SRC))

CC = cc
CFLAGS = -g -O2 -Wall -Wno-unused-function -fno-builtin -fPIC -include cephes.h -include math.h
AR = ar
RANLIB = ranlib
INCS = mconf.h
AS = as

ifeq ($(OS),Windows_NT)
	OUTPUT = libmd.dll
else # UNIX, use uname to determine which one
	UNAME_S := $(shell uname -s)
	UNAME_M := $(shell uname -m)
	ifeq ($(UNAME_S), Linux)
		LDFLAGS = -lm
		OUTPUT = libmd.so
	endif
	ifeq ($(UNAME_S), Darwin)
		LDFLAGS = -lm
		ifeq ($(UNAME_M), arm64)
			OUTPUT = libmd-arm64.dylib
		else
			OUTPUT = libmd-x86-64.dylib
		endif
	endif
endif

all: shared

stamp-timing: libmd.a mtst time-it
	time-it "mtst > /dev/null"
	touch stamp-timing

time-it: time-it.o
	$(CC) -o time-it time-it.o

time-it.o: time-it.c
	$(CC) -O2 -c time-it.c

dcalc: dcalc.o libmd.a
	$(CC) -o dcalc dcalc.o libmd.a
#	aout2exe mtst

mtst: mtst.o libmd.a
	$(CC) -v -o mtst mtst.o libmd.a
#	gcc -Wl,-verbose -b i486-linuxaout -v -o mtst mtst.o libmd.a
#	coff2exe mtst

mtst.o: mtst.c
	$(CC) -O2 -Wall -c mtst.c

dtestvec: dtestvec.o libmd.a
	$(CC) -o dtestvec dtestvec.o libmd.a

dtestvec.o: dtestvec.c
	$(CC) -g -c dtestvec.c

paranoia: paranoia.o setprec.o libmd.a
	$(CC) -o paranoia paranoia.o setprec.o libmd.a

paranoia.o: paranoia.c
	$(CC) $(CFLAGS) -c paranoia.c

shared: $(OBJS) $(INCS)
	   $(CC) -shared -o $(OUTPUT) $(LDFLAGS) $(OBJS)

libmd.a: $(OBJS) $(INCS)

# for real Unix:
	$(AR) rv libmd.a $(OBJS)
# for djgcc MSDOS:
#	>libmd.rf -rv libmd.a $(OBJS)
#	$(AR) @libmd.rf
	$(RANLIB) libmd.a

# If the following are all commented out, the C versions
# will be used by default.

# IBM PC:
#sqrt.o: sqrt.387
#	$(AS) -o sqrt.o sqrt.387
#
#floor.o: floor.387
#	$(AS) -o floor.o floor.387
#
#setprec.o: setprec.387
#	$(AS) -o setprec.o setprec.387

# ELF versions for linux (no underscores)
#sqrt.o: sqrtelf.387
#	$(AS) -o sqrt.o sqrtelf.387

#floor.o: floorelf.387
#	$(AS) -o floor.o floorelf.387

#setprec.o: setprelf.387
#	$(AS) -o setprec.o setprelf.387

# Motorola 68881. Caution, subroutine return conventions vary.
#sqrt.o: sqrt.688
#	$(AS) -o sqrt.o sqrt.688
#
#setprec.o: setprec.688
#	$(AS) -o setprec.o setprec.688

# SPARC:
#sqrt.o: sqrt.spa
#	$(AS) -o sqrt.o sqrt.spa

clean:
	rm -f *.o
	rm -f mtst
	rm -f paranoia
	rm -f dcalc
	rm -f libmd.a
	rm -f libmd.dll
	rm -f libmd.so
	rm -f libmd-*.dylib
	rm -f time-it
	rm -f dtestvec

