CC     = gcc
RACKET = racket
RM     = rm

UNAME_S := $(shell uname -s)
UNAME_P := $(shell uname -p)
ifeq ($(UNAME_S),Darwin)
    ifeq ($(UNAME_P),arm)
        $(info M1 MacOSX detected)
        CC := arch -x86_64 $(CC)
    endif
endif

runtime.o: runtime.c runtime.h
	$(CC) -c -g -std=c99 runtime.c

test: runtime.o
	$(RACKET) run-tests.rkt

clean:
	$(RM) -f *.o *.out *.exe *.s *~
