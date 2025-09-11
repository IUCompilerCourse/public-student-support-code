CC     = gcc
RACKET = racket
RM     = rm

runtime.o: runtime.c runtime.h
	$(CC) -c -g -std=c99 runtime.c

test: runtime.o
	$(RACKET) run-tests.rkt

clean: test-clean
	$(RM) -f *.o *.out *.exe *.s *~

test-clean:
	$(RM) -f tests/*.o tests/*.out tests/*.exe tests/*.s tests/*~
