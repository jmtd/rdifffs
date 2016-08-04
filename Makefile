all: rdifffs Test

rdifffs: rdifffs.hs Rdiff.o
	ghc -threaded --make rdifffs.hs

Rdiff.o: Rdiff.hs
	ghc -c Rdiff.hs

util/grepCopy: util/grepCopy.hs Rdiff.o
	ghc -threaded --make util/grepCopy.hs

Test: Rdiff.o Test.hs
	ghc --make Test.hs

check: Test
	./Test

clean:
	rm -f rdifffs *.hi *.o

.PHONY: clean check
