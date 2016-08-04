all: rdifffs

rdifffs: rdifffs.hs Rdiff.o
	ghc -threaded --make rdifffs.hs

Rdiff.o: Rdiff.hs
	ghc -c Rdiff.hs

util/grepCopy: util/grepCopy.hs Rdiff.o
	ghc -threaded --make util/grepCopy.hs

Test: Rdiff.o
	ghc --make Test.hs

clean:
	rm -f rdifffs *.hi *.o

.PHONY: clean
