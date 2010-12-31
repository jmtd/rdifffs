all: rdifffs

rdifffs: rdifffs.lhs Rdiff.o
	ghc -threaded --make rdifffs.lhs

Rdiff.o: Rdiff.hs
	ghc -c Rdiff.hs

util/grepCopy: util/grepCopy.hs Rdiff.o
	ghc -threaded --make util/grepCopy.hs

clean:
	rm -f rdifffs *.hi *.o

.PHONY: clean
