all: rdifffs

rdifffs: rdifffs.lhs Rdiff.o
	ghc -threaded --make rdifffs.lhs

Rdiff.o: Rdiff.hs
	ghc -c Rdiff.hs

clean:
	rm -f rdifffs *.hi *.o

.PHONY: clean
