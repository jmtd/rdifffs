all: rdifffs

rdifffs: rdifffs.lhs
	ghc -threaded --make rdifffs.lhs

clean:
	rm -f rdifffs *.hi *.o

.PHONY: clean
