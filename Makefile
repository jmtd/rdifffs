all: rdifffs

rdifffs: RdiffBackup.lhs RdiffFS.hs main.hs
	ghc -threaded --make main.hs -o rdifffs

clean:
	rm -f rdifffs *.hi *.o

.PHONY: clean

