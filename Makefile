all: RDiffBackupFS

RDiffBackupFS: RDiffBackupFS.lhs RdiffFS.hs
	ghc -threaded --make RDiffBackupFS.lhs

clean:
	rm -f rdifffs *.hi *.o

.PHONY: clean
