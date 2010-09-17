all: RDiffBackupFS

RDiffBackupFS: RDiffBackupFS.lhs
	ghc -threaded --make RDiffBackupFS.lhs

clean:
	rm -f rdifffs *.hi *.o

.PHONY: clean
