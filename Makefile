all: RDiffBackupFS

RDiffBackupFS: RDiffBackupFS.hs
	ghc -threaded --make RDiffBackupFS.hs
