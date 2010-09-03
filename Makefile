all: RDiffBackupFS

RDiffBackupFS: RDiffBackupFS.lhs
	ghc -threaded --make RDiffBackupFS.lhs
