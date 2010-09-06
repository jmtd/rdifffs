all: RDiffBackupFS

RDiffBackupFS: RDiffBackupFS.lhs RdiffFS.hs
	ghc -threaded --make RDiffBackupFS.lhs
