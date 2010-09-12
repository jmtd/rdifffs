all: RDiffBackupFS

RDiffBackupFS: RDiffBackupFS.lhs RdiffFS.hs main.hs
	ghc -threaded --make main.hs
