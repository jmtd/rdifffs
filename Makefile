all: rdifffs

rdifffs: RdiffBackup.lhs RdiffFS.hs main.hs
	ghc -threaded --make main.hs -o rdifffs
