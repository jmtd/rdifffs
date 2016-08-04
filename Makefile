all: rdifffs Test

rdifffs: rdifffs.hs Rdiff.o
	ghc -threaded --make rdifffs.hs

Rdiff.o: Rdiff.hs
	ghc -c Rdiff.hs

util/grepCopy: util/grepCopy.hs Rdiff.o
	ghc -threaded --make util/grepCopy.hs

Test: Test.hs
	rm -f Rdiff.o # so we get hpc coverage
	ghc -fhpc Test.hs --make

hpc:
	mkdir hpc

check: Test hpc
	./Test
	hpc report Test
	hpc markup --destdir hpc Test

clean:
	rm -f rdifffs *.hi *.o Test hpc *.tix

.PHONY: clean check
