all: distclean
				ghc --make -threaded -O2 -rtsopts trs.hs

distclean: clean
				rm -f trs

clean:
				rm -f *.hi
				rm -f *.o
