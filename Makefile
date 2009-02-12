
all: configure build

configure:
	runhaskell Setup.hs configure

build:
	runhaskell Setup.hs build
	@echo "Finished building. The compiler is at dist/build/cm/cm"

clean:
	runhaskell Setup.hs clean

docs:
	runhaskell Setup.hs haddock
	runhaskell Setup.hs haddock --hoogle
	-rm -Rf doc/haddock/
	cp -R dist/doc/html/cmc doc/haddock

.PHONY: all configure build clean docs