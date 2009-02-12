
all: configure build

configure:
	runhaskell Setup.hs configure

build:
	runhaskell Setup.hs build
	@echo "Finished building. The compiler is at dist/build/cm/cm"

clean:
	runhaskell Setup.hs clean

docs:
	cd doc && xelatex report1

	runhaskell Setup.hs haddock --executables
	-rm -Rf doc/haddock/
	cp -R dist/doc/html/cmc doc/haddock

.PHONY: all configure build clean docs