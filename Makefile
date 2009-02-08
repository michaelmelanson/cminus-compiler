
all: configure build

configure:
	runhaskell Setup.hs configure

build:
	runhaskell Setup.hs build
	@echo "Finished building. The compiler is at dist/build/cm/cm"

clean:
	runhaskell Setup.hs clean