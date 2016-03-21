all:
	cabal build

install:
	cabal install

clean:
	cabal clean

test:
	runhaskell RunTests.hs

doc:
	cabal haddock --executable
