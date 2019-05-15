all:
	@echo ">>install cabal"
	sudo apt-get install cabal-install
	cabal update
	@echo ">>cabal installed"
	@echo ">>install zlib"
	sudo apt-get install zlibc zlib1g-dev
	@echo ">>install idris"
	cabal install idris
