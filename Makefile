CABAL = cabal
GHC_PKG = ghc-pkg

all: build

doc: configure
	$(CABAL) haddock

install: install_deps
	$(CABAL) install

uninstall:
	 $(GHC_PKG) unregister googleplus

build: configure install_deps
	$(CABAL) build

install_deps: googleplus.cabal
	$(CABAL) install --only-dependencies

configure: googleplus.cabal **/*.hs
	$(CABAL) configure

clean:
	$(CABAL) clean
