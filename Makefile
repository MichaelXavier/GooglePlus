CABAL = cabal
GHC_PKG = ghc-pkg

all: build

install: install_deps
	$(CABAL) install

uninstall:
	 $(GHC_PKG) unregister googleplus

build: configure install_deps
	$(CABAL) build

install_deps: googleplus.cabal
	$(CABAL) install --only-dependencies

configure: googleplus.cabal *.hs **/*.hs
	$(CABAL) configure

clean:
	$(CABAL) clean
