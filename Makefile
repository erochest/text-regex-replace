
SRC=$(shell find src -name '*.hs')

all: init test docs package

init: stack.yaml

test:
	stack test

docs:
	stack haddock

package: test
	hpack
	stack sdist

upload: package
	stack upload --pvp-bounds lower .

tags: configure ${SRC}
	codex update

hlint:
	hlint src specs

clean:
	stack clean
	codex cache clean
	-rm -rf dist

distclean: clean

configure:
	cabal configure --package-db=clear --package-db=global --package-db=`stack path --snapshot-pkg-db` --package-db=`stack path --local-pkg-db`

build:
	stack build

ghcid:
	ghcid --command="stack ghci"

watch:
	stack test --file-watch

restart: distclean init build

rebuild: clean configure build

stack.yaml:
	stack init --prefer-nightly

.PHONY: all init test clean distclean configure build rebuild hlint ghcid watch
