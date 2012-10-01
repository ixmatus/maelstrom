.PHONY: cabal clean

all: configure compile

compile:
	cabal build

configure:
	cabal configure

clean:
	cabal clean
	rm -f tabula-recta.bytestr
