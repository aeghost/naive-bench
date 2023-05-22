# This makefile is not exaustive and act here as not mush than a `.sh` file...

.PHONY: all run build clean dist-clean

all: run

run:
	dune exec bin/naive_bench.exe

build:
	dune build bin/naive_bench.exe

clean:
	dune clean

dist-clean: clean
	rm -rf _socks/tmp