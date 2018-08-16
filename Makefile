all: build bench_tool

dev:
	dune build --dev -j16

build:
	dune build

bench_tool:
	ln -s _build/install/default/bin/$@ ./$@

clean:
	dune clean

test:
	dune runtest

preprocess:
	dune build @preprocess

promote:
	dune promote

.PHONY: all build dev clean test promote
