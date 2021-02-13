

build: main.ml lang.ml parser.ml
	dune build --profile release

run: main.ml lang.ml parser.ml
	dune exec bin/main.exe --profile release

size: _build/default/main.exe
	ls -lh _build/default/main.exe