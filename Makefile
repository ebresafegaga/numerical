

build: main.ml lang.ml parser.ml
	dune build ./main.exe --profile release

run: main.ml lang.ml parser.ml
	dune exec ./main.exe --profile release