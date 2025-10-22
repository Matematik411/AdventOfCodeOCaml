.PHONY: build run clean format

build:
	dune build

run:
	./main.exe $(ARGS)

clean:
	dune clean

format:
	dune build @fmt --auto-promote

watch:
	dune build --watch