.DEFAULT_GOAL := build

.PHONY: setup
setup:
	make switch
	make dependencies
	make build

.PHONY: dependencies
dependencies:
	make package
	opam install -y ocamlformat=0.26.1 ocaml-lsp-server utop

.PHONY: switch
switch:
	opam switch create . 5.1.0 --no-install

.PHONY: build
build:
	dune build

.PHONY: package
package:
	opam install . -y --deps-only --with-test

.PHONY: completed
completed:
	dune exec ./completed_days.exe

.PHONY: clean
clean:
	dune clean

.PHONY: fmt
fmt:
	dune build --auto-promote @fmt

.PHONY: utop
utop:
	dune utop .

.PHONY: reset
reset:
	rm -rf ./_opam

day%:
	dune test ./day_$* -w
