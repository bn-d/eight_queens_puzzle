DUNE=dune
DARGS=$(if $(VERBOSE),--verbose,) $(DUNE_ARGS)

clean:
	$(DUNE) clean $(DARGS)

build:
	$(DUNE) build $(DARGS)

run:
	$(DUNE) exec $(DARGS) src/eight_queens.exe

format:
	$(DUNE) build @fmt --auto-promote $(DARGS)

.PHONY: clean build test format
.DEFAULT: build

