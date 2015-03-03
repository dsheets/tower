.PHONY: all clean tower test

DIRTY_FLAG=$(shell git diff-index --quiet HEAD || echo "dirty")
ifeq ($(DIRTY_FLAG),dirty)
DIRTY=true
else
DIRTY=false
endif

all: tower

tower.native: towerVersion.ml
	ocamlbuild -use-ocamlfind -cflags -w,@f@p@u@y -Is cli,lib tower.native

tower: tower.native
	mv tower.native tower

towerVersion.ml:
	printf "let git_rev = \""                > towerVersion.ml
	git rev-parse HEAD | tr -d '\n'         >> towerVersion.ml
	printf "\"\nlet git_dirty = $(DIRTY)\n" >> towerVersion.ml

test:
	$(MAKE)
	$(MAKE) -C test

clean:
	ocamlbuild -clean
	rm -f towerVersion.ml tower
	$(MAKE) -C test clean
