PROG = hello.bin
SRC  = hello.ml

all: $(PROG)
$(PROG): $(SRC)
	ocamlopt -g -o $@ $<

clean:
	rm -f *~ *.bin *.cmi *.cmx *.o

.PHONY: all clean
