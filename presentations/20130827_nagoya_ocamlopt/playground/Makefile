PROG = hello.bin
SRC  = hello.ml

all: $(PROG)
$(PROG): $(SRC)
	ocamlopt -g -runtime-variant d -o $@ $<

clean:
	rm -f *~ *.bin *.cmi *.cmx *.o

.PHONY: all clean
