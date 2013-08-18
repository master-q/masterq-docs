PROG = hello.bin
SRC  = hello.ml
DEBUG_OPT = # -g -runtime-variant d

all: $(PROG)
$(PROG): $(SRC)
	ocamlopt $(DEBUG_OPT) -dstartup -o $@ $<

clean:
	rm -f *~ *.bin *.cmi *.cmx *.o *.startup.s

.PHONY: all clean
