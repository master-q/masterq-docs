PROG       = call-c-fromats
PATSCC     = $(PATSHOME)/bin/patscc
PATSOPT    = $(PATSHOME)/bin/patsopt
CFLAGS     = -O2 -I$(PATSHOME) -I$(PATSHOME)/ccomp/runtime
CLEANFILES = $(PROG) main_dats.c

all: $(PROG)

$(PROG): main.dats
	$(PATSCC) -o $@ $?

clean:
	rm -f $(CLEANFILES)
