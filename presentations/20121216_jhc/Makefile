DOCS := $(wildcard draw/*.odg)
PNGS := $(patsubst %.odg,%.png,${DOCS})

all: ${PNGS}
%.png: %.odg
	unoconv -f png $<

clean:
	rm -f *~

distclean: clean
	rm -f draw/*.png *~

.PHONY: distclean
