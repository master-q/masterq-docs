ODGS := $(wildcard draw/*.odg)
PNGS := $(patsubst %.odg,%.png,${ODGS})

all: ${PNGS}

%.png: %.odg
	unoconv -n -f png -o unoconv_tmp $< 2> /dev/null   || \
          unoconv -f png -o unoconv_tmp $<                 || \
	  unoconv -n -f png -o unoconv_tmp $< 2> /dev/null || \
          unoconv -f png -o unoconv_tmp $<
	convert -resize 800x unoconv_tmp.png $@
	rm -f unoconv_tmp.png

.PHONY: distclean
