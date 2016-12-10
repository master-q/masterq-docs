ODGS := $(wildcard draw/*.dia)
PNGS := $(patsubst %.dia,%.png,${ODGS})

all: ${PNGS}

%.png: %.dia
	dia -e $@ $<
