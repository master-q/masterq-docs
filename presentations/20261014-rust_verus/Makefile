MINUTES := 5
TARGET := slides
SRC    := $(TARGET).md
PDF    := $(TARGET).pdf
HTML   := $(TARGET).html

.PHONY: all show html pdf clean

all: show

show:
	RABBIT_MINUTES=$(MINUTES) marp --engine ./rabbit-engine.js --preview $(SRC)

html: $(HTML)

$(HTML): $(SRC)
	marp -o $@

pdf: $(SRC)
	marp --pdf

clean:
	rm -f $(PDF) $(HTML)
