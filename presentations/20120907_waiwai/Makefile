TARGETS := WaiStaticApp WaiStaticWebkit WaiEchoApp

all: $(TARGETS)

$(TARGETS):
	ghc -threaded $@.hs

clean:
	rm -rf *~ *.o *.hi $(TARGETS)

.PHONY: clean
