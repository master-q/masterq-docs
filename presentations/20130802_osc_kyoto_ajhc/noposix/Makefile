SUBDIRS := 1 2 3 4

small clean:
	@for i in $(SUBDIRS); do \
		$(MAKE) -C $$i $@; \
	done

.PHONY: all clean $(SUBDIRS)
