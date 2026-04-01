%.zip: $(shell find . -name "$*/*.rkt" -o -name "$*/*.[ch]" -o -name "Makefile")
	-$(RM) $@
	zip -r $@ $* \
		-x \*.[os] \
		-x \*.DS_Store \
		-x \*~ \
		-x \*zip \
		-x \*Zone.Identifier \
		-x \*\*compiled\*\*

.PHONY: clean

clean:
	-$(RM) *.zip
