## Makefile for brain++



FILES = $(wildcard src/*.bfp)
OUT = $(addprefix out/, $(notdir $(FILES:.bfp=.bf)))
SED = ':a;s/?/\#/g;s/  \+/ /;s/+ +/++/;s/- -/--/;s/< </<</;s/> >/>>/;s/[ [/[[/;s/] ]/]]/;s/+ \?-//;s/- \?+//;s/< \?>//;s/> \?<//;s/] *\[ *- *]/]/;ta'



all: $(OUT)

clean:
	rm -fv $(OUT)

distclean: clean;

$(OUT):
	@echo "cpp -I include -P $< | sed '<magic>' > $@"
	@cpp -I include -P $< | sed $(SED) > $@



dependency_rule = out/$(notdir $(1:.bfp=.bf)): $1 include/*.bfh
$(foreach file,$(FILES),$(eval $(call dependency_rule,$(file))))
