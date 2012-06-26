## Makefile for brain++



FILES = $(wildcard src/*.bfp)
OUT = $(addprefix bin/, $(notdir $(FILES:.bfp=.bf)))



all: $(OUT)

clean:
	rm -fv $(OUT)

$(OUT):
	cpp -I include $< | egrep -v "^#" | tr -d '[:space:]' > $@



dependency_rule = bin/$(notdir $(1:.bfp=.bf)): $1 include/*.bfh
$(foreach file,$(FILES),$(eval $(call dependency_rule,$(file))))
