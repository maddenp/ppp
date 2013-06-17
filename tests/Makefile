TARGETS=clean
TESTS=$(wildcard t*)

default:
	$(error Valid targets are: $(TARGETS))

clean:
	@$(foreach x,$(TESTS),$(MAKE) --quiet -C $(x) clean;)
