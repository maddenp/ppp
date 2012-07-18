TARGETS=clean control
TESTS=$(wildcard t*)

default:
	$(error Valid targets are: $(TARGETS))

clean:
	@$(foreach x,$(TESTS),$(MAKE) --quiet -C $(x) clean;)

control:
	@$(foreach x,$(TESTS),echo "Updating 'control' for test $(x)" && $(MAKE) --quiet -C $(x) control;)
