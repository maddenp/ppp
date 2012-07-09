TESTS=$(wildcard t*)

default:
	$(error Valid targets are: $(TARGETS))

clean:
	$(foreach x,$(TESTS),$(MAKE) -C clean)
