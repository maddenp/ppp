TARGETS=clean control
TESTS=$(wildcard t*)

default:
	$(error Valid targets are: $(TARGETS))

clean:
	$(foreach x,$(TESTS),$(MAKE) -C $(x) clean;)

control:
	$(error Bad idea (see README))
