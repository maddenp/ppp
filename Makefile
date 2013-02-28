BINS=$(addsuffix _parser.rb,fortran normfree)

all: $(BINS)

%_parser.rb: %.tt
	RUBYLIB=lib tt -o $@ $<

clean:
	$(RM) $(BINS) *.env *.mod *.o a.out
