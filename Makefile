BINS=$(addsuffix _parser.rb,fortran normalize)

all: $(BINS)

%_parser.rb: %_grammar.tt
	RUBYLIB=lib tt -o $@ $<

clean:
	$(RM) $(BINS) *.mod *.o a.out
