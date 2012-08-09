BINS=$(addsuffix _parser.rb,fortran normalize)

all: $(BINS)

%_parser.rb: %_grammar.tt %_nodes.rb
	tt -o $@ $<

clean:
	$(RM) $(BINS) *.mod a.out
