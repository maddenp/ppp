BINS=$(addsuffix _parser.rb,fortran normalize)

all: $(BINS)

%_parser.rb: %_grammar.tt %_nodes.rb
	RUBYLIB=lib tt -o $@ $<

clean:
	$(RM) $(BINS) *.mod a.out
