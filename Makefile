PROG=normalize_parser.rb
DEPS=normalize_grammar.tt normalize_nodes.rb

$(PROG): $(DEPS)
	tt -o $@ $<

clean:
	$(RM) $(PROG)
