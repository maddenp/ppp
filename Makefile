BINS=$(addsuffix _parser.rb,fortran normfree)

all: $(BINS)

%_parser.rb: %.tt
	RUBYLIB=lib tt -o $@ $<

clean:
	$(RM) $(BINS) *.mod *.o *.sms a.out
