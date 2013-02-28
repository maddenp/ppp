BINS=$(addsuffix _parser.rb,fortran normfree sms)

all: $(BINS)

sms_parser.rb: fortran_parser.rb

%_parser.rb: %.tt
	RUBYLIB=lib tt -o $@ $<

clean:
	$(RM) $(BINS) *.env *.mod *.o a.out
