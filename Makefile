BINS=$(addsuffix _parser.rb,fortran normfree sms_fortran sms_normfree) pppc

all: $(BINS)

sms_fortran_parser.rb: fortran_parser.rb
sms_normfree_parser.rb: normfree_parser.rb

%_parser.rb: %.tt
	RUBYLIB=lib tt -o $@ $<

pppc: pppc.c
	gcc -Wall $^ -lm -o $@

clean:
	$(RM) $(BINS) *.env *.mod *.o a.out
