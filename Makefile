CLIENT=pppc
PARSERS=$(addsuffix _parser.rb,fortran normfixed normfree sms_fortran sms_normfixed sms_normfree)

.PHONY: clean cleaner

all: $(PARSERS) $(CLIENT)

sms_fortran_parser.rb: fortran_parser.rb
sms_normfixed_parser.rb: normfixed_parser.rb
sms_normfree_parser.rb: normfree_parser.rb

%_parser.rb: %.tt
	RUBYLIB=lib tt -o $@ $<

$(CLIENT): $(CLIENT).c
	gcc -Wall $^ -lm -o $@

clean:
	$(RM) $(CLIENT) *.env *.mod *.o a.out socket*

cleaner: clean
	$(RM) $(PARSERS)
