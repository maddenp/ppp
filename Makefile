CLIENT=pppc
PARSERS=$(addsuffix _parser.rb,fortran normalizer sms_fortran sms_normalizer)

.PHONY: clean cleaner

all: $(PARSERS) $(CLIENT)

sms_fortran_parser.rb: fortran_parser.rb
sms_normalizer_parser.rb: normalizer_parser.rb

%_parser.rb: %.tt
	RUBYLIB=lib tt -o $@ $<

$(CLIENT): $(CLIENT).c
	gcc -Wall $^ -lm -o $@

clean:
	$(RM) $(CLIENT) *_parser.rb *.env *.mod *.o a.out socket*

cleaner: clean
	$(RM) $(PARSERS)
