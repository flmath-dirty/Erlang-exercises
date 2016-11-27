
export ERLC = erlc +debug_info

ERL = erl

compile	:
	mkdir -p ebin 
	$(ERLC) -o ebin \
	*.erl

compile_test :	
	mkdir -p ebin 
	$(ERLC) +export_all -o ebin \
	*.erl

run	:
	$(ERL) -pa ebin

eunit	:
	$(ERL) -pa ebin \
	-s eunit test listReverse_test

all	 : compile run

test	 : compile_test eunit
