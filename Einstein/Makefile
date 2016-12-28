export ERLC = erlc +debug_info

export ERL = erl


compile    :
	mkdir -p ebin
	$(ERLC) -o ebin \
	*.erl

compile_unit :
	mkdir -p ebin
	$(ERLC) +export_all -o ebin \
	*.erl

run    :
	$(ERL) -pa ebin
eunit    :
	$(ERL) -pa ebin  \
	-s eunit test einstein_test

solve_test   :
	$(ERL) -pa ebin  \
	-s eunit test einstein_solution_test


all     : compile run

test     : compile_unit eunit

solution_test : compile_unit solve_test

solve : compile
	$(ERL) -pa ebin  \
	-run einstein_solve solve \
	-run init stop
