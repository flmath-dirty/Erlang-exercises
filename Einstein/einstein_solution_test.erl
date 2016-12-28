%%%-------------------------------------------------------------------
%%% @author FLMath <lematteu@gmail.com>
%%% @copyright (C) 2016, FLMath
%%% @doc
%%%
%%% @end
%%% Created : 21 Dec 2016 by FLMath
%%%-------------------------------------------------------------------

-module(einstein_solution_test).

-include_lib("eunit/include/eunit.hrl").
-compile([export_all]).

-define(SOLUTION, [
     [5,swedish,white,dog,beer,bluemaster],
     [4,german,green,fish,coffee,prince],
     [3,british,red,bird,milk,pall_mall],	
     [2,danish,blue,horse,tea,blend],
     [1,norwegian,yellow,cat,water,dunhill]]).

space_full_element_test()->
    Input =[[1,2,3,4,5],
            [british, swedish, danish, norwegian, german],
            [red,green,white,yellow,blue],
            [dog,bird,fish,cat,horse],
            [tea,coffee,milk,beer,water],
           [pall_mall,dunhill,blend,prince,bluemaster]],
    Output = 5*5*5*5*5*5,
    ?assertMatch(Output, length(sofs:to_external(einstein:space(Input)))). 

restrict_atomic_basic_test()->
    Rules= einstein_rules:space_restriction_rules(),

    Input =[[1,2,3,4,5],
            [british, swedish, danish, norwegian, german],
            [red,green,white,yellow,blue],
            [dog,bird,fish,cat,horse],
            [tea,coffee,milk,beer,water],
            [pall_mall,dunhill,blend,prince,bluemaster]],
    Output = 133,
    Solution = ?SOLUTION,
    CalculatedOutput=einstein:restrict(
		       Rules,
		       sofs:to_external(
			 einstein:space(Input))),
    ?assertMatch(Output, length(CalculatedOutput )),
    ?assertMatch([], lists:subtract(Solution,CalculatedOutput)).

restrict_atomic_specific_tuples_test()->
    Input  = 
	[{4,german,green,fish,coffee,prince},
	 {3,british,red,bird,milk,pall_mall}],
    Output = 
	[[3,british,red,bird,milk,pall_mall],
	 [4,german,green,fish,coffee,prince]
	 ],
    RestrictionRules = 
	[
	 einstein_rules:british_in_red_house(),
	 einstein_rules:swedish_has_dog(),
	 einstein_rules:danish_drinks_tea(),
	 einstein_rules:green_homee_drinks_coffee(),
	 einstein_rules:pall_mall_smoker_has_bird(),
	 einstein_rules:yellow_homee_smokes_dunhill(),
	 einstein_rules:middle_homee_drinks_milk(),
	 einstein_rules:first_homee_is_norwegian(),
	 einstein_rules:bluemaster_smoker_drinks_beer(),
	 einstein_rules:prince_smoker_is_german()
	],
    CalculatedOutput=einstein:restrict(
		       RestrictionRules,
		      Input),
    ?assertMatch(Output, lists:sort(CalculatedOutput)).
   


generate_possible_solution_test()->  
    SolutionFilterFuns = [
			  fun einstein_filters:max_values/1],
    ConstructionReductionFuns = 
	[
	 fun einstein_general_rules:does_house_descritpion_intersects_with_scenario/2 |
	 einstein_rules:generating_restriction_rules()
	],

    Rules= einstein_rules:space_restriction_rules(),
   
    Input =[[1,2,3,4,5],
            [british, swedish, danish, norwegian, german],
            [red,green,white,yellow,blue],
            [dog,bird,fish,cat,horse],
            [tea,coffee,milk,beer,water],
            [pall_mall,dunhill,blend,prince,bluemaster]],
    Output = einstein:restrict(Rules,
			       sofs:to_external(
				 einstein:space(Input))),
    FinalOutput = einstein:generate_valid_scenarios(
		    SolutionFilterFuns, 
		    ConstructionReductionFuns, 
		    Output),
    io:format("~p~n",[FinalOutput]),
    AssumedOutput=?SOLUTION,
    ?assertMatch([AssumedOutput],FinalOutput).
