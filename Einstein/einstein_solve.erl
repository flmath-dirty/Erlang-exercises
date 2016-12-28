%%%-------------------------------------------------------------------
%%% @author FLMath <lematteu@gmail.com>
%%% @copyright (C) 2016, FLMath
%%% @doc
%%%
%%% @end
%%% Created : 21 Dec 2016 by FLMath
%%%-------------------------------------------------------------------
-module(einstein_solve).
-export([solve/0]).

solve()->  
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
    FinalOutput.

