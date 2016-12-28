%%%-------------------------------------------------------------------
%%% @author FLMath <lematteu@gmail.com>
%%% @copyright (C) 2016, FLMath
%%% @doc
%%%
%%% @end
%%% Created : 21 Dec 2016 by FLMath
%%%-------------------------------------------------------------------
-module(einstein_test).

-include_lib("eunit/include/eunit.hrl").
-compile([export_all]).

space_empty_test()->
    Input =[],
    Output = [],
    ?assertMatch(Output, einstein:space(Input)).   

space_one_element_test()->
    Input =[[british, swedish, danish, norwegian, german]],
    Output = lists:sort([{british}, {swedish}, {danish}, {norwegian}, {german}]),
    ?assertMatch(Output, lists:sort(sofs:to_external(einstein:space(Input)))).   


space_two_element_test()->
    Input =[[british, swedish, danish, norwegian, german],
            [red,green,white,yellow,blue]],
    Output =
        lists:sort(
          [{british,red}, {swedish,red}, {danish,red}, {norwegian,red}, {german,red},
           {british,green}, {swedish,green}, {danish,green}, {norwegian,green}, {german,green},
           {british,white}, {swedish,white}, {danish,white}, {norwegian,white}, {german,white},
           {british,yellow}, {swedish,yellow}, {danish,yellow}, {norwegian,yellow}, {german,yellow},
           {british,blue}, {swedish,blue}, {danish,blue}, {norwegian,blue}, {german,blue}]),
    ?assertMatch(Output,  lists:sort(sofs:to_external(einstein:space(Input)))).   

space_full_element_test()->
    Input =[[1,2,3,4,5],
            [british, swedish, danish, norwegian, german],
            [red,green,white,yellow,blue],
            [dog,bird,fish,cat,horse],
            [tea,coffee,milk,beer,water],
           [pall_mall,dunhill,blend,prince,bluemaster]],
    Output = 5*5*5*5*5*5,
    ?assertMatch(Output, length(sofs:to_external(einstein:space(Input)))). 

restrict_basic_test()->
    Rule_1 = einstein_rules:british_in_red_house(),
      
    Input =[[1,2,3,4,5],
            [british, swedish, danish, norwegian, german],
            [red,green,white,yellow,blue],
            [dog,bird,fish,cat,horse],
            [tea,coffee,milk,beer,water],
            [pall_mall,dunhill,blend,prince,bluemaster]],
    Output = 10625,
    ?assertMatch(Output, length(
                           einstein:restrict(
                             [Rule_1,Rule_1],
                             sofs:to_external(
                               einstein:space(Input))))).
restrict_two_basic_test()->
    Rule_1 = einstein_rules:british_in_red_house(),
    Rule_2 = einstein_rules:swedish_has_dog(),
   
    Input =[[1,2,3,4,5],
            [british, swedish, danish, norwegian, german],
            [red,green,white,yellow,blue],
            [dog,bird,fish,cat,horse],
            [tea,coffee,milk,beer,water],
            [pall_mall,dunhill,blend,prince,bluemaster]],
    Output = 7000,
    ?assertMatch(Output, length(
                           einstein:restrict(
                             [Rule_1,Rule_2],
                             sofs:to_external(
                               einstein:space(Input))))).
does_intersect_test()->
    Input1 = [british,red],
    Input2 = [swedish,red],  
    Output = true,
    ?assertMatch(Output, einstein_general_rules:does_intersect(Input1,Input2)).

does_intersect_false_test()->
     Input1 = [british,green],
     Input2 = [swedish,red],  
     Output = false,
     ?assertMatch(Output, einstein_general_rules:does_intersect(Input1,Input2)).

does_house_descritpion_intersects_with_scenario_true_test()->
     Input = [british,green],
     InputScenario = [[swedish,red],[british,yellow]],
     Output = true,
     ?assertMatch(Output, 
		  einstein_general_rules:does_house_descritpion_intersects_with_scenario(
		  InputScenario, Input)).

does_house_descritpion_intersects_with_scenario_false_test()->
     Input = [german,green],
     InputScenario = [[swedish,red],[british,yellow]],
     Output = false,
     ?assertMatch(Output, 
		  einstein_general_rules:does_house_descritpion_intersects_with_scenario(
		  InputScenario, Input)).

is_length_right_test()->
    Input = [[british,red], [swedish,red], [british,green], [swedish,green]],
    InputFun  = einstein_general_rules:is_right_length_factory(4),
    Output = true,
    ?assertMatch(Output, InputFun(Input)).
is_length_right_fail_test()->
    Input = [[swedish,red], [british,green], [swedish,green]],
    InputFun  = einstein_general_rules:is_right_length_factory(4),
    Output = false,
    ?assertMatch(Output, InputFun(Input)).


space_three_element_test()->
    Input =[[british, swedish, danish],
            [red,green,white]],
    Output =
        lists:sort(
          [{british,red}, {swedish,red}, {danish,red},
           {british,green}, {swedish,green}, {danish,green},
           {british,white}, {swedish,white}, {danish,white}
          ]),
    ?assertMatch(Output,  lists:sort(sofs:to_external(einstein:space(Input)))).

find_houseNo_test()->
    Input1 = [[1,british, red, dog, beer,dunhill]],
    Input2 = [3,swedish, blue, horse, water,pall_mall],
    Output = 3,
   
    ?assertMatch(Output, einstein_general_rules:find_houseNo(Input1, Input2, {4,horse})).

find_houseNo2_test()->
    Input1 = [[1,british, red, dog, beer,dunhill],
	      [2,swedish, green, cat, beer, pall_mall]],
    Input2 = [3,swedish, blue, horse, water,pall_mall],
    Output = 2,
   
    ?assertMatch(Output, einstein_general_rules:find_houseNo(Input1, Input2, {3,green})).
find_houseNo3_test()->
    Input1 = [[1,british, red, dog, beer,dunhill],
	      [2,swedish, green, cat, beer, pall_mall]],
    Input2 = [3,swedish, blue, horse, water,pall_mall],
    Output = false,
    ?assertMatch(Output, einstein_general_rules:find_houseNo(Input1, Input2, {3,yellow})).

get_house_nums_test()->
    Input1 = [[1,british, red, dog, beer,dunhill],
	      [2,swedish, green, cat, beer, pall_mall]],
    Input2 = [3,swedish, blue, horse, water,pall_mall],
    Output = [1,2,3],
    ?assertMatch(Output,lists:sort(einstein_general_rules:get_house_nums(Input1, Input2))).

neighbour_1_test()->
    Input1 = [[1,british, red, dog, beer,dunhill],
	      [2,swedish, green, cat, beer, bluemaster]],
    Input2 = [3,german, blue, horse, water,pall_mall],
    Rule=[{4,dog},{6,pall_mall}],
    Output = true,
    ?assertMatch(Output,
		 einstein_general_rules:distant_neighbour_rejection_test(
		   Input1, Input2,Rule)).
neighbour_not_rejected_test()->
    Input1 = [[1,british, red, dog, beer,dunhill],
	      [2,swedish, green, cat, beer, bluemaster]],
    Input2 = [3,german, blue, horse, water,pall_mall],
    Rule=[{4,cat},{6,pall_mall}],
    Output = false,
    ?assertMatch(Output,
		 einstein_general_rules:distant_neighbour_rejection_test(
		   Input1, Input2,Rule)).
neighbour_not_rejected_2_test()->
    Input1 = [[1,british, red, dog, beer,dunhill],
	      [2,swedish, green, bird, beer, bluemaster]],
    Input2 = [3,german, blue, horse, water,pall_mall],
    Rule=[{4,horse},{6,prince}],
    Output = false,
    ?assertMatch(Output,
		 einstein_general_rules:distant_neighbour_rejection_test(
		   Input1, Input2, Rule)).

neighbour_not_rejected_3_test()->
    Input1 = [[1,british, red, dog, beer,dunhill],
	      [2,swedish, green, bird, beer, bluemaster]],
    Input2 = [3,german, blue, horse, water,pall_mall],
    Rule=[{4,cat},{6,prince}],
    Output = false,
    ?assertMatch(Output,
		 einstein_general_rules:distant_neighbour_rejection_test(
		   Input1, Input2, Rule)).
is_on_the_left_test()->
    Input1 = [[1,british, red, dog, wine,dunhill],
	      [2,swedish, green, cat, beer, bluemaster]],
    Input2 = [3,german, blue, horse, water,pall_mall],
    RuleLeft={4,dog},
    RuleRight={5,water},
    Output = true,
    ?assertMatch(Output,
		 einstein_general_rules:left_neighbour_rejection_test(
		   Input1, Input2,RuleLeft,RuleRight)).
is_on_the_left_2_test()->
    Input1 = [[1,british, red, cat, wine,dunhill],
	      [2,swedish, green, dog, beer, bluemaster]],
    Input2 = [3,german, blue, horse, water,pall_mall],
    RuleLeft={4,dog},
    RuleRight={5,water},
    Output = false,
    ?assertMatch(Output,
		 einstein_general_rules:left_neighbour_rejection_test(
		   Input1, Input2,RuleLeft,RuleRight)).

is_on_the_left_3_test()->
    Input1 = [[1,british, red, cat, wine,dunhill],
	      [2,swedish, green, dog, beer, bluemaster]],
    Input2 = [3,german, blue, horse, water,pall_mall],
    RuleLeft={4,bird},
    RuleRight={5,water},
    Output = true,
    ?assertMatch(Output,
		 einstein_general_rules:left_neighbour_rejection_test(
		   Input1, Input2,RuleLeft,RuleRight)).

is_on_the_left_4_test()->
    Input1 = [[1,british, red, cat, wine,dunhill],
	      [2,swedish, green, dog, beer, bluemaster]],
    Input2 = [3,german, blue, horse, water,pall_mall],
    RuleLeft={4,horse},
    RuleRight={5,tea},
    Output = false,
    ?assertMatch(Output,
		 einstein_general_rules:left_neighbour_rejection_test(
		   Input1, Input2,RuleLeft,RuleRight)).
is_on_the_left_5_test()->
    Input1 = [[1,british, red, cat, wine,dunhill],
	      [2,swedish, green, dog, beer, bluemaster]],
    Input2 = [3,german, blue, horse, water,pall_mall],
    RuleLeft={4,bird},
    RuleRight={5,tea},
    Output = false,
    ?assertMatch(Output,
		 einstein_general_rules:left_neighbour_rejection_test(
		   Input1, Input2,RuleLeft,RuleRight)).
is_on_the_left_6_test()->
    Input1 = [[1,british, red, cat, wine,dunhill],
	      [4,swedish, green, dog, beer, bluemaster]],
    Input2 = [3,german, blue, horse, water,pall_mall],
    RuleLeft={4,bird},
    RuleRight={5,water},
    Output = false,
    ?assertMatch(Output,
		 einstein_general_rules:left_neighbour_rejection_test(
		   Input1, Input2,RuleLeft,RuleRight)).

is_on_the_left_7_test()->
    Input1 = [[1,british, red, cat, wine,dunhill],
	      [4,swedish, green, dog, beer, bluemaster]],
    Input2 = [3,german, blue, horse, water,pall_mall],
    RuleLeft={4,bird},
    RuleRight={5,wine},
    Output = true,
    ?assertMatch(Output,
		 einstein_general_rules:left_neighbour_rejection_test(
		   Input1, Input2,RuleLeft,RuleRight)).
is_on_the_left_8_test()->
    Input1 = [[1,british, red, cat, wine,dunhill],
	      [5,swedish, green, dog, beer, bluemaster]],
    Input2 = [3,german, blue, horse, water,pall_mall],
    RuleLeft={4,dog},
    RuleRight={5,tea},
    Output = true,
    ?assertMatch(Output,
		 einstein_general_rules:left_neighbour_rejection_test(
		   Input1, Input2,RuleLeft,RuleRight)).

left_neighbour_factory_1_test()->
    Input1 = [[1,british, red, dog, beer,dunhill],
	      [2,swedish, green, cat, beer,pall_mall]],
    Input2 = [3,german, blue, horse, water,bluemaster],
    Rule_fun = einstein_general_rules:left_neighbour_test_factory(
		 {4,dog},{6,pall_mall}),
    Output = false,
    ?assertMatch(Output, Rule_fun(Input1, Input2)).

neighbour_factory_1_test()->
    Input1 = [[1,british, red, dog, beer,dunhill],
	      [2,swedish, green, cat, beer, bluemaster]],
    Input2 = [3,german, blue, horse, water,pall_mall],
    Rule_fun = einstein_general_rules:neighbour_test_factory(
		 [{4,dog},{6,pall_mall}]),
    Output = true,
    ?assertMatch(Output, Rule_fun(Input1, Input2)).

rules_restrictions_test()->
    Input1 = [[1,british, red, dog, beer,dunhill]],
    Input2 = [3,swedish, blue, horse, water,pall_mall],
  
    Output = false,
    Rule = einstein_rules:blend_smoker_house_next_to_cat_owner() ,
    ?assertMatch(Output, Rule(Input1,Input2)).

rules_not_neighbour_restrictions_test()->
    Input1 = [[1,british, red, dog, beer,blend]],
    Input2 = [3,swedish, blue, horse, water,pall_mall],
  
    Output = false,
    Rule = einstein_rules:blend_smoker_house_next_to_cat_owner(),
    ?assertMatch(Output, Rule(Input1,Input2)).

rules_neighbour_restrictions_test()->
    Input1 = [[1,british, red, dog, beer,blend]],
    Input2 = [3,swedish, blue, cat, water,pall_mall],
    Output = true,
    Rule = einstein_rules:blend_smoker_house_next_to_cat_owner(),
    ?assertMatch(Output, Rule(Input1,Input2)).

rules_neighbour_positive_restrictions_test()->
    Input1 = [[1,british, red, dog, beer,blend]],
    Input2 = [2,swedish, blue, cat, water,pall_mall], 

    Output = false,
    Rule = einstein_rules:blend_smoker_house_next_to_cat_owner(),
    ?assertMatch(Output, Rule(Input1,Input2)).

apply_disjoint_restriction_step_test()->
    Input1 = [[1,british, red, dog, beer, blend]],
    Input2 = [2,swedish, blue, cat, water, pall_mall],
                         
    Output = false,
    ?assertMatch(Output, 
		 einstein_general_rules:does_house_descritpion_intersects_with_scenario(
		   Input1,Input2)).

intersect_restriction_step_test()->
     Input1 = [[1,british, red, dog, beer, blend]],
     Input2 = [[2,swedish, blue, cat, water, pall_mall]],
     InputFun = [fun einstein_general_rules:does_house_descritpion_intersects_with_scenario/2],

     Output = [[2,swedish, blue, cat, water,pall_mall]],
       
     ?assertMatch(Output, einstein:get_compatibile_scenarios(InputFun,Input1,Input2)).

many_disjoint_restriction_step_test()->
    Input1 = [[1,british, red, dog, beer, blend]],
    Input2 = [[2,swedish, blue, cat, water, pall_mall],
              [2,swedish, red, cat, water, pall_mall],
              [2,swedish, green, cat, beer, pall_mall],
              [1,swedish, green, cat, wine, pall_mall]
             ],
    InputFun = [fun einstein_general_rules:does_house_descritpion_intersects_with_scenario/2                   
	       ],
    Output = [[2,swedish, blue, cat, water,pall_mall]],
       
    ?assertMatch(Output, einstein:get_compatibile_scenarios(InputFun,Input1,Input2)).

many_disjoint_restriction_step_2_test()->
    Input1 = [[1,british, red, dog, beer, blend],
              [2,swedish, blue, cat, water, pall_mall]],
    Input2 = [
              [2,swedish, red, cat, water, pall_mall],
              [3,danish, white, horse, wine, bluemaster],
              [2,swedish, green, cat, beer, pall_mall],
              [4,norwegian, yellow, bird, wine, dunhill],
              [1,swedish, green, cat, wine, pall_mall]
             ],
    InputFun = [fun einstein_general_rules:does_house_descritpion_intersects_with_scenario/2                   
               ],
    Output = lists:sort([
                         [3,danish, white, horse, wine, bluemaster],
                         [4,norwegian, yellow, bird, wine, dunhill]
                        ]),
       
    ?assertMatch(Output, lists:sort(
                           einstein:get_compatibile_scenarios(InputFun,Input1,Input2))).



many_funs_disjoint_restriction_step_test()->
    Input1 = [[1,british, red, dog, beer, blend],
              [2,swedish, blue, cat, water, pall_mall]],
    Input2 = [
              [2,swedish, red, cat, water, pall_mall],
              [4,danish, white, horse, wine, blend],
              [2,swedish, green, cat, beer, pall_mall],
              [4,norwegian, yellow, bird, wine, dunhill],
              [1,swedish, green, cat, wine, pall_mall]
             ],
    InputFun = [fun einstein_general_rules:does_house_descritpion_intersects_with_scenario/2,                   
                einstein_rules:blend_smoker_house_next_to_cat_owner()
               ],
    Output = lists:sort([
                         [4,norwegian, yellow, bird, wine, dunhill]
                        ]),
       
    ?assertMatch(Output, lists:sort(
                           einstein:get_compatibile_scenarios(InputFun,Input1,Input2))).

blend_smoker_restriction_step_test()->
    Input1 = [[1,british, red, dog, beer, blend],
              [2,swedish, blue, bird, water, pall_mall]],
    Input2 = [
              [2,swedish, red, cat, water, pall_mall],
              [4,danish, white, horse, wine, blend],
              [2,swedish, green, cat, beer, pall_mall],
              [4,norwegian, yellow, cat, wine, dunhill],
              [1,swedish, green, cat, wine, pall_mall]
             ],
    InputFun = [fun einstein_general_rules:does_house_descritpion_intersects_with_scenario/2,                   
                einstein_rules:blend_smoker_house_next_to_cat_owner()
               ],
    Output = lists:sort([]),
       
    ?assertMatch(Output, lists:sort(
                           einstein:get_compatibile_scenarios(InputFun,Input1,Input2))).

blend_smoker_restriction_symetric_step_test()->
    Input1 = [[1,british, red, dog, beer, blend],
	      [4,norwegian, yellow, horse, wine, dunhill]],
              
    Input2 = [
              [2,swedish, red, cat, water, pall_mall],
              [4,danish, white, horse, wine, blend],
              [2,swedish, green, cat, beer, pall_mall],
              [2,swedish, blue, bird, water, pall_mall],
              [1,swedish, green, cat, wine, pall_mall]
             ],
    InputFun = [fun einstein_general_rules:does_house_descritpion_intersects_with_scenario/2,
                einstein_rules:blend_smoker_house_next_to_cat_owner()
               ],
    Output = lists:sort([]),
       
    ?assertMatch(Output, lists:sort(
                           einstein:get_compatibile_scenarios(InputFun,Input1,Input2))).

many_funs_2_disjoint_restriction_step_test()->
    Input1 = [[1,british, red, dog, beer, bluemaster],
              [2,swedish, blue, cat, water, pall_mall]],
    Input2 = [
              [2,swedish, red, cat, water, pall_mall],
              [3,danish, white, horse, wine, blend],
              [2,swedish, green, cat, beer, pall_mall],
              [4,norwegian, yellow, bird, wine, dunhill],
              [1,swedish, green, cat, wine, pall_mall]
             ],
    InputFun = [fun einstein_general_rules:does_house_descritpion_intersects_with_scenario/2,                   
                einstein_rules:blend_smoker_house_next_to_cat_owner()
               ],
    Output = lists:sort([
                         [3,danish, white, horse, wine, blend],
                         [4,norwegian, yellow, bird, wine, dunhill]
                        ]),
       
    ?assertMatch(Output, lists:sort(
                           einstein:get_compatibile_scenarios(InputFun,Input1,Input2))).


many_funs_3_disjoint_restriction_step_test()->
    Input1 = [[1,british, red, dog, beer, bluemaster],
              [2,swedish, blue, cat, water, pall_mall]],
    Input2 = [
              [2,swedish, red, cat, water, pall_mall],
              [4,danish, white, horse, wine, blend],
              [2,swedish, green, cat, beer, pall_mall],
              [3,norwegian, yellow, bird, wine, dunhill],
              [1,swedish, green, cat, wine, pall_mall]
             ],
    InputFun = [fun einstein_general_rules:does_house_descritpion_intersects_with_scenario/2,                   
                einstein_rules:blend_smoker_house_next_to_cat_owner()
               ],
    Output = lists:sort([]),
       
    ?assertMatch(Output, lists:sort(
                           einstein:get_compatibile_scenarios(InputFun,Input1,Input2))).

create_Scenario_PossibleExtensionList_tuples_null_test()->
    Input = [],
    Output = [],
    
    ?assertMatch(Output, 
		 einstein:create_Scenario_PossibleExtensionList_tuples(Input)).

create_Scenario_PossibleExtensionList_tuples_one_test()->
    Input =  [
	      [4,danish, white, horse, wine, blend]
             ],
    Output = [
	      {
		[[4,danish, white, horse, wine, blend]],[]
	      }],

    ?assertMatch(Output, 
		 einstein:create_Scenario_PossibleExtensionList_tuples(Input)).


create_Scenario_PossibleExtensionList_tuples_two_element_test()->
    Input =  [
              [3,norwegian, yellow, bird, wine, dunhill],
	      [4,danish, white, horse, wine, blend]
             ],
    Output =
	  sort_Scenario_PossibleExtensionList_tuples(
	  [
	      {[[3,norwegian, yellow, bird, wine, dunhill]],
	       [[4,danish, white, horse, wine, blend]
	       ]},
	      {
		[[4,danish, white, horse, wine, blend]],[]
	      }]),
    io:format("~p~n~n",[Output]),
    io:format("~p",[
		    sort_Scenario_PossibleExtensionList_tuples(
		      einstein:create_Scenario_PossibleExtensionList_tuples(Input))]),
    ?assertMatch(Output,
		 sort_Scenario_PossibleExtensionList_tuples(
		 einstein:create_Scenario_PossibleExtensionList_tuples(Input))
		).


create_Scenario_PossibleExtensionList_tuples_3_element_test()->
    Input =  [
	      [2,swedish, green, cat, beer, pall_mall],
              [3,norwegian, yellow, bird, wine, dunhill],
	      [4,danish, white, horse, wine, blend]
             ],
    Output =
	sort_Scenario_PossibleExtensionList_tuples(
	  [

	   {
	     [[2,swedish, green, cat, beer, pall_mall]],
	     [
	      [3,norwegian, yellow, bird, wine, dunhill],
	      [4,danish, white, horse, wine, blend]
	     ]},
	   {[[3,norwegian, yellow, bird, wine, dunhill]],
	    [[4,danish, white, horse, wine, blend]
	    ]},
	   {
	     [[4,danish, white, horse, wine, blend]],[]
	   }]),
    io:format("~p~n~n",[Output]),
    io:format("~p",[
		    sort_Scenario_PossibleExtensionList_tuples(
		      einstein:create_Scenario_PossibleExtensionList_tuples(Input))]),
    ?assertMatch(Output,
		 sort_Scenario_PossibleExtensionList_tuples(
		 einstein:create_Scenario_PossibleExtensionList_tuples(Input))
		).

create_Scenario_PossibleExtensionList_tuples_full_test()->
    Input =  [
	      [1,swedish, green, cat, wine, pall_mall],
              [2,swedish, red, cat, water, pall_mall],
	      [2,swedish, green, cat, beer, pall_mall],
              [3,norwegian, yellow, bird, wine, dunhill],
	      [4,danish, white, horse, wine, blend]
             ],
    Output =
	sort_Scenario_PossibleExtensionList_tuples(
	  [
	   {
	     [[1,swedish, green, cat, wine, pall_mall]],
	     [
	      [2,swedish, red, cat, water, pall_mall],
	      [2,swedish, green, cat, beer, pall_mall],
	      [3,norwegian, yellow, bird, wine, dunhill],
	      [4,danish, white, horse, wine, blend]
	     ]
	   },
	   {
	     [[2,swedish, green, cat, beer, pall_mall]],
	     [
	      [2,swedish, red, cat, water, pall_mall],
	      [3,norwegian, yellow, bird, wine, dunhill],
	      [4,danish, white, horse, wine, blend]
	   ]
	   },
	  {
	    [[2,swedish, red, cat, water, pall_mall]],
	    [
	     [3,norwegian, yellow, bird, wine, dunhill],
	     [4,danish, white, horse, wine, blend]
	    ]
	  },
	  {
	    [[3,norwegian, yellow, bird, wine, dunhill]],
	    [[4,danish, white, horse, wine, blend]]
	  },
	  {
	    [[4,danish, white, horse, wine, blend]],
	    []
	  }
	 ]),
    io:format("~p~n~n",[Output]),
    io:format("~p",[
		    sort_Scenario_PossibleExtensionList_tuples(
		      einstein:create_Scenario_PossibleExtensionList_tuples(Input))]),
    ?assertMatch(Output,
		 sort_Scenario_PossibleExtensionList_tuples(
		 einstein:create_Scenario_PossibleExtensionList_tuples(Input))
		).


step_down_Scenario_PossibleExtensionList_tuples_null_test()->
    Input = {[[]],[]},
    
    InputFun = [fun einstein_general_rules:does_house_descritpion_intersects_with_scenario/2,                   
                einstein_rules:blend_smoker_house_next_to_cat_owner()
               ],
    Output = [],
       
    ?assertMatch(Output, lists:sort(
                           einstein:break_down_Scenario_PossibleExtensionList_tuple(InputFun,Input))).



disjoint_break_down_Scenario_PossibleExtensionList_tuple_step_test()->
    Input1 = [[1,british, red, dog, beer, blend]],
    Input2 = [[2,swedish, blue, cat, water, pall_mall]],
    Input = {Input1,Input2},
    
    InputFun = [fun einstein_general_rules:does_house_descritpion_intersects_with_scenario/2,                   
                einstein_rules:blend_smoker_house_next_to_cat_owner()
               ],
    Output = [{[
	       [1,british, red, dog, beer, blend],
	       [2,swedish, blue, cat, water,pall_mall]],[]}],
	?assertMatch(Output,  sort_Scenario_PossibleExtensionList_tuples(
				einstein:break_down_Scenario_PossibleExtensionList_tuple(
				  InputFun,Input))).


many_disjoint_break_down_Scenario_PossibleExtensionList_tuple_step_test()->
    Input1 = [[1,british, red, dog, beer, blend]],
    Input2 = [[2,swedish, blue, cat, water, pall_mall],
              [2,swedish, red, cat, water, pall_mall],
              [2,swedish, green, cat, beer, pall_mall],
              [1,swedish, green, cat, wine, pall_mall]
             ],
    InputFun = [fun einstein_general_rules:does_house_descritpion_intersects_with_scenario/2                   
               ],
    Input = {Input1,Input2},
    Output = [{[
	       [1,british, red, dog, beer, blend],
	       [2,swedish, blue, cat, water, pall_mall]],[]}],

    ?assertMatch(Output, sort_Scenario_PossibleExtensionList_tuples(
			    einstein:break_down_Scenario_PossibleExtensionList_tuple(
			      InputFun,Input))).

many_disjoint_break_down_Scenario_PossibleExtensionList_tuple_step_2_test()->
    Input1 = [[1,british, red, dog, beer, blend],
              [2,swedish, blue, cat, water, pall_mall]],
    Input2 = [
              [2,swedish, red, cat, water, pall_mall],
              [3,danish, white, horse, wine, bluemaster],
              [2,swedish, green, cat, beer, pall_mall],
              [4,norwegian, yellow, bird, wine, dunhill],
              [1,swedish, green, cat, wine, pall_mall]
             ],
    Input = {Input1,Input2},
    InputFun = [fun einstein_general_rules:does_house_descritpion_intersects_with_scenario/2],
    Output = [
	      {
		[
		 [1,british, red, dog, beer, blend],
		 [2,swedish, blue, cat, water, pall_mall],
		 [3,danish, white, horse, wine, bluemaster]
		],
		[[4,norwegian, yellow, bird, wine, dunhill]]
	      },
	      {
		[
		 [1,british, red, dog, beer, blend],
		 [2,swedish, blue, cat, water, pall_mall],
		 [4,norwegian, yellow, bird, wine, dunhill]
		
		],
		[[3,danish, white, horse, wine, bluemaster]]
	      }],
         ?assertMatch(Output, sort_Scenario_PossibleExtensionList_tuples(
			    einstein:break_down_Scenario_PossibleExtensionList_tuple(
			      InputFun,Input))).

many_funs_disjoint_break_down_Scenario_PossibleExtensionList_tuple_step_test()->
    Input1 = [[1,british, red, dog, beer, blend],
              [2,swedish, blue, cat, water, pall_mall]],
    Input2 = [
              [2,swedish, red, cat, water, pall_mall],
              [4,danish, white, horse, wine, blend],
              [2,swedish, green, cat, beer, pall_mall],
              [4,norwegian, yellow, bird, wine, dunhill],
              [1,swedish, green, cat, wine, pall_mall]
             ],
    Input = {Input1,Input2},
    InputFun = [fun einstein_general_rules:does_house_descritpion_intersects_with_scenario/2,                   
                einstein_rules:blend_smoker_house_next_to_cat_owner()
               ],
    Output = [{
		[[1,british, red, dog, beer, blend],
		 [2,swedish, blue, cat, water, pall_mall],
		 [4,norwegian, yellow, bird, wine, dunhill]
		],[]
	      }],
       
    ?assertMatch(Output, sort_Scenario_PossibleExtensionList_tuples(
			   einstein:break_down_Scenario_PossibleExtensionList_tuple(
			     InputFun,Input))).
 
many_funs_2_disjoint_break_down_Scenario_PossibleExtensionList_tuple_step_test()->
    Input1 = [[1,british, red, dog, beer, bluemaster],
              [2,swedish, blue, cat, water, pall_mall]],
    Input2 = [
              [2,swedish, red, cat, water, pall_mall],
              [3,danish, white, horse, wine, blend],
              [2,swedish, green, cat, beer, pall_mall],
              [4,norwegian, yellow, bird, wine, dunhill],
              [1,swedish, green, cat, wine, pall_mall]
             ],
    Input = {Input1,Input2},
    InputFun = [fun einstein_general_rules:does_house_descritpion_intersects_with_scenario/2,                   
                einstein_rules:blend_smoker_house_next_to_cat_owner()
               ],


    Output = [
	      {
		[[1,british, red, dog, beer, bluemaster],
		 [2,swedish, blue, cat, water, pall_mall],
		 [3,danish, white, horse, wine, blend]],
		[[4,norwegian, yellow, bird, wine, dunhill]]
	      },
	      {
		[[1,british, red, dog, beer, bluemaster],
		 [2,swedish, blue, cat, water, pall_mall],
		 [4,norwegian, yellow, bird, wine, dunhill]],
		[[3,danish, white, horse, wine, blend]]
	      }],
    
    ?assertMatch(Output, sort_Scenario_PossibleExtensionList_tuples(
			   einstein:break_down_Scenario_PossibleExtensionList_tuple(
			     InputFun,Input))).

many_funs_3_disjoint_break_down_Scenario_PossibleExtensionList_tuple_step_test()->
    Input1 = [[1,british, red, dog, beer, bluemaster],
              [2,swedish, blue, cat, water, pall_mall]],
    Input2 = [
              [2,swedish, red, cat, water, pall_mall],
              [4,danish, white, horse, wine, blend],
              [2,swedish, green, cat, beer, pall_mall],
              [3,norwegian, yellow, bird, wine, blend],
              [1,swedish, green, cat, wine, pall_mall]
             ], 
    Input = {Input1,Input2},
    InputFun = [fun einstein_general_rules:does_house_descritpion_intersects_with_scenario/2,                   
                einstein_rules:blend_smoker_house_next_to_cat_owner()
               ],

    Output = [
	      {[[1,british, red, dog, beer, bluemaster],
		[2,swedish, blue, cat, water, pall_mall],
		[3,norwegian, yellow, bird, wine, blend]],
	       []
	      }],

    ?assertMatch(Output, sort_Scenario_PossibleExtensionList_tuples(
			   einstein:break_down_Scenario_PossibleExtensionList_tuple(
			     InputFun,Input))).



generate_possible_solutions_one_element_test()->  
    Input = [[1,british, red, dog, beer, blend]],
    SolutionFilterFuns = [],
    ConstructionReductionFuns = [fun einstein_general_rules:does_house_descritpion_intersects_with_scenario/2,                   
                einstein_rules:blend_smoker_house_next_to_cat_owner()
               ],
    Output = [[[1,british, red, dog, beer, blend]]],
       
    ?assertMatch(Output, sort_sort(
                           einstein:generate_valid_scenarios(
			     SolutionFilterFuns, ConstructionReductionFuns ,Input))).

generate_possible_solutions_two_element_test()->  
    Input = [[1,british, red, dog, beer, blend],
	     [2,swedish, blue, cat, water, pall_mall]],
    SolutionFilterFuns = [],
    ConstructionReductionFuns = [
				 fun einstein_general_rules:does_house_descritpion_intersects_with_scenario/2,
				 einstein_rules:blend_smoker_house_next_to_cat_owner()
				],
    Output = 
	sort_sort(
	  [
	   [[1,british, red, dog, beer, blend]],
	   [[2,swedish, blue, cat, water, pall_mall]],
	   [
	    [1,british, red, dog, beer, blend],
	    [2,swedish, blue, cat, water, pall_mall]
	    
	   ]
	  ]),
       
    ?assertMatch(Output, sort_sort(
                           einstein:generate_valid_scenarios(
			     SolutionFilterFuns, ConstructionReductionFuns ,Input))).



generate_possible_solutions_no_filter_full_test()->  

    SolutionFilterFuns = [],
    ConstructionReductionFuns = [
				 fun einstein_general_rules:does_house_descritpion_intersects_with_scenario/2,
				 einstein_rules:blend_smoker_house_next_to_cat_owner()
               ],
    Input = [
              [2,swedish, red, cat, water, pall_mall],
              [3,danish, white, horse, wine, blend],
              [2,swedish, green, cat, beer, pall_mall],
              [4,norwegian, yellow, bird, wine, dunhill],
              [1,swedish, green, cat, wine, pall_mall]
             ],
    Output = 
	sort_sort(
	  [
	   [[2,swedish, red, cat, water, pall_mall]],
	   [[3,danish, white, horse, wine, blend]],
	   [[2,swedish, green, cat, beer, pall_mall]],
	   [[4,norwegian, yellow, bird, wine, dunhill]],
	   [[1,swedish, green, cat, wine, pall_mall]],
	   [
	    [2, swedish, red, cat, water, pall_mall],
	    [3,danish, white, horse, wine, blend]
	   ],
	   [
	    [2, swedish, red, cat, water, pall_mall],
	    [4,norwegian, yellow, bird, wine, dunhill]
	   ],
	   [
	    [2,swedish, green, cat, beer, pall_mall],
	    [3,danish, white, horse, wine, blend]
	   ],
	   [
	    [2,swedish, green, cat, beer, pall_mall],
	    [4,norwegian, yellow, bird, wine, dunhill]
	   ]
	  ]),      
    ?assertMatch(Output, sort_sort(
                           einstein:generate_valid_scenarios(
			     SolutionFilterFuns, ConstructionReductionFuns ,Input))).


generate_possible_solutions_2_element_filter_full_test()->  

    SolutionFilterFuns = [fun einstein_filters:two_values/1],
    ConstructionReductionFuns = [fun einstein_general_rules:does_house_descritpion_intersects_with_scenario/2,                   
                einstein_rules:blend_smoker_house_next_to_cat_owner()
               ],
    Input = [
              [2,swedish, red, cat, water, pall_mall],
              [3,danish, white, horse, wine, blend],
              [2,swedish, green, cat, beer, pall_mall],
              [4,norwegian, yellow, bird, wine, dunhill],
              [1,swedish, green, cat, wine, pall_mall]
             ],
    Output = 
	sort_sort(
	  [
	   [
	    [2, swedish, red, cat, water, pall_mall],
	    [3,danish, white, horse, wine, blend]
	   ],
	   [
	    [2, swedish, red, cat, water, pall_mall],
	    [4,norwegian, yellow, bird, wine, dunhill]
	   ],
	   [
	    [2,swedish, green, cat, beer, pall_mall],
	    [3,danish, white, horse, wine, blend]
	   ],
	   [
	    [2,swedish, green, cat, beer, pall_mall],
	    [4,norwegian, yellow, bird, wine, dunhill]
	   ]
	  ]),      
    ?assertMatch(Output, sort_sort(
                           einstein:generate_valid_scenarios(
			     SolutionFilterFuns, ConstructionReductionFuns ,Input))).

generate_possible_solutions_max_element_filter_full_test()->  
    SolutionFilterFuns = [fun einstein_filters:max_values/1],
    ConstructionReductionFuns = [fun einstein_general_rules:does_house_descritpion_intersects_with_scenario/2,                   
                einstein_rules:blend_smoker_house_next_to_cat_owner()
               ],
    Input = [
              [2,swedish, red, cat, water, pall_mall],
              [3,danish, white, horse, wine, blend],
              [2,swedish, green, cat, beer, pall_mall],
              [4,norwegian, yellow, bird, wine, dunhill],
              [1,swedish, green, cat, wine, pall_mall]
             ],
    Output = 
	sort_sort(
	  [
	   [
	    [2, swedish, red, cat, water, pall_mall],
	    [3,danish, white, horse, wine, blend]
	   ],
	   [
	    [2, swedish, red, cat, water, pall_mall],
	    [4,norwegian, yellow, bird, wine, dunhill]
	   ],
	   [
	    [2,swedish, green, cat, beer, pall_mall],
	    [3,danish, white, horse, wine, blend]
	   ],
	   [
	    [2,swedish, green, cat, beer, pall_mall],
	    [4,norwegian, yellow, bird, wine, dunhill]
	   ]
	  ]),      
    ?assertMatch(Output, sort_sort(
                           einstein:generate_valid_scenarios(
			     SolutionFilterFuns, ConstructionReductionFuns ,Input))).

generate_possible_solutions_max_element_2_filter_full_test()->  
    SolutionFilterFuns = [fun einstein_filters:max_values/1],
    ConstructionReductionFuns = [fun einstein_general_rules:does_house_descritpion_intersects_with_scenario/2,                   
                einstein_rules:blend_smoker_house_next_to_cat_owner()
               ],
    Input = [
              [2,swedish, red, cat, water, pall_mall],
              [3,danish, white, horse, wine, blend],
              [2,swedish, green, cat, beer, pall_mall],
              [4,norwegian, yellow, bird, beer, dunhill],
              [1,swedish, green, cat, wine, pall_mall]
             ],
    Output = 
	sort_sort(
	  [
	   [
	    [2, swedish, red, cat, water, pall_mall],
	    [3,danish, white, horse, wine, blend],
	    [4,norwegian, yellow, bird, beer, dunhill]
	   ]
	  ]),      
    ?assertMatch(Output, sort_sort(
                           einstein:generate_valid_scenarios(
			     SolutionFilterFuns, ConstructionReductionFuns, Input))).

generate_possible_solutions_max_element_3_filter_full_test()->  
    SolutionFilterFuns = [fun einstein_filters:max_values/1],
    ConstructionReductionFuns = [
				 fun einstein_general_rules:does_house_descritpion_intersects_with_scenario/2,
				 einstein_rules:blend_smoker_house_next_to_cat_owner()
				],
    Input = [
	     [2,swedish, red, cat, water, pall_mall],
	     [1,german, green, dog, coffee, blue_master],
	     [3,danish, white, horse, wine, blend],
	     [2,swedish, green, cat, beer, pall_mall],
	     [1,german, green, dog, tea, blue_master],
	     [4,norwegian, yellow, bird, beer, dunhill],
	     [1,swedish, green, cat, wine, pall_mall]
             ],
    Output = 
	sort_sort(
	  [
	   [
	    [1,german, green, dog, coffee, blue_master],
	    [2, swedish, red, cat, water, pall_mall],
	    [3,danish, white, horse, wine, blend],
	    [4,norwegian, yellow, bird, beer, dunhill]
	   ],
	   [
	    [1,german, green, dog, tea, blue_master],
	    [2, swedish, red, cat, water, pall_mall],
	    [3,danish, white, horse, wine, blend],
	    [4,norwegian, yellow, bird, beer, dunhill]
	   ]
	  ]),      
    ?assertMatch(Output, sort_sort(
                           einstein:generate_valid_scenarios(
			     SolutionFilterFuns, ConstructionReductionFuns, Input))).



solution_restrictions_test()->
    Input1 =  [
	       [5,swedish,white,dog,beer,bluemaster],
	       [4,german,green,fish,coffe,prince],
	       [3,british,red,bird,milk,pall_mall],	
	       [2,danish,blue,horse,tea,blend]],
    Input2 = [1,norwegian,yellow,cat,water,dunhill], 

    Output = false,
    Rule = einstein_rules:blend_smoker_house_next_to_cat_owner(),
    ?assertMatch(Output, Rule(Input1,Input2)).

solution_restrictions_2_test()->
    Input1 =  [
	       [5,swedish,white,dog,beer,bluemaster],
	       [4,german,green,fish,coffe,prince],
	       [3,british,red,bird,milk,pall_mall],	
	       [2,danish,blue,horse,tea,blend]],
    Input2 = [1,norwegian,yellow,cat,water,dunhill], 

    Output = false,
    Rule = einstein_rules:dunhill_smoker_house_next_to_horse_owner(),
    ?assertMatch(Output, Rule(Input1,Input2)).


solution_restrictions_3_test()->
    Input1 =  [
	       [5,swedish,white,dog,beer,bluemaster],
	       [4,german,green,fish,coffe,prince],
	       [3,british,red,bird,milk,pall_mall],	
	       [2,danish,blue,horse,tea,blend]],
    Input2 = [1,norwegian,yellow,cat,water,dunhill], 

    Output = false,
    Rule = einstein_rules:norwegian_house_next_to_blue(),
    ?assertMatch(Output, Rule(Input1,Input2)).

solution_restrictions_4_test()->
    Input1 =  [
	       [5,swedish,white,dog,beer,bluemaster],
	       [4,german,green,fish,coffe,prince],
	       [3,british,red,bird,milk,pall_mall],	
	       [2,danish,blue,horse,tea,blend]],
    Input2 = [1,norwegian,yellow,cat,water,dunhill], 

    Output = false,
    Rule = einstein_rules:blend_smoker_house_next_to_water_drinker(),
    ?assertMatch(Output, Rule(Input1,Input2)).

solution_restrictions_6_test()->
    Input1 =  [
	       [5,swedish,white,dog,beer,bluemaster],
	       [4,german,green,fish,coffe,prince],
	       [3,british,red,bird,milk,pall_mall],	
	       [2,danish,blue,horse,tea,blend]],
    Input2 = [1,norwegian,yellow,cat,water,dunhill], 

    Output = false,
    Rule = einstein_rules:white_house_next_to_green(),
    ?assertMatch(Output, Rule(Input1,Input2)).

%%Help functions
sort_Scenario_PossibleExtensionList_tuples(TheTuples)->
    lists:sort([{lists:sort(Scenario),
		lists:sort(PossibleExtensions)}
	       || {Scenario,PossibleExtensions}<-TheTuples]).

sort_sort(ListsOfList)->
    lists:sort([lists:sort(List)||List <-ListsOfList]).
