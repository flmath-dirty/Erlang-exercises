%%%-------------------------------------------------------------------
%%% @author FLMath <lematteu@gmail.com>
%%% @copyright (C) 2016, FLMath
%%% @doc
%%%
%%% @end
%%% Created : 21 Dec 2016 by FLMath
%%%-------------------------------------------------------------------

-module(einstein).

-export([space/1, generate_valid_scenarios/3, restrict/2]).

generate_valid_scenarios(
  SolutionFilterFuns, ScenarioReductionFuns, 
  ListOfPossibleElements)->
    
    BaseScenarios = create_Scenario_PossibleExtensionList_tuples(ListOfPossibleElements),
    generate_valid_scenarios(
      SolutionFilterFuns, ScenarioReductionFuns, BaseScenarios,[]).

generate_valid_scenarios(
  SolutionFilterFuns, ScenarioReductionFuns, 
  [ProcessedScenario | ScenariosAccumulator], Solutions)->
    
    NewScenarios = break_down_Scenario_PossibleExtensionList_tuple(
		     ScenarioReductionFuns,
		     ProcessedScenario),
    {Leaves, NonLeafNodes} = lists:partition(fun is_leaf_fun/1, NewScenarios),
    
    AdditionalSolutions = 
	[AdditionalSolution || {AdditionalSolution, _} <- [ProcessedScenario | Leaves]],
    
    generate_valid_scenarios(
      SolutionFilterFuns, ScenarioReductionFuns, 
      lists:append(NonLeafNodes, ScenariosAccumulator), 
      lists:append(AdditionalSolutions,Solutions));
generate_valid_scenarios(
  SolutionFilterFuns, _ScenarioReductionFuns, 
  [], Solutions) ->
    apply_solution_filters(SolutionFilterFuns, Solutions).

apply_solution_filters([SolutionFilterFun|SolutionFilterFuns], Solutions)->
    FilteredSolutions = SolutionFilterFun(Solutions),
	apply_solution_filters(SolutionFilterFuns, FilteredSolutions);
apply_solution_filters([], Solutions) ->
    Solutions.

is_leaf_fun({_Scenario,PossibleExtensionList}) ->
    PossibleExtensionList==[].
       
break_down_Scenario_PossibleExtensionList_tuple(RestrictionsFunList,
							 {Scenario,PossibleExtensionList})->
    CompatibileExtensions = 
	get_compatibile_scenarios(RestrictionsFunList, Scenario, PossibleExtensionList),
    HighestElementInTheScenario = lists:max(Scenario),
    [move_Extension_to_Scenario_field(Extension,{Scenario,CompatibileExtensions})
      || Extension <- CompatibileExtensions, Extension > HighestElementInTheScenario].

move_Extension_to_Scenario_field(Extension,{Scenario,Extensions})->
    {[Extension|Scenario],lists:delete(Extension,Extensions)}.

create_Scenario_PossibleExtensionList_tuples(ListOfPossibleExtensions)->
    SortedListOfPossibleExtensions = lists:sort(ListOfPossibleExtensions),
    create_Scenario_PossibleExtensionList_tuples(SortedListOfPossibleExtensions,[]).

create_Scenario_PossibleExtensionList_tuples([TopElement|RestOfScenarios]=_ListOfPossibleElements,
					     TheTuplesAccumulator)->
    create_Scenario_PossibleExtensionList_tuples(RestOfScenarios,
						 [{[TopElement],RestOfScenarios}
						  |TheTuplesAccumulator]);
create_Scenario_PossibleExtensionList_tuples([],TheTuplesAccumulator) ->
    TheTuplesAccumulator.

get_compatibile_scenarios(RestrictionsFunList, Scenario,AdditionalHouseCandidates)->
    get_compatibile_scenarios(RestrictionsFunList, Scenario,
                              AdditionalHouseCandidates,[]).

get_compatibile_scenarios(RestrictionsFunList, Scenario,
                          [AdditionalHouseCandidate|AdditionalHouseCandidates],
                          PossibleExtensionsAccumulator)->
    PossibleExtension =
        apply_restriction_funs(RestrictionsFunList, Scenario, AdditionalHouseCandidate),
    UpdatedPossibleExtensionsAccumulator =
        append_description(PossibleExtension, PossibleExtensionsAccumulator),
   
    get_compatibile_scenarios(RestrictionsFunList, Scenario, AdditionalHouseCandidates,
                              UpdatedPossibleExtensionsAccumulator);
get_compatibile_scenarios(_RestrictionsFunList, _Scenario,
                          [],PossibleExtensionsAccumulator) ->
    PossibleExtensionsAccumulator.

append_description(PossibleExtension, PossibleExtensionsAccumulator)
  when PossibleExtension==[]->
    PossibleExtensionsAccumulator;
append_description(PossibleExtension, PossibleExtensionsAccumulator)->
    [PossibleExtension|PossibleExtensionsAccumulator].

apply_restriction_funs(RestrictionsFunList,
                       Scenario,
                       AdditionalHouseCandidate)->
    IsRestricted = check_restrictions_with_funs(
		     RestrictionsFunList,
		     Scenario,AdditionalHouseCandidate),
    case IsRestricted of
	false  -> AdditionalHouseCandidate;
	true -> []
    end.

check_restrictions_with_funs(RestrictionsFunList, Scenario, AdditionalHouseCandidate)->
    ApplyRestrictionFun = apply_fun_to_description_factory(Scenario,AdditionalHouseCandidate),
    lists:any(ApplyRestrictionFun, RestrictionsFunList).

apply_fun_to_description_factory(Scenario,AdditionalHouseCandidate)->
    fun(RestrictionFun)->
	    RestrictionFun(Scenario,AdditionalHouseCandidate) 
    end.

space([]) ->[];
space(FamilyOfAttributesLists) ->
    FamilyOfAttributesSets = [sofs:set(AttributesLists) || AttributesLists <- FamilyOfAttributesLists ],
    sofs:product(list_to_tuple(FamilyOfAttributesSets)).

restrict([Rule|ListOfRules],Space)->
    restrict(ListOfRules,lists:filter(Rule,Space));
restrict([],Space) ->
    restrict([],Space, []).

restrict([],[Tuple|Space], SpaceOfTerms)->
    restrict([],Space, [tuple_to_list(Tuple)|SpaceOfTerms]);
restrict([],[],SpaceOfTerms) ->
    SpaceOfTerms.






