%%%-------------------------------------------------------------------
%%% @author FLMath <lematteu@gmail.com>
%%% @copyright (C) 2016, FLMath
%%% @doc
%%%
%%% @end
%%% Created : 21 Dec 2016 by FLMath
%%%-------------------------------------------------------------------
-module(einstein_general_rules).

-export([is_right_length_factory/1,
	 inhouse_rules_factory/2,
	 does_house_descritpion_intersects_with_scenario/2,
	 neighbour_test_factory/1,
	 left_neighbour_test_factory/2]).

is_right_length_factory(Length)->
    fun(LocalValue) -> Length==length(LocalValue) end.


inhouse_rules_factory({FirstFieldNo, FirstFieldValue},
		      {SecondFieldNo,SecondFieldValue})->
    fun(HouseTuple) -> 
	    inhouse_rule(HouseTuple,
			 {FirstFieldNo, FirstFieldValue},
			 {SecondFieldNo,SecondFieldValue}) 
    end. 

inhouse_rule(HouseTuple,
	     {FirstFieldNo, FirstFieldValue},
	     {SecondFieldNo,SecondFieldValue}) ->
    case {element(FirstFieldNo,HouseTuple),
	  element(SecondFieldNo,HouseTuple)} 
    of
        
	{FirstFieldValue,SecondFieldValue}->true;        
        {FirstFieldValue,_} -> false;
        {_,SecondFieldValue}-> false;
        _ -> true
    end.

does_house_descritpion_intersects_with_scenario(Scenario,HouseDescription)->
    _DoesAnyFieldInterect = lists:any(
			     fun(HouseFromScenario)-> 
				     does_intersect(HouseFromScenario,HouseDescription) 
			     end,
			     Scenario).

does_intersect([FirstListElement|FirstList],[SecondListElement|SecondList])->
    case FirstListElement == SecondListElement of
        false  -> does_intersect(FirstList,SecondList);
        true -> true
    end;
does_intersect([],[]) ->
    false.

neighbour_test_factory(ElementOrderNumberValueTuplesList)->
    fun(Scenario, HouseDescription) ->
	    distant_neighbour_rejection_test(
	      Scenario, HouseDescription, 
	      ElementOrderNumberValueTuplesList)
    end.

left_neighbour_test_factory(LeftOrderNumberValueTuple,RightOrderNumberValueTuple)->
    fun(Scenario, HouseDescription) ->
	    left_neighbour_rejection_test(Scenario, HouseDescription, 
					  LeftOrderNumberValueTuple,
					  RightOrderNumberValueTuple)
    end.

distant_neighbour_rejection_test(Scenario, HouseDescription, ElementOrderNumberValueTuplesList) ->
    [{FirstFieldOrderNo,FirstExpectedValue},
     {SecondFieldOrderNo,SecondExpectedValue}] = ElementOrderNumberValueTuplesList,
    FirstHouseNo = find_houseNo(Scenario, HouseDescription, {FirstFieldOrderNo,FirstExpectedValue}),
    SecondHouseNo = find_houseNo(Scenario, HouseDescription, {SecondFieldOrderNo,SecondExpectedValue}),
    AllHouseNums = get_house_nums(Scenario, HouseDescription),
    do_we_reject_neighbour_logic(FirstHouseNo,SecondHouseNo,AllHouseNums).

do_we_reject_neighbour_logic(false,false,_AllHouseNums)-> 
    false;
do_we_reject_neighbour_logic(false,SecondHouseNo,AllHouseNums) -> 
    are_neighbours_taken(SecondHouseNo,AllHouseNums);
do_we_reject_neighbour_logic(FirstHouseNo,false,AllHouseNums) -> 
    are_neighbours_taken(FirstHouseNo,AllHouseNums);
do_we_reject_neighbour_logic(FirstHouseNo,SecondHouseNo,_AllHouseNums) -> 
    abs(FirstHouseNo-SecondHouseNo)=/=1.

are_neighbours_taken(HouseNo,AllHouseNums)->
    LeftNeighbourFun  = is_taken_by_factory(HouseNo-1),
    RightNeighbourFun = is_taken_by_factory(HouseNo+1),
    IsLeftNeighbourTaken = lists:any(LeftNeighbourFun,AllHouseNums),
    IsRightNeighbourTaken = lists:any(RightNeighbourFun,AllHouseNums),
    IsLeftNeighbourTaken and IsRightNeighbourTaken.

left_neighbour_rejection_test(Scenario, HouseDescription, 
			      LeftOrderNumberValueTuple,
			      RightOrderNumberValueTuple) ->
    {LeftFieldOrderNo,LeftExpectedValue}   = LeftOrderNumberValueTuple,
    {RightFieldOrderNo,RightExpectedValue} = RightOrderNumberValueTuple,
    FirstHouseNo = find_houseNo(Scenario, HouseDescription, {LeftFieldOrderNo,LeftExpectedValue}),
    SecondHouseNo = find_houseNo(Scenario, HouseDescription, {RightFieldOrderNo,RightExpectedValue}),
    AllHouseNums = get_house_nums(Scenario, HouseDescription),
    do_we_reject_left_neighbour_logic(FirstHouseNo,SecondHouseNo,AllHouseNums).

do_we_reject_left_neighbour_logic(false,false,_AllHouseNums)-> 
    false;
do_we_reject_left_neighbour_logic(false,RightHouseNo,AllHouseNums) -> 
    is_left_neighbour_taken(RightHouseNo,AllHouseNums);
do_we_reject_left_neighbour_logic(LeftHouseNo,false,AllHouseNums) -> 
    is_right_neighbour_taken(LeftHouseNo,AllHouseNums);
do_we_reject_left_neighbour_logic(LeftHouseNo,RightHouseNo,_AllHouseNums) -> 
    RightHouseNo-LeftHouseNo =/= 1.

is_left_neighbour_taken(HouseNo,AllHouseNums)->
    LeftNeighbourFun  = is_taken_by_factory(HouseNo-1),
    _IsLeftNeighbourTaken = lists:any(LeftNeighbourFun,AllHouseNums).
is_right_neighbour_taken(HouseNo,AllHouseNums)->
    RightNeighbourFun  = is_taken_by_factory(HouseNo+1),
    _IsLeftNeighbourTaken = lists:any(RightNeighbourFun,AllHouseNums).    

is_taken_by_factory(HouseNo) ->
    fun(LocalHouseNo) ->
	    case is_out_of_scope(HouseNo) of
		true -> true;
		false -> HouseNo==LocalHouseNo
	    end
    end.

is_out_of_scope(HouseNo)->
    (HouseNo == 0) or (HouseNo == 6).

get_house_nums(Scenario, HouseDescription)->
    [hd(House)|| House <- [HouseDescription |Scenario]].
find_houseNo(Scenario, HouseDescription, {FieldOrderNo,ExpectedValue})->
    HouseNo =  [hd(House)|| House <- [HouseDescription |Scenario], 
		  ExpectedValue== lists:nth(FieldOrderNo,House)],
    case HouseNo of
	[Integer] -> Integer;
	[] -> false
    end.

