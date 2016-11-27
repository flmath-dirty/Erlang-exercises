-module(listReverse_test).
 
-include_lib("eunit/include/eunit.hrl").
-compile([export_all]).

empty_flatten_and_reverse_test()->
    Input =[],
    Output = [],
    ?assertMatch(Output, listReverse:flatten_and_reverse(Input,[],[])).

empty_flatten_and_reverse_2_test()->
    Input =[1],
    Output = [1],
    ?assertMatch(Output, listReverse:flatten_and_reverse(Input,[],[])).

empty_flatten_and_reverse_3_test()->
    Input =[4,[3,2],1],
    Output = [1,2,3,4],
    ?assertMatch(Output, listReverse:flatten_and_reverse(Input,[],[])).

empty_flatten_and_reverse_4_test()->
    Input =[1, 2, [3, 4, [5, 6, 7], 8], 9],
    Output = [9,8,7,6,5,4,3,2,1],
    ?assertMatch(Output, listReverse:flatten_and_reverse(Input,[],[])).



empty_list_test()->
    Input =[],
    Output = [],
    ?assertMatch(Output, listReverse:do(Input)).    

one_element_list_test()->
    Input =[3],
    Output = [3],
    ?assertMatch(Output, listReverse:do(Input)). 

two_element_list_test() ->
    Input =[3,1],
    Output = [1,3],
    ?assertMatch(Output, listReverse:do(Input)). 

nested_list_test() ->
    Input =[4,[3,2],1],
    Output = [1,[2,3],4],
    ?assertMatch(Output, listReverse:do(Input)). 

list_from_mail_test() ->
    Input  = [1, 2, [3, 4, [5, 6, 7], 8], 9],
    Output = [9, 8, [7, 6, [5, 4, 3], 2], 1],
    ?assertMatch(Output, listReverse:do(Input)).

list_expanded_test() ->
    Input  = [1, 2,   [3,  4, [5, 6, 7], 8, [9, 10,11]], 12],
    Output = [12, 11, [10, 9, [8, 7, 6], 5, [4, 3, 2 ]], 1],
    ?assertMatch(Output, listReverse:do(Input)).
