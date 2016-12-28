%%%-------------------------------------------------------------------
%%% @author FLMath <lematteu@gmail.com>
%%% @copyright (C) 2016, FLMath
%%% @doc
%%%
%%% @end
%%% Created : 21 Dec 2016 by FLMath
%%%-------------------------------------------------------------------
-module(einstein_filters).

-export([two_values/1,max_values/1]).

two_values(ListOfSolutions) ->
    lists:filter(
      fun(Solution) ->length(Solution) == 2 end, 
      ListOfSolutions).

four_values(ListOfSolutions) ->
    lists:filter(
      fun(Solution) ->length(Solution) == 4 end, 
      ListOfSolutions).

five_values(ListOfSolutions) ->
    lists:filter(
      fun(Solution) ->length(Solution) == 5 end, 
      ListOfSolutions).

max_values(ListOfSolutions) ->
    MaxLength = lists:max([length(Solution) || Solution <- ListOfSolutions]),
    lists:filter(
      fun(Solution) ->length(Solution) == MaxLength end, 
      ListOfSolutions).
