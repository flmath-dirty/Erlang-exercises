-module(listReverse). 

-export([do/1]).

do(List) -> {Result, _} = inject_values_into_list_structure(List,flatten_and_reverse(List,[],[])), Result.

flatten_and_reverse([Element|Tail]=_List,ValuesAccumulator,ListsAccumulator)
  when is_list(Element) ->
    flatten_and_reverse(Element,ValuesAccumulator,[Tail|ListsAccumulator]);
flatten_and_reverse([Value|Tail],ValuesAccumulator,ListsAccumulator) ->
    flatten_and_reverse(Tail, [Value|ValuesAccumulator],ListsAccumulator);

flatten_and_reverse([],ValuesAccumulator,[StoredListTail|ListsAccumulator]) ->
    flatten_and_reverse(StoredListTail,ValuesAccumulator,ListsAccumulator);
flatten_and_reverse([],ValuesAccumulator,[]) ->
    ValuesAccumulator.

inject_values_into_list_structure([Element|Tail]=_Structure,ValuesAccumulator)
  when is_list(Element) ->
    {NewHead,ValuesOutsideElement} = inject_values_into_list_structure(Element,ValuesAccumulator),
    {NewTail,ValuesOuter} = inject_values_into_list_structure(Tail,ValuesOutsideElement),
    {[NewHead | NewTail],ValuesOuter};
inject_values_into_list_structure([_ReplacedValue|Tail]=_Structure,[Value|ValuesAccumulator]) ->
    {NewTail, ValuesAccumulatorAfterPop} = inject_values_into_list_structure(Tail,ValuesAccumulator),
    {[Value | NewTail], ValuesAccumulatorAfterPop};
inject_values_into_list_structure([],ValuesAccumulator) ->
    {[],ValuesAccumulator}.








