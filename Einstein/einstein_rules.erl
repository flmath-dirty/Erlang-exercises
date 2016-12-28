%%%-------------------------------------------------------------------
%%% @author FLMath <lematteu@gmail.com>
%%% @copyright (C) 2016, FLMath
%%% @doc
%%%
%%% @end
%%% Created : 21 Dec 2016 by FLMath
%%%-------------------------------------------------------------------
-module(einstein_rules).
-compile([export_all]).

space_restriction_rules()->
    [
     british_in_red_house(),
     swedish_has_dog(),
     danish_drinks_tea(),
     green_homee_drinks_coffee(),
     pall_mall_smoker_has_bird(),
     yellow_homee_smokes_dunhill(),
     middle_homee_drinks_milk(),
     first_homee_is_norwegian(),
     bluemaster_smoker_drinks_beer(),
     prince_smoker_is_german()
    ].

generating_restriction_rules()->
    [
     blend_smoker_house_next_to_cat_owner(),
     dunhill_smoker_house_next_to_horse_owner(),
     norwegian_house_next_to_blue(),
     blend_smoker_house_next_to_water_drinker(),
     white_house_next_to_green()
    ].

%% 1. The British man lives in the red house.
british_in_red_house() ->
    einstein_general_rules:inhouse_rules_factory({2, british}, {3,red}).
%% 2. The Swedish man has a dog for a pet.
swedish_has_dog() ->
    einstein_general_rules:inhouse_rules_factory({2, swedish}, {4,dog}).    
%% 3. The Danish man drinks tea.
danish_drinks_tea() ->
    einstein_general_rules:inhouse_rules_factory({2, danish}, {5,tea}).
%% 5.The owner of the green house drinks coffee.
green_homee_drinks_coffee() ->
    einstein_general_rules:inhouse_rules_factory({3, green}, {5,coffee}).
%% 6. The person that smokes Pall Mall has a bird.
pall_mall_smoker_has_bird() ->
    einstein_general_rules:inhouse_rules_factory({4, bird}, {6,pall_mall}).
%% 7. The owner of the yellow house smokes Dunhill.
yellow_homee_smokes_dunhill() ->
    einstein_general_rules:inhouse_rules_factory({3, yellow}, {6,dunhill}).
%% 8. The person that lives in the middle house drinks milk.
middle_homee_drinks_milk() ->
    einstein_general_rules:inhouse_rules_factory({1, 3}, {5,milk}).
%% 9. The Norwegian lives in the first house.
first_homee_is_norwegian() ->
    einstein_general_rules:inhouse_rules_factory({1, 1}, {2,norwegian}).
%% 12. The one that smokes Bluemaster drinks beer.
bluemaster_smoker_drinks_beer() ->
    einstein_general_rules:inhouse_rules_factory({5, beer}, {6,bluemaster}).
%% 13. The German smokes Prince.
prince_smoker_is_german() ->
    einstein_general_rules:inhouse_rules_factory({6,prince}, {2,german}).

%% 10. The person that smokes Blend, lives next to the one that has a cat.
blend_smoker_house_next_to_cat_owner() ->
    einstein_general_rules:neighbour_test_factory([{6,blend},{4,cat}]).
%% 11. The person that has a horse lives next to the one that smokes Dunhill.
dunhill_smoker_house_next_to_horse_owner() ->
    einstein_general_rules:neighbour_test_factory([{6,dunhill},{4,horse}]).
%% 14. The Norwegian lives next to a blue house.
norwegian_house_next_to_blue() ->
    einstein_general_rules:neighbour_test_factory([{2,norwegian},{3,blue}]).
%% 15. The person that smokes Blend, has a neighbour that drinks water.
blend_smoker_house_next_to_water_drinker() ->
    einstein_general_rules:neighbour_test_factory([{6,blend},{5,water}]).
%% 4. The green house is to the left of the white house.
white_house_next_to_green() ->
    einstein_general_rules:left_neighbour_test_factory({3,green},{3,white}).
