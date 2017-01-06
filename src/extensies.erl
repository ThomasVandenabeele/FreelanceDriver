%%%-------------------------------------------------------------------
%%% @author Thomas
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. Jan 2017 10:05
%%%-------------------------------------------------------------------
-module(extensies).
-author("Thomas").

%% API
-export([
  get_duur/2,
  get_limietdagen_van_huidige_week/0,
  get_limietdagen_van_gegeven_week/1
]).

get_duur(Start, Eind) ->
  calendar:datetime_to_gregorian_seconds(Eind) - calendar:datetime_to_gregorian_seconds(Start).

get_limietdagen_van_huidige_week() ->
  get_limietdagen_van_gegeven_week(calendar:local_time()).

get_limietdagen_van_gegeven_week({HDate, _}) ->
  Weekdag = calendar:day_of_the_week(HDate),
  HSeconds = calendar:datetime_to_gregorian_seconds({HDate, {12,0,0}}),
  {StartDatum, _ } = calendar:gregorian_seconds_to_datetime(HSeconds-((Weekdag-1)*86400)),
  {EindDatum, _ } = calendar:gregorian_seconds_to_datetime(HSeconds+((7-Weekdag)*86400)),
  {{StartDatum, { 0, 0, 0}}, {EindDatum, { 23, 59, 59}}}.