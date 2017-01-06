%%%-------------------------------------------------------------------
%%% @author Thomas Vandenabeele
%%% @copyright (C) 2017, Universiteit Hasselt & KU Leuven
%%% @doc
%%%
%%% Simulator module om de freelance drivers applicatie te testen
%%%           Start simulatie met simulator:start().
%%%
%%% @end
%%% Created : 05. Jan 2017 18:14
%%%-------------------------------------------------------------------
-module(simulator).
-author("Thomas").

%% API
-export([start/0, maak_freelancer/4, maak_job/4,
  maak_event/2, maak_event/3]).

-include("records.hrl").

start() ->
  io:fwrite("========================================================================================================~n", []),
  io:fwrite("                                       START SIMULATIE.~n", []),
  io:fwrite("========================================================================================================~n", []),
  freelancer:start(),
  run_simulatie().

run_simulatie() ->
  lists:foreach(fun(F) -> freelancer:insert(F) end,
    [
      #freelancer{id = F1_id} = maak_freelancer("Roelink", "Filip", 61,
        #{adres => {"Hauwaart 125", "9310 Meldert"}, tel => "0476 79 29 18", email => "FlipRoelink@teleworm.us"}),

      maak_freelancer("van Zanden", "Lyam", 32,
        #{adres => {"Herentalsebaan 346", "1080 Brussel"}, tel => "0481 21 14 02", email => "LyamvanZanden@jourrapide.com"}),

      #freelancer{id = F2_id} = maak_freelancer("Verhees", "Geoffrey", 25,
        #{adres => {"Stationsstraat 496", "5032 Isnes"}, tel => "0491 72 08 42", email => "GeoffreyVerhees@armyspy.com"}),

      maak_freelancer("Arets", "Freddy", 47,
        #{adres => {"Herentalsebaan 431", "1050 Brussel"}, tel => "0495 90 54 68", email => "FreddyArets@armyspy.com"})
    ]),

  lists:foreach(fun(J) -> freelancer:insert(J) end,
    [
      #job{id = J1_id} =  maak_job("Brengen/Halen: Zoon Maarten naar basketbalwedstrijd", {per_uur, 12}, "Mama", F1_id),
      #job{id = J2_id} = maak_job("Brengen: Gezin op Zaventem afzetten", {vast, 65}, "Papa", F2_id)
    ]),

  lists:foreach(fun(E) -> freelancer:registreer_event(E) end,
    [
      maak_event({{2017, 1, 5}, {20, 0, 0}}, {{2017, 1, 5}, {22, 0, 0}}),
      maak_event({{2017, 1, 6}, {10, 0, 0}}, {{2017, 1, 6}, {14, 0, 0}}, J1_id),
      #event{id = E1_id} = maak_event({{2017, 1, 6}, {5, 30, 0}}, {{2017, 1, 6}, {6, 30, 0}}, J2_id)
    ]),

  io:fwrite("Start data aangemaakt. Bekijken via observer:start().~n", []),
  io:fwrite("========================================================================================================~n", []),

  freelancer:wijs_event_af(F2_id, E1_id),

  io:fwrite("--------------------------------------------------------------------------------------------------------~n", []),

  {WU1, L1} = freelancer:get_werkuur_loon_opdracht(J1_id),
  io:fwrite("De freelancer voor opdracht ~p werkt ~p uren en verdient daarmee ~p euro.~n", [J1_id, WU1, L1]),
  {WU2, L2} = freelancer:get_werkuur_loon_opdracht(J2_id),
  io:fwrite("De freelancer voor opdracht ~p werkt ~p uren en verdient daarmee ~p euro.~n", [J2_id, WU2, L2]),

  io:fwrite("--------------------------------------------------------------------------------------------------------~n", []),

  Lijst = freelancer:get_werkuren_lonen_deze_week(F1_id),
  {TotaalUur, TotaalVergoeding} = freelancer:get_totaal_werkuur_vergoeding_deze_week(F1_id),
  io:fwrite("De freelancer ~s verkrijgt volgende opdrachtvergoedingen:~n~p.~n", [freelancer:format_naam_freelancer(F1_id), Lijst]),
  io:fwrite("De freelancer ~s verdient voor deze week in totaal dus ~p euro om voor ~p uren te werken.~n", [freelancer:format_naam_freelancer(F1_id), TotaalVergoeding, TotaalUur]),

  io:fwrite("--------------------------------------------------------------------------------------------------------~n", []),

  Datum = {2017, 1, 6},
  Lijst2 = freelancer:get_jobs_op_dag(F1_id, Datum),
  io:fwrite("De freelancer ~p heeft op ~p volgende opdrachten: ~n~p~n", [freelancer:format_naam_freelancer(F1_id), Datum, Lijst2]),

  io:fwrite("--------------------------------------------------------------------------------------------------------~n", []),

  Events = freelancer:get_events_op_dag(Datum),
  io:fwrite("Er zijn op ~p ~p agenda-items, namelijk: ~n~p~n", [Datum, length(Events), Events]),

  io:fwrite("--------------------------------------------------------------------------------------------------------~n", []),

  Events2 = freelancer:get_events_op_dag(F1_id, Datum),
  io:fwrite("De freelancer ~p heeft op ~p volgende agenda-items: ~n~p~n", [freelancer:format_naam_freelancer(F1_id), Datum, Events2]),

  io:fwrite("========================================================================================================~n", []),
  io:fwrite("                                       EINDE SIMULATIE.~n", []),
  io:fwrite("========================================================================================================~n", []),

  {ok, einde_simulatie}.

% Hulpfunctie om ook via console een freelancer record aan te kunnen maken
maak_freelancer(N, VN, LT, C) ->
  #freelancer{id = extensies:unieke_id(), naam=N, voornaam=VN, leeftijd=LT, contactgegevens=C}.

% Hulpfunctie om ook via console een job record aan te kunnen maken
maak_job(B, V, OG, F) ->
  #job{id = extensies:unieke_id(), beschrijving = B, vergoeding = V, opdrachtgever = OG, freelancer_id = F}.

% Hulpfunctie om ook via console een event record aan te kunnen maken zonder opdracht
maak_event(S, E) ->
  D = extensies:get_duur(S, E),
  #event{id = extensies:unieke_id(), start = S, duur = D}.

% Hulpfunctie om ook via console een event record aan te kunnen maken met opdracht/job
maak_event(S, E, O) ->
  D = extensies:get_duur(S, E),
  #event{id = extensies:unieke_id(), start = S, duur = D, opdracht = O}.
