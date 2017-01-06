%%%-------------------------------------------------------------------
%%% @author Thomas
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. Jan 2017 17:15
%%%-------------------------------------------------------------------
-module(freelancer).
-author("Thomas").

%% API
-export([start/0, insert/1, wijs_event_af/2, get_werkuur_loon_opdracht/1, get_event_bij_job/1,
  get_werkuren_lonen_deze_week/1, get_totaal_werkuur_vergoeding_deze_week/1,
  get_werkuren_lonen_gegeven_week/2, get_totaal_werkuur_vergoeding_gegeven_week/2,
  registreer_event/1]).

-include("records.hrl").

% Initialiseer de freelancer applicatie
start() ->
  ets:new(freelancers, [ordered_set, named_table, {keypos, #freelancer.id}]),
  ets:new(jobs, [ordered_set, named_table, {keypos, #job.id}]),
  ets:new(agenda, [ordered_set, named_table, {keypos, #event.id}]).

% Voeg een nieuw record toe aan de overeenkomstige ets-tabel.
insert(F = #freelancer{}) ->
  ets:insert(freelancers, F);
insert(J = #job{}) ->
  ets:insert(jobs, J);
insert(E = #event{}) ->
  ets:insert(agenda, E);
insert(_) ->
  throw({error, undefined_record}).

% Registreer een nieuw evenement in de kalender
% Wanneer het om een event met een opdracht gaat,
% zou er een melding kunnen gestuurd worden via sms/email aan de freelancer.
registreer_event(E = #event{opdracht = null}) ->
  insert(E);
registreer_event(E = #event{}) ->
  [Job] = ets:match_object(jobs, #job{id = E#event.opdracht, _='_'}),
  [F] = ets:match_object(freelancers, #freelancer{id = Job#job.freelancer_id, _='_'}),
  #{adres := _, tel := Tel, email := Mail} = F#freelancer.contactgegevens,
  io:fwrite("Nieuw event voor freelancer '~s ~s',~n     er wordt een melding gestuurd naar ~s en ~s.~n", [F#freelancer.naam, F#freelancer.voornaam, Tel, Mail]),
  insert(E).

% Haal het totaal aantal werkuren en vergoeding van de huidige week op
% Returns: {Totaal Werkuren per week, Totale vergoeding van de week}
get_totaal_werkuur_vergoeding_deze_week(FID) ->
  get_totaal_werkuur_vergoeding_gegeven_week(FID, extensies:get_limietdagen_van_huidige_week()).

% Haal het totaal aantal werkuren en vergoeding van de gegeven week op
% Returns: {Totaal Werkuren per week, Totale vergoeding van de week}
get_totaal_werkuur_vergoeding_gegeven_week(FID, {BeginDatum, EindDatum}) ->
  Lijst = get_werkuren_lonen_gegeven_week(FID, {BeginDatum, EindDatum}),
  lists:foldl(fun({WU, V}, {SomWU, SomV}) -> {WU + SomWU, V + SomV} end, {0,0}, Lijst).

% Geeft het aantal werkuren en totale vergoedingen terug voor jobs die starten in de de huidige week
% Returns: Lijst van {Totaal Werkuur, Totale Vergoeding}
get_werkuren_lonen_deze_week(FID) ->
  get_werkuren_lonen_gegeven_week(FID, extensies:get_limietdagen_van_huidige_week()).

% Geeft het aantal werkuren en totale vergoedingen terug voor jobs die starten in de opgegeven week
% Returns: Lijst van {Totaal Werkuur, Totale Vergoeding}
get_werkuren_lonen_gegeven_week(FID, {BeginDatum, EindDatum}) ->
  Jobs = ets:tab2list(jobs),
  SJobs = lists:filter(fun(J) -> J#job.freelancer_id == FID end, Jobs),

  FEvents = map_jobs_naar_events(SJobs),
  EventJobsId = lists:zipwith(fun(J, E) -> {J#job.id, E} end, SJobs, FEvents),

  {SBD, SED} = {calendar:datetime_to_gregorian_seconds(BeginDatum), calendar:datetime_to_gregorian_seconds(EindDatum)},

  EventsBeginnendDezeWeek = lists:filter(fun({_, E}) ->
                            StartE = calendar:datetime_to_gregorian_seconds(E#event.start),
                            StartE >= SBD andalso StartE =< SED
                          end, EventJobsId),

  lists:map(fun({JId, _}) ->
              get_werkuur_loon_opdracht(JId)
            end, EventsBeginnendDezeWeek).

% Geeft het aantal werkuren en totale vergoeding voor een specifieke job terug
% Returns: {Werkuur, Totale Vergoeding}
get_werkuur_loon_opdracht(JID) ->
  [J] = ets:lookup(jobs, JID),
  E = get_event_bij_job(J#job.id),

  Werkuren = E#event.duur/3600,
  case J#job.vergoeding of
    {vast, Prijs} -> {Werkuren, Prijs};
    {per_uur, Ppu} -> {Werkuren, (Werkuren*Ppu) };
    _ -> throw({error, geen_vergoeding})
  end.

% Wanneer een freelancer zijn opdracht wilt afwijzen (doorgeven aan iemand anders),
% kan deze functie gebruikt worden.
wijs_event_af(FID, EID) ->
  [Event] = ets:lookup(agenda, EID),
  io:fwrite("Freelancer ~p wilt het volgende event afwijzen: ~p.~n", [FID, EID]),
  % Eerst controle of freelancer wel bij deze event hoort
  case Event#event.opdracht of
      null -> throw({error, geen_opdracht_event});
      JobId ->
        [Job] = ets:lookup(jobs, JobId),
        if
          Job#job.freelancer_id =/= FID -> throw({error, geen_combinatie});
          true ->

            % Haal andere mogelijke freelancers op
            AndereFreelancers = lists:filter(fun(F) -> F#freelancer.id =/= FID end, ets:tab2list(freelancers)),

            % Haal hieruit de freelancers die beschikbaar zijn, dus geen overlap hebben met andere opdrachten
            Jobs = ets:tab2list(jobs),
            BeschikbareFls = lists:filter(fun(F) ->
                                            JobsVanF = lists:filter(fun(J) -> J#job.freelancer_id == F#freelancer.id end, Jobs),
                                            EventsVanF = map_jobs_naar_events(JobsVanF),

                                            Start = calendar:datetime_to_gregorian_seconds(Event#event.start),
                                            check_overlapping(Start, Start+Event#event.duur, EventsVanF) == false
                                          end, AndereFreelancers),

            if
              length(BeschikbareFls)==0 -> io:fwrite("Er zijn geen andere freelancers beschikbaar, je zal moeten werken~n", []);
              true ->
                % Neem eerstvolgende mogelijke freelancer de opdracht
                F = lists:nth(1, BeschikbareFls),
                ets:update_element(jobs, JobId, {6, F#freelancer.id}) orelse io:fwrite("De herindeling is niet gelukt~n", []),
                io:fwrite("Job ~p '~s' is nu toegekend aan '~s ~s'.~n", [Job#job.id, Job#job.beschrijving, F#freelancer.naam, F#freelancer.voornaam])
            end
        end
  end.

% Check of er voor de gegeven begin en eindtijd een overlapping is met een event uit de gegeven lijst
check_overlapping(BeginTijd, EindTijd, Lijst) ->
  if
    length(Lijst) == 0 -> false;
    true ->
      length(lists:filter(
        fun(#event{start = StartG, duur = Duur}) ->
          Start = calendar:datetime_to_gregorian_seconds(StartG),
          Eind = Start + Duur,
          (not ((EindTijd < Start) or (Eind < BeginTijd)))
        end, Lijst)) =/= 0
  end.

% Haal het bijhorende event op bij specifieke job
get_event_bij_job(JID) ->
  [E] = ets:match_object(agenda, #event{opdracht = JID, _='_'}), E.

% Map een lijst van jobs naar een lijst van events
map_jobs_naar_events(Jobs) ->
  lists:map(fun(J) -> get_event_bij_job(J#job.id) end, Jobs).




