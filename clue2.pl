:- dynamic weapons/1.
add_weapons(L) :- 
	assert(weapons(L)).
:- dynamic rooms/1.
add_rooms(L) :- 
	assert(rooms(L)).
:- dynamic suspects/1.
add_suspects(L) :- 
	assert(suspects(L)).

:- dynamic does_not_have/2.
:- dynamic has/2.
:- dynamic could_have/2.

:- dynamic control/1.

%% add players after adding all other elements, add in order
:- dynamic players/1.
add_players(L) :- 
	assert(players(L)), init_players(L).
init_players([]).
init_players([H|T]) :-  
	weapons(W), suspects(S), rooms(R), append(W, S, WS), append(WS, R, WSR),
	assert(could_have(H, WSR)), assert(has(H, [])), init_players(T).


%% define who you control and which cards you have
%% control has cards
%% other players can't have control's cards
control_player(CONTROL, CARDS) :- 
	assert(has(CONTROL, CARDS)), unset_all_could_have(CARDS).
unset_all_could_have(CARDS) :- 
	players(PLAYERS), member(PLAYER, PLAYERS), unset_could_have(PLAYER, CARDS).

%% every player but SUGGESTER can't have suggested cards
%% can do as single list, can do as 3 separate args (W, R, S), might need 3 separate args for more advanced functionality
unproven(SUGGESTER, CARDS) :- 
	players(PLAYERS), member(P, PLAYERS), not(P = SUGGESTER), unset_could_have(P, CARDS).

%% players between SUGGESTER and PROVER do not have CARDS
%% PROVER could have CARDS
%% have to consider order as cyclical
proven(SUGGESTER, PROVER, CARDS) :-
	player_between(SUGGESTER, PROVER, P), unset_could_have(P, CARDS).

%% same as other one, only we know which card prover has
proven_to_me(SUGGESTER, PROVER, CARDS, SHOWN_CARD) :-
	unset_all_between(SUGGESTER, PROVER, CARDS),
	set_has(PROVER, SHOWN_CARD).

unset_all_between(START, END, CARDS) :- player_between(SUGGESTER, PROVER, P), unset_could_have(P, CARDS).

%% make_suggestion(PLAYER, W, R, S) :- weapons(WEAPONS), rooms(ROOMS), suspects(SUSPECTS),
%% 	member(W, WEAPONS), member(R, ROOMS), member(S, SUSPECTS),
%% 	has(_, HAS_LIST), not(member(W, HAS_LIST)), not(member(R, HAS_LIST)), not(member(S, HAS_LIST)),
	


%% return players between START and END, 
%% if END is after START, return players that are after START AND before END
player_between(START, END, P) :- players(PLAYERS),
	append(_, [START|AFTER_S], PLAYERS), member(END, AFTER_S), 
	append(BEFORE_E, [END|_], PLAYERS), member(P, AFTER_S), member(P, BEFORE_E).
%% if END is before END, return players after START OR before END
player_between(START, END, P) :- players(PLAYERS),
	append(BEFORE_S, [START|AFTER_S], PLAYERS), member(END, BEFORE_S), 
	append(BEFORE_E, [END|_], PLAYERS), (member(P, AFTER_S); member(P, BEFORE_E)).


set_does_not_have(P, CARDS) :- 
	does_not_have(P, CURR), append(CURR, CARDS, NEW), sort(NEW, NEW_UNIQUE), 
	retract(does_not_have(P, CURR)), assert(does_not_have(P, NEW_UNIQUE)).
set_could_have(P, CARDS) :- 
	could_have(P, CURR), append(CURR, CARDS, NEW), sort(NEW, NEW_UNIQUE), 
	retract(could_have(P, CURR)), assert(does_not_have(P, NEW_UNIQUE)).
set_has(P, CARD) :- 
	has(P, CURR), append(CURR, [CARD], NEW), sort(NEW, NEW_UNIQUE), 
	retract(has(P, CURR)), assert(has(P, NEW_UNIQUE)).

unset_could_have(P, CARDS) :-
	could_have(P, CURRENT), subtract(CURRENT, CARDS, NEW), retract(could_have(P, CURRENT)), assert(could_have(P, NEW)).

%% add_weapons([w1, w2, w3, w4, w5, w6]).
%% add_rooms([r1, r2, r3, r4, r5, r6]).
%% add_suspects([s1, s2, s3, s4, s5, s6]).
/*

consult('clue.pl').

add_players([s1, s2, s3, s4, s5, s6]).

control_player(s3, [s2, w1, r1]).

*/


list :-
	rooms(R), write("Rooms: "), writeln(R),
	suspects(S), write("Suspects: "), writeln(S),
	weapons(W), write("Weapons: "), writeln(W).

list_could :- 
	could_have(P, COULD), write("Player:"), writeln(P), write("Could haves: "), writeln(COULD). 

list_has :- 
	has(P, CARDS), write("Player:"), writeln(P), write("Has: "), writeln(CARDS). 

%% print_could_have :- could_have(P1, COULD), write("Could haves: "), writeln(P1), writeln(COULD).
%% print_has :- has(P1, COULD), write("Has's: "), writeln(P1), writeln(COULD).



%% defaults for testing
weapons([w1, w2, w3, w4, w5, w6]).
rooms([r1, r2, r3, r4, r5, r6]).
suspects([s1, s2, s3, s4, s5, s6]).
