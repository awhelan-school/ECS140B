%% 
%% GAME SETUP
%% 
:- dynamic weapons/1.
add_weapons(L) :- 
	assert(weapons(L)).
:- dynamic rooms/1.
add_rooms(L) :- 
	assert(rooms(L)).
:- dynamic suspects/1.
add_suspects(L) :- 
	assert(suspects(L)).

%% lists of cards players are confirmed to have and could have
%% cards are either in "has" or "could have" or neither, but not in both
%% at the start of the game could haves contain all cards
%% when a player is confirmed to have a card, it is added to his has list 
%% and removed from his could have list. It is also removed from everyone else's could have lists
:- dynamic has/2.
:- dynamic could_have/2.

%% add players after adding all other elements, add in order
:- dynamic players/1.
add_players(L) :- 
	assert(players(L)), init_players(L).
init_players([]).
init_players([H|T]) :-  
	weapons(W), suspects(S), rooms(R), append(W, S, WS), append(WS, R, WSR),
	assert(could_have(H, WSR)), assert(has(H, [])), init_players(T).


%% controlling player, the one who uses this program
:- dynamic control/1.
%% define who you control and which cards you have
%% other players can't have those cards(including you)
control_player(P, CARDS) :- 
	unset_all_could_have(CARDS), 
	has(P, CURRENT), retract(has(P, CURRENT)), assert(has(P, CARDS)).
unset_all_could_have(CARDS) :- 
	players(PLAYERS), member(P, PLAYERS), unset_could_have(P, CARDS).





%% 
%% PLAYING THE GAME
%% 

%% call this when a suggestion happens and it is not proven by anyone
%% every player but SUGGESTER can't have suggested cards,
%% since they didn't prove the suggestion
unproven(SUGGESTER, CARDS) :- 
	players(PLAYERS), member(P, PLAYERS), not(P = SUGGESTER), unset_could_have(P, CARDS).


%% call this when a suggestion is proven, but you are not the suggester
%% players between SUGGESTER and PROVER can't have suggested cards
%% have to consider order as cyclical
proven(SUGGESTER, PROVER, CARDS) :-
	player_between(SUGGESTER, PROVER, P), unset_could_have(P, CARDS).


%% same as other one, only we are making the suggestion 
%% and therefore see which cards the prover showed
proven_to_me(SUGGESTER, PROVER, CARDS, SHOWN_CARD) :-
	set_has(PROVER, SHOWN_CARD),
	unset_all_could_have([SHOWN_CARD]),
	unset_all_between(SUGGESTER, PROVER, CARDS).


%% return players between START and END, 
%% if END is after START, return players that are after START AND before END
player_between(START, END, P) :- 
players(PLAYERS),
	append(_, [START|AFTER_S], PLAYERS), member(END, AFTER_S), 
	append(BEFORE_E, [END|_], PLAYERS), member(P, AFTER_S), member(P, BEFORE_E).
%% if END is before END, return players after START OR before END
player_between(START, END, P) :- 
players(PLAYERS),
	append(BEFORE_S, [START|AFTER_S], PLAYERS), member(END, BEFORE_S), 
	append(BEFORE_E, [END|_], PLAYERS), (member(P, AFTER_S); member(P, BEFORE_E)).

%% helper predicate for proofs
unset_all_between(START, END, CARDS) :- 
	player_between(START, END, P), unset_could_have(P, CARDS).




%% make_suggestion(PLAYER, W, R, S) :- weapons(WEAPONS), rooms(ROOMS), suspects(SUSPECTS),
%% 	member(W, WEAPONS), member(R, ROOMS), member(S, SUSPECTS),
%% 	has(_, HAS_LIST), not(member(W, HAS_LIST)), not(member(R, HAS_LIST)), not(member(S, HAS_LIST)),
	


unset_could_have(P, CARDS) :-
	could_have(P, CURRENT), subtract(CURRENT, CARDS, NEW), retract(could_have(P, CURRENT)), 
	assert(could_have(P, NEW)).
set_has(P, CARD) :- 
	has(P, CURR), append(CURR, [CARD], NEW), sort(NEW, NEW_UNIQUE), retract(has(P, CURR)), 
	assert(has(P, NEW_UNIQUE)).


list :-
	rooms(R), write("Rooms: "), writeln(R),
	suspects(S), write("Suspects: "), writeln(S),
	weapons(W), write("Weapons: "), writeln(W).

list_could :- 
	could_have(P, COULD), write("Player:"), writeln(P), write("Could have: "), writeln(COULD). 

list_has :- 
	has(P, CARDS), write("Player:"), writeln(P), write("Has: "), writeln(CARDS). 



%% defaults for testing
weapons([w1, w2, w3, w4, w5, w6]).
rooms([r1, r2, r3, r4, r5, r6]).
suspects([s1, s2, s3, s4, s5, s6]).


/*
TESTING

consult('clue.pl').

add_players([s1, s2, s3, s4, s5, s6]).

control_player(s3, [s1, w1, r1]).

unproven(s1, [s2, w2, r2]).
^ should remove s2, w2 and r2 from everyone's could have list except s1's

proven(s1, s4, [s2, w3, r2]).
^ removes w3 from could have lists of s2, s3 only

proven_to_me(s3, s4, [s2, w3, r2], w3). 
^ s4 now has w3, w3 is removed from everyone else's could have lists
*/