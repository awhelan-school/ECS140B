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
:- dynamic card_amount/1.
set_card_amount(X) :-
	assert(card_amount(X)).

%% card states for each player
:- dynamic has/2.
:- dynamic could_have/2.
:- dynamic proof/2.

%% add players after adding all other elements, add in order
:- dynamic players/1.
add_players(L) :- 
	assert(players(L)), init_players(L).
init_players([]).
init_players([H|T]) :-  
	weapons(W), suspects(S), rooms(R), append(W, S, WS), append(WS, R, WSR),
	assert(could_have(H, WSR)), assert(has(H, [])), assert(proof(H, [])),
	init_players(T).


%% controlling player, the one who uses this program
:- dynamic control/1.
%% define who you control and which cards you have
control_player(P, CARDS) :- 
	assert(control(P)),
	set_has_multiple(P, CARDS).



%% 
%% PLAYING THE GAME
%% 

%% call this when a suggestion happens and it is not proven by anyone
%% every player but SUGGESTER can't have suggested cards,
%% since they didn't prove the suggestion
unproven(SUGGESTER, CARDS) :- 
	players(PLAYERS), member(P, PLAYERS), not(P = SUGGESTER), unset_could_have(P, CARDS).


%% call this when a suggestion is proven, but you are not the suggester
%% add proof for prover
%% players between SUGGESTER and PROVER can't have suggested cards
%% have to consider order as cyclical
proven(SUGGESTER, PROVER, CARDS) :-
	add_proof(PROVER, CARDS),
	unset_could_have_for_all_between(SUGGESTER, PROVER, CARDS).
add_proof(P, PROOF) :-
	proof(P, CURRENT), retract(proof(P, CURRENT)), 
	append(CURRENT, [PROOF], NEW), assert(proof(P, NEW)).

%% same as other one, only we are making the suggestion 
%% and therefore see which card the prover showed
proven_to_me(SUGGESTER, PROVER, CARDS, SHOWN_CARD) :-
	set_has(PROVER, SHOWN_CARD),
	unset_could_have_for_all_between(SUGGESTER, PROVER, CARDS).

%% helper predicate for proofs
unset_could_have_for_all_between(START, END, CARDS) :- 
	player_between(START, END, P), unset_could_have(P, CARDS).
%% return players between START and END, 
%% if END is after START, return players that are after START AND before END
player_between(START, END, P) :- 
players(PLAYERS),
	append(_, [START|AFTER_S], PLAYERS), member(END, AFTER_S), 
	append(BEFORE_E, [END|_], PLAYERS), member(P, AFTER_S), member(P, BEFORE_E).
%% if START is before END, return players after START OR before END
player_between(START, END, P) :- 
players(PLAYERS),
	append(BEFORE_S, [START|AFTER_S], PLAYERS), member(END, BEFORE_S), 
	append(BEFORE_E, [END|_], PLAYERS), (member(P, AFTER_S); member(P, BEFORE_E)).


%% called after each alteration to could_have or has for any player
%% need to do it for all players each time
analyze_proofs_all() :-
	players(PLAYERS), member(P, PLAYERS), analyze_proofs(P).

analyze_proofs(P) :-
	setof(CARDS, generate_possible_hand(P, CARDS), ALL), 
	intersection_of_sets(ALL, INTERSECTION), not(length(INTERSECTION, 0)),
	set_has_multiple(P, INTERSECTION). %% need to check if intersection is already member of has, if it is, don't call set_has again

intersection_of_sets([], []).
intersection_of_sets([H], H).
intersection_of_sets([H1|[H2|T]], INTER) :-
	sort(H1, SORTED_H1), sort(H2, SORTED_H2),
	intersection(SORTED_H1, SORTED_H2, INTER), 
	(length([H2|T], 1); intersection_of_sets([H2|T], INTER)).

%% generate possible cards that player P could have
%% these cards must either be in could have or has list of player P
%% they also must together satisfy all proofs that P has done
generate_possible_hand(P, CARDS) :-
	card_amount(CARD_AMOUNT),
	length(UCARDS, CARD_AMOUNT),
	all_could_or_has(P, UCARDS),
	proves_all_proofs(P, UCARDS),
	is_set(UCARDS),
	sort(UCARDS, CARDS).


all_could_or_has(_, []).
all_could_or_has(P, [H|T]) :-
	has(P, HAS), could_have(P, COULD),
	(member(H, HAS); member(H, COULD)),
	all_could_or_has(P, T).

proves_all_proofs(P, CARDS) :-
	proof(P, PROOFS), proves_all_proofs_helper(P, CARDS, PROOFS).
proves_all_proofs_helper(P, CARDS, []).
proves_all_proofs_helper(P, CARDS, [H|T]) :-
	member(C, CARDS), member(C, H),
	proves_all_proofs_helper(P, CARDS, T).

	
unset_could_have(P, CARDS) :-
	could_have(P, CURRENT), subtract(CURRENT, CARDS, NEW), retract(could_have(P, CURRENT)), 
	assert(could_have(P, NEW)).

unset_could_have_for_everyone(CARDS) :- 
	players(PLAYERS), member(P, PLAYERS), unset_could_have(P, CARDS).

set_has(P, CARD) :- 
	has(P, CURR), append(CURR, [CARD], NEW), sort(NEW, NEW_UNIQUE), retract(has(P, CURR)), 
	assert(has(P, NEW_UNIQUE)), unset_could_have_for_everyone([CARD]), fail; true.

set_has_multiple(P, []).
set_has_multiple(P, [H|T]) :-
	set_has(P, H), set_has_multiple(P, T). 



list :-
	rooms(R), write("Rooms: "), writeln(R),
	suspects(S), write("Suspects: "), writeln(S),
	weapons(W), write("Weapons: "), writeln(W).

list_could :- 
	could_have(P, COULD), write("Player:"), writeln(P), write("Could have: "), writeln(COULD). 

list_has :- 
	has(P, CARDS), write("Player:"), writeln(P), write("Has: "), writeln(CARDS). 

list_proofs :- 
	proof(P, CARDS), write("Player:"), writeln(P), write("Proved: "), writeln(CARDS). 

%% defaults for testing
weapons([w1, w2, w3, w4, w5, w6]).
rooms([r1, r2, r3, r4, r5, r6]).
suspects([s1, s2, s3, s4, s5, s6]).
card_amount(3).


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




example for generate_possible_hand()
input all of this, then generate_possible_hand(s1, X), it will output permutations of [w3, s2, r4]

proven(s4, s1, [w3, s1, r1]).
proven(s4, s1, [w3, s1, r2]).
proven(s4, s1, [w3, s1, r3]).

proven(s4, s1, [w1, s2, r1]).
proven(s4, s1, [w1, s2, r2]).
proven(s4, s1, [w1, s2, r3]).
proven(s4, s1, [w1, s1, r4]).
proven(s4, s1, [w1, s2, r4]).
proven(s4, s1, [w1, s3, r4]).
*/
