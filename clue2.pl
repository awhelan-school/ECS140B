/*
TESTING

[clue].

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

proven(s4, s1, [w3, s5, r3]).

proven(s4, s1, [w1, s2, r1]).
proven(s4, s1, [w1, s2, r2]).
proven(s4, s1, [w1, s2, r3]).
proven(s4, s1, [w1, s1, r4]).
proven(s4, s1, [w1, s2, r4]).
proven(s4, s1, [w1, s3, r4]).


EXAMPLE:
after this s1 must have w3 and w4, because controlling player has s1 and r1
[clue].
add_players([s1, s2, s3, s4, s5, s6]).
control_player(s3, [s1, w1, r1]).
proven(s4, s1, [w3, s1, r1]).
proven(s4, s1, [w4, s1, r1]).




*/





%% 
%% GAME SETUP
%% 
:- dynamic weapons/1.
:- dynamic rooms/1.
:- dynamic suspects/1.
:- dynamic players/1.
:- dynamic in_play/1.
:- dynamic confirmed/1.
add_weapons(L) :- 
	assert(weapons(L)).
add_rooms(L) :- 
	assert(rooms(L)).
add_suspects(L) :- 
	assert(suspects(L)).
add_players(L) :- 
	assert(players(L)), init_players(L),
	weapons(W), suspects(S), rooms(R), append(W, S, WS), append(WS, R, WSR),
	assert(in_play(WSR)),
	assert(confirmed([])).
%% add players after adding all other elements, add in order
init_players([]).
init_players([H|T]) :-  
	weapons(W), suspects(S), rooms(R), append(W, S, WS), append(WS, R, WSR),
	assert(possible(H, WSR)), assert(known(H, [])), assert(proofs(H, [])),
	init_players(T).


%% card states for each player
:- dynamic known/2.
:- dynamic possible/2.
:- dynamic proofs/2.
:- dynamic card_amount/2.
override_known(P, NEW) :- 
	known(P, CURRENT), retract(known(P, CURRENT)), assert(known(P, NEW)). 
override_possible(P, NEW) :- 
	possible(P, CURRENT), retract(possible(P, CURRENT)), assert(possible(P, NEW)). 


%% controlling player, the one who uses this program
%% define who you control and which cards you have
:- dynamic control/1.
control_player(P, CARDS) :- 
	assert(control(P)),
	set_known_multiple(P, CARDS), 
	possible(P, CURRENT), retract(possible(P, CURRENT)), assert(possible(P, [])), fail; true.




%% 
%% PLAYING THE GAME
%% 

%% call this when a suggestion happens and it is not proven by anyone
%% every player but SUGGESTER can't have suggested cards,
%% since they didn't prove the suggestion
unproven(SUGGESTER, CARDS) :- 
	players(PLAYERS), member(P, PLAYERS), not(P = SUGGESTER), unset_possible(P, CARDS),
	in_play(INPLAY), check_confirmed(INPLAY), analyze_proofs_all(), fail; true.


%% call this when a suggestion is proven, but you are not the suggester
%% add proof for prover
%% players between SUGGESTER and PROVER can't have suggested cards
%% have to consider order as cyclical
proven(SUGGESTER, PROVER, CARDS) :-
	add_proof(PROVER, CARDS),
	unset_possible_for_all_between(SUGGESTER, PROVER, CARDS),
	in_play(INPLAY), check_confirmed(INPLAY), analyze_proofs_all(), fail; true.
add_proof(P, PROOF) :-
	proofs(P, CURRENT), not(member(PROOF, CURRENT)), retract(proofs(P, CURRENT)), 
	append(CURRENT, [PROOF], NEW), assert(proofs(P, NEW)).

%% same as other one, only we are making the suggestion 
%% and therefore see which card the prover showed
proven_to_me(SUGGESTER, PROVER, CARDS, SHOWN_CARD) :-
	set_known(PROVER, SHOWN_CARD),
	unset_possible_for_all_between(SUGGESTER, PROVER, CARDS),
	in_play(INPLAY), check_confirmed(INPLAY), analyze_proofs_all(), fail; true.

%% helper predicate for proofs
unset_possible_for_all_between(START, END, CARDS) :- 
	player_between(START, END, P), unset_possible(P, CARDS).
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



%% called after each alteration to possible or known for any player
%% need to do it for all players each time
%% if analyze_proofs() evaluates to true, that means set_known_multiple() has been called and therefore there's new information
%% in that case call analyze_proofs_all recursively, do this until no new info is obtained
%% analyze_proofs_all().
analyze_proofs_all() :-
	writeln("yay"), players(PLAYERS), member(P, PLAYERS), analyze_proofs(P), analyze_proofs_all(), fail; true.

analyze_proofs(P) :-
	setof(CARDS, generate_possible_hand(P, CARDS), ALL), 
	not(length(ALL, 0)), intersection_of_sets(ALL, INTERSECTION),
	known(P, CURRENT), not(contains(CURRENT, INTERSECTION)), %% need to check if intersection is already member of known, if it is, don't call set_known again
	writeln("Deduced that this player has these cards:"), write(P), write(" "), write(INTERSECTION),
	set_known_multiple(P, INTERSECTION).  %% set flag that we got new info and to run the whole thing again 


%% handle special cases of empty and 1
intersection_of_sets([], []).
intersection_of_sets([X], X).
intersection_of_sets(SETS, INTER) :-
	not(length(SETS, 1)), not(length(SETS, 0)), 
	SETS = [H|_],
	intersection_of_sets_helper(SETS, H, INTER).
	
intersection_of_sets_helper([H], PREV, INTER) :-
	sort(H, SORTED_H), intersection(SORTED_H, PREV, INTER).
intersection_of_sets_helper([H|T], PREV, INTER) :-
	not(length([H1, H2|T], 1)), not(length([H1, H2|T], 0)), 
	sort(H, SORTED_H), intersection(SORTED_H, PREV, NEXT), intersection_of_sets_helper(T, NEXT, INTER). 




%% TODO: check if generated hand contains new info, set_known only if that's true, if set_has happens, set flag "new_info", check at the end if new info and if so run everything again
%% the generated hand should contain ALL cards in known list, the rest must be from possible list
%% can't contain only less than all cards in known list

%% generate possible cards that player P could have
%% these cards must either be in could have or known list of player P
%% they also must together satisfy all proofs that P known done
generate_possible_hand(P, CARDS) :-
	card_amount(P, CARD_AMOUNT),
	length(UCARDS, CARD_AMOUNT),
	all_possible_or_known(P, UCARDS),
	proves_all_proofs(P, UCARDS),
	is_set(UCARDS),
	sort(UCARDS, CARDS).

%% true if A contains B
contains(_, []).
contains(A, [H]) :-
	member(H, A).
contains(A, [H|T]) :-
	member(H, A), contains(A, T). 



all_possible_or_known(_, []).
all_possible_or_known(P, [H|T]) :-
	known(P, KNOWN), possible(P, POSSIBLE),
	(member(H, KNOWN); member(H, POSSIBLE)),
	all_possible_or_known(P, T).

proves_all_proofs(P, CARDS) :-
	proofs(P, PROOFS), proves_all_proofs_helper(P, CARDS, PROOFS).
proves_all_proofs_helper(_, _, []).
proves_all_proofs_helper(P, CARDS, [H|T]) :-
	member(C, CARDS), member(C, H),
	proves_all_proofs_helper(P, CARDS, T).




card_known(C) :-
	players(PS), member(P, PS), known(P, KNOWN),
	member(C, KNOWN).

card_possible(C) :-
	players(PS), member(P, PS), possible(P, POSSIBLE),
	member(C, POSSIBLE).
%% card_impossible(C) :-
%% 	players(PS),
%% 	card_impossible_helper(C, PS).

%% card_impossible_helper(C, []).
%% card_impossible_helper(C, [H|T]) :-
%% 	possible(H, POSSIBLE), not(member(C, POSSIBLE)) ,	
%% 	card_impossible_helper(T).



check(C) :-
	not(card_known(C)), not(card_possible(C)),
	confirmed(CURRENT), not(member(C, CURRENT)),
	append(CURRENT, [C], NEW), 
	retract(confirmed(CURRENT)), writeln("Added card to confirmed list:"), 
	writeln(C), assert(confirmed(NEW)), fail; true.

check_confirmed([]).
check_confirmed([H|T]) :-
	check(H),
	check_confirmed(T).






unset_possible(P, CARDS) :-
	possible(P, CURRENT), subtract(CURRENT, CARDS, NEW), retract(possible(P, CURRENT)), 
	assert(possible(P, NEW)).

unset_possible_for_everyone(CARDS) :- 
	players(PLAYERS), member(P, PLAYERS), unset_possible(P, CARDS).

set_known(P, CARD) :- 
	known(P, CURR), append(CURR, [CARD], NEW), sort(NEW, NEW_UNIQUE), retract(known(P, CURR)), 
	assert(known(P, NEW_UNIQUE)), unset_possible_for_everyone([CARD]), 
	in_play(INPLAY), subtract(INPLAY, [CARD], NEW_INPLAY), retract(in_play(INPLAY)), assert(in_play(NEW_INPLAY)), 
	fail; true.

set_known_multiple(_, []).
set_known_multiple(P, [H|T]) :-
	set_known(P, H), set_known_multiple(P, T). 


list :-
	rooms(R), write("Rooms: "), writeln(R),
	suspects(S), write("Suspects: "), writeln(S),
	weapons(W), write("Weapons: "), writeln(W).

list_possible :- 
	possible(P, CARDS), write("Player:"), writeln(P), write("Possible: "), writeln(CARDS). 

list_known :- 
	known(P, CARDS), write("Player:"), writeln(P), write("Known: "), writeln(CARDS). 

list_proofs :- 
	proofs(P, CARDS), write("Player:"), writeln(P), write("Proved: "), writeln(CARDS).

list_in_play :- 
	in_play(CARDS), write("In play:"), writeln(CARDS).

list_confirmed :- 
	confirmed(CARDS), write("Confirmed:"), writeln(CARDS).

%% defaults for testing
%% weapons([w1, w2, w3, w4, w5, w6]).
%% rooms([r1, r2, r3, r4, r5, r6]).
%% suspects([s1, s2, s3, s4, s5, s6]).
%% card_amount(s1, 3).
%% card_amount(s2, 3).
%% card_amount(s3, 3).
%% card_amount(s4, 3).
%% card_amount(s5, 3).
%% card_amount(s6, 3).


%% order is important
weapons([candlestick, dagger, leadpipe, revolver, rope, wrench]).
rooms([ballroom, billiardroom, conservatory, diningroom, hall, kitchen, library, lounge, study]).
suspects([mustard, orchid, green, scarlett, peacock, plum]).
card_amount(mustard, 5).
card_amount(orchid, 5).
card_amount(green, 4).
card_amount(scarlett, 4).

/*
CLUE VIDEO
weapons([candlestick, dagger, leadpipe, revolver, rope, wrench]).
rooms([ballroom, billiardroom, conservatory, diningroom, hall, kitchen, library, lounge, study]).
suspects([mustard, orchid, green, scarlett, peacock, plum]).
add_players([mustard, orchid, green, scarlett]).
card_amount(mustard, 5).
card_amount(orchid, 5).
card_amount(green, 4).
card_amount(scarlett, 4).

these must be called after consulting script
add_players([mustard, orchid, green, scarlett]).
control_player(mustard, [peacock, plum, leadpipe, kitchen, study]).


1. mustard
proven_to_me(mustard, orchid, [green,candlestick,ballroom], candlestick).
orchid has candlestick
candlestick removed from all possible list

2. orchid
proven(orchid, green, [peacock,rope,ballroom]).
new proof in green's proofs list

3. green
proven(green, mustard, [orchid,leadpipe,billiardroom]).
scarlett doesn't have orchid

scarlett can't move into a room

---------------
4. mustard
proven_to_me(mustard, orchid, [orchid,dagger,billiardroom], candlestick).
orchid has candlestick

5. orchid
proven(orchid, green, [plum,dagger,conservatory]).

6. green
proven(green, scarlett, [mustard,rope,conservatory]).

7. scarlett
proven(scarlett, orchid, [orchid,rope,diningroom]).

---------------
8. mustard
proven_to_me(mustard, green, [orchid,dagger,lounge], dagger).
green has dagger

9. orchid
proven(orchid, green, [scarlett,rope,library]).

10. green
proven(green, mustard, [plum,rope,billiardroom]).

11. scarlett
proven(scarlett, orchid, [green,rope,billiardroom]).

---------------
12. mustard
proven_to_me(mustard, orchid, [orchid,rope,diningroom], diningroom).

13. orchid
proven(orchid, mustard, [peacock,rope,diningroom]).
program deduces that green has ballroom

14. green
proven(green, scarlett, [scarlett,revolver,diningroom]).

15. scarlett
proven(scarlett, mustard, [peacock,dagger,kitchen]).

---------------
16. mustard
proven_to_me(mustard, scarlett, [orchid,rope,lounge], lounge).

17. orchid
proven(orchid, scarlett, [mustard,rope,lounge]).

18. green
proven(green, mustard, [orchid,candlestick,kitchen]).

19. scarlett
proven(scarlett, orchid, [mustard,candlestick,billiardroom]).

---------------
20. mustard
unproven(mustard, [orchid,rope,conservatory]).

*/

