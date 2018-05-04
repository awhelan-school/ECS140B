

%% 
%% GAME SETUP
%% 
:- dynamic weapons/1.
:- dynamic rooms/1.
:- dynamic suspects/1.
:- dynamic players/1.
:- dynamic inplay/1.
:- dynamic solution/1.
solution([]).

add_weapons(L) :- 
	assert(weapons(L)).
add_rooms(L) :- 
	assert(rooms(L)).
add_suspects(L) :- 
	assert(suspects(L)).
add_players(L) :- 
	assert(players(L)), 
	init_players(L),
	weapons(W), suspects(S), rooms(R), append(W, S, WS), append(WS, R, WSR),
	assert(inplay(WSR)).
%% add players after adding all other elements, add in order
init_players([]).
init_players([H|T]) :-  
	weapons(W), suspects(S), rooms(R), append(W, S, WS), append(WS, R, WSR),
	assert(possible(H, WSR)), assert(known(H, [])), assert(proofs(H, [])),
	init_players(T).


%% states for each player
:- dynamic known/2.
:- dynamic possible/2.
:- dynamic proofs/2.
:- dynamic card_amount/2.


%% controlling player, the one who uses this program
%% define who you control and which cards you have
:- dynamic control/1.
control_player(P, CARDS) :- 
	assert(control(P)),
	set_known(P, CARDS), 
	possible(P, CURRENT), 
	retract(possible(P, CURRENT)), assert(possible(P, [])), 
	fail; true.




%% 
%% PLAYING THE GAME
%% 



generate_suggestion(BEST_SUGGESTION) :-
	setof(S, possible_suggestion(S), ALL),
	best_suggestion(ALL, BEST_SUGGESTION).   

possible_suggestion([S, W, R]) :-
	weapons(WS), suspects(SS), rooms(RS),
	member(W, WS), member(S, SS), member(R, RS).

best_suggestion([H|T], BEST) :-
    best_suggestion_helper(T, H, BEST).

best_suggestion_helper([], BEST, BEST).
best_suggestion_helper([H|T], PREV, BEST) :-
	score_total(H, H_SCORE), 
	score_total(PREV, PREV_SCORE),
	H_SCORE > PREV_SCORE, 
    best_suggestion_helper(T, H, BEST).
best_suggestion_helper([H|T], PREV, BEST) :-
	score_total(H, H_SCORE), 
	score_total(PREV, PREV_SCORE),
	H_SCORE =< PREV_SCORE, 
    best_suggestion_helper(T, PREV, BEST).
	
score_total([C1, C2, C3], TOTAL) :-
	score(C1, S1), 
	score(C2, S2), 
	score(C3, S3),
	TOTAL is S1 + S2 + S3.

%% 10 - cards still in play are the best
%% 1  - cards that control has are ok alternative for when cards there are no cards left in play for that type
%% 0  - cards not in play and not owned by control are the worst
score(C, 10) :-
	inplay(INPLAY), member(C, INPLAY), 
	!.
score(C, 1) :-
	inplay(INPLAY), not(member(C, INPLAY)), 
	control(CONTROL), known(CONTROL, KNOWN),
	member(C, KNOWN),
	!.
score(C, 0) :-
	inplay(INPLAY), member(C, INPLAY),
	control(CONTROL), P \= CONTROL,
	known(P, KNOWN), not(member(C, KNOWN)), 
	!.



%% call this when a suggestion happens and it is not proven by anyone
%% every player but SUGGESTER can't have suggested cards,
unproven(SUGGESTER, CARDS) :- 
	players(PLAYERS), member(P, PLAYERS), not(P = SUGGESTER), 
	unset_possible(P, CARDS),
	check_for_solution(), 
	analyze_proofs_all(), 
	fail; true.


%% call this when a suggestion is proven, but you are not the suggester
%% add proof for prover
%% players between SUGGESTER and PROVER can't have suggested cards
proven(SUGGESTER, PROVER, CARDS) :-
	add_proof(PROVER, CARDS),
	unset_possible_for_all_between(SUGGESTER, PROVER, CARDS),
	check_for_solution(), 
	analyze_proofs_all(),
	fail; true.
add_proof(P, PROOF) :-
	proofs(P, CURRENT), not(member(PROOF, CURRENT)), retract(proofs(P, CURRENT)), 
	append(CURRENT, [PROOF], NEW), assert(proofs(P, NEW)), 
	fail; true.

%% same as other one, only we are making the suggestion 
%% and therefore see which card the prover showed
proven_to_me(SUGGESTER, PROVER, CARDS, SHOWN_CARD) :-
	set_known(PROVER, [SHOWN_CARD]),
	unset_possible_for_all_between(SUGGESTER, PROVER, CARDS),
	check_for_solution(), 
	analyze_proofs_all(), 
	fail; true.







%% analyze proofs that players did to maybe deduce which cards they have
:- dynamic flag/1.
flag(1).

analyze_proofs_all() :-
	writeln("analyzing"),
	retractall(flag(_)),
	analyze_proofs_all_each(),
	flag(1), analyze_proofs_all().

analyze_proofs_all_each() :-
	players(PLAYERS), member(P, PLAYERS), 
	analyze_proofs(P), fail; true. 

analyze_proofs(P) :-
	setof(CARDS, possible_hand(P, CARDS), ALL), 
	not(length(ALL, 0)), intersection_of_sets(ALL, INTERSECTION),
	not(length(INTERSECTION, 0)),
	known(P, CURRENT), not(contains(CURRENT, INTERSECTION)), %% need to check if intersection is already member of known, if it is, don't call set_known again
	writeln("Deduced that this player has these cards:"), write(P), write(" "), write(INTERSECTION),
	set_known(P, INTERSECTION). 

%% true if list A contains list B
contains(_, []).
contains(A, [H]) :-
	member(H, A).
contains(A, [H|T]) :-
	member(H, A), contains(A, T).

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


%% generate possible cards that player P could have
%% these cards must either be in could have or known list of player P
%% they also must together satisfy all proofs that P known done
possible_hand(P, HAND) :-
	card_amount(P, CARD_AMOUNT),
	length(HAND_UNSORTED, CARD_AMOUNT),
	all_possible_or_known(P, HAND_UNSORTED),
	proves_all_proofs(P, HAND_UNSORTED),
	is_set(HAND_UNSORTED),
	sort(HAND_UNSORTED, HAND). 

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

check_for_solution() :-
	inplay(INPLAY), check_for_solution_helper(INPLAY),
	fail; true.

check_for_solution_helper([]).
check_for_solution_helper([H|T]) :-
	check_if_card_is_solution(H),
	check_for_solution_helper(T);
	true.

check_if_card_is_solution(C) :-
	not(card_known(C)), not(card_possible(C)),
	solution(CURRENT), not(member(C, CURRENT)),
	append(CURRENT, [C], NEW), 
	retract(solution(CURRENT)), assert(solution(NEW)),
	writeln("Added card to solution list:"), writeln(C),  
	remove_of_same_type(C), %% if a card of type is in solution, all other cards of same type are out of play
	fail; true.

card_known(C) :-
	players(PS), member(P, PS), known(P, KNOWN),
	member(C, KNOWN).
card_possible(C) :-
	players(PS), member(P, PS), possible(P, POSSIBLE),
	member(C, POSSIBLE).

remove_of_same_type(C) :-
	weapons(TYPE), member(C, TYPE),
	inplay(CURRENT), subtract(CURRENT, TYPE, NEW),
	retract(inplay(CURRENT)), assert(inplay(NEW)).
remove_of_same_type(C) :-
	rooms(TYPE), member(C, TYPE),
	inplay(CURRENT), subtract(CURRENT, TYPE, NEW),
	retract(inplay(CURRENT)), assert(inplay(NEW)).
remove_of_same_type(C) :-
	suspects(TYPE), member(C, TYPE),
	inplay(CURRENT), subtract(CURRENT, TYPE, NEW),
	retract(inplay(CURRENT)), assert(inplay(NEW)).





unset_possible(P, CARDS) :-
	possible(P, CURRENT), subtract(CURRENT, CARDS, NEW), retract(possible(P, CURRENT)), 
	assert(possible(P, NEW)),
	assert(flag(1)); 
	true.

unset_possible_for_everyone(CARDS) :- 
	players(PLAYERS), member(P, PLAYERS), unset_possible(P, CARDS);
	true.

unset_possible_for_all_between(START, END, CARDS) :- 
	player_between(START, END, P), unset_possible(P, CARDS),
	fail; true.
player_between(START, END, P) :- 
	players(PLAYERS),
	append(_, [START|AFTER_S], PLAYERS), member(END, AFTER_S), 
	append(BEFORE_E, [END|_], PLAYERS), member(P, AFTER_S), member(P, BEFORE_E).
player_between(START, END, P) :- 
	players(PLAYERS),
	append(BEFORE_S, [START|AFTER_S], PLAYERS), member(END, BEFORE_S), 
	append(BEFORE_E, [END|_], PLAYERS), (member(P, AFTER_S); member(P, BEFORE_E)).

set_known(P, CARDS) :-
	known(P, KNOWN), 
	append(KNOWN, CARDS, NEW_KNOWN_UNSORTED), SORT(NEW_KNOWN_UNSORTED, NEW_KNOWN),
	retract(known(P, KNOWN)), assert(known(P, NEW_KNOWN)),
	unset_possible_for_everyone(CARDS),
	inplay(INPLAY), subtract(INPLAY, CARDS, NEW_INPLAY),
	retract(inplay(INPLAY)), assert(inplay(NEW_INPLAY)), 
	fail; true.




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

list_inplay :- 
	inplay(CARDS), write("In play:"), writeln(CARDS).

list_confirmed :- 
	solution(CARDS), write("Confirmed:"), writeln(CARDS).


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

