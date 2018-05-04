/*
Changed these:

players()
->
order()

known(P, X)
->
players(P, X, _)

possible(P, X)
->
players(P, _, X)

Need to figure out:

need flag() predicate, flag(1) by default

*/


%% analyze proofs that players did to maybe deduce which cards they have
:- dynamic flag/1.
flag(1).

analyze_proofs_all() :-
	writeln("analyzing"),
	retractall(flag(_)),
	analyze_proofs_all_each(),
	flag(1), analyze_proofs_all().

analyze_proofs_all_each() :-
	order(PLAYERS), member(P, PLAYERS), 
	analyze_proofs(P), fail; true. 

analyze_proofs(P) :-
	setof(CARDS, possible_hand(P, CARDS), ALL), 
	not(length(ALL, 0)), intersection_of_sets(ALL, INTERSECTION),
	not(length(INTERSECTION, 0)),
	players(P, CURRENT, _), not(contains(CURRENT, INTERSECTION)), %% need to check if intersection is already member of known, if it is, don't call set_known again
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
	players(P, KNOWN, POSSIBLE), 
	(member(H, KNOWN); member(H, POSSIBLE)),
	all_possible_or_known(P, T).

proves_all_proofs(P, CARDS) :-
	proofs(P, PROOFS), proves_all_proofs_helper(P, CARDS, PROOFS).
proves_all_proofs_helper(_, _, []).
proves_all_proofs_helper(P, CARDS, [H|T]) :-
	member(C, CARDS), member(C, H),
	proves_all_proofs_helper(P, CARDS, T).