/*
Changed these:

weapons, suspects, rooms 
-> 
weapons_base, suspects_base, rooms_base

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

solution()
need a predicate for solution list
starts as empty

inplay(INPLAY)
list of cards still in play

*/


check_if_card_is_solution(C) :-
	not(card_known(C)), not(card_possible(C)),
	solution(CURRENT), not(member(C, CURRENT)),
	append(CURRENT, [C], NEW), 
	retract(solution(CURRENT)), assert(solution(NEW)),
	writeln("Added card to solution list:"), writeln(C),  
	remove_of_same_type(C), %% if a card of type is in solution, all other cards of same type are out of play
	fail; true.

card_known(C) :-
	order(PS), member(P, PS), players(P, KNOWN, _),
	member(C, KNOWN).
card_possible(C) :-
	order(PS), member(P, PS), players(P, _, POSSIBLE),
	member(C, POSSIBLE).

remove_of_same_type(C) :-
	weapons_base(TYPE), member(C, TYPE),
	inplay(CURRENT), subtract(CURRENT, TYPE, NEW),
	retract(inplay(CURRENT)), assert(inplay(NEW)).
remove_of_same_type(C) :-
	rooms_base(TYPE), member(C, TYPE),
	inplay(CURRENT), subtract(CURRENT, TYPE, NEW),
	retract(inplay(CURRENT)), assert(inplay(NEW)).
remove_of_same_type(C) :-
	suspects_base(TYPE), member(C, TYPE),
	inplay(CURRENT), subtract(CURRENT, TYPE, NEW),
	retract(inplay(CURRENT)), assert(inplay(NEW)).

