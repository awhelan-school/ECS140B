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

inplay(INPLAY)
list of cards still in play
*/

unset_possible(P, CARDS) :-
	players(P, KNOWN, POSSIBLE), subtract(POSSIBLE, CARDS, NEW), 
	retract(players(P, KNOWN, POSSIBLE)), 
	assert(players(P, KNOWN, NEW)),
	assert(flag(1)); 
	true.

unset_possible_for_everyone(CARDS) :- 
	order(PLAYERS), member(P, PLAYERS), 
	unset_possible(P, CARDS);
	true.

unset_possible_for_all_between(START, END, CARDS) :- 
	player_between(START, END, P), unset_possible(P, CARDS),
	fail; true.
player_between(START, END, P) :- 
	order(PLAYERS),
	append(_, [START|AFTER_S], PLAYERS), member(END, AFTER_S), 
	append(BEFORE_E, [END|_], PLAYERS), member(P, AFTER_S), member(P, BEFORE_E).
player_between(START, END, P) :- 
	order(PLAYERS),
	append(BEFORE_S, [START|AFTER_S], PLAYERS), 
	member(END, BEFORE_S), 
	append(BEFORE_E, [END|_], PLAYERS), 
	(member(P, AFTER_S); member(P, BEFORE_E)).

set_known(P, CARDS) :-
	players(P, KNOWN, POSSIBLE), 
	append(KNOWN, CARDS, NEW_KNOWN_UNSORTED), 
	SORT(NEW_KNOWN_UNSORTED, NEW_KNOWN),
	retract(players(P, KNOWN, POSSIBLE)), 
	assert(players(P, NEW_KNOWN, POSSIBLE)),
	unset_possible_for_everyone(CARDS),
	inplay(INPLAY), subtract(INPLAY, CARDS, NEW_INPLAY),
	retract(inplay(INPLAY)), assert(inplay(NEW_INPLAY)), 
	fail; true.