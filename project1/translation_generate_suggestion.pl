/*
Changed these:

weapons, suspects, rooms 
-> 
weapons_base, suspects_base, rooms_base


Need to figure out:

control(P)
true if P is the controlling player, player who is using the program

inplay(INPLAY)
list of cards still in play
*/

generate_suggestion(BEST_SUGGESTION) :-
	setof(S, possible_suggestion(S), ALL),
	best_suggestion(ALL, BEST_SUGGESTION).   

possible_suggestion([S, W, R]) :-
	weapons_base(WS), suspects_base(SS), rooms_base(RS),
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