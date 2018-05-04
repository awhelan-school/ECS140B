/*

Changed these:

players()
->
order()

proofs()
->
suggestions()

Need to figure out:


*/


%% call this when a suggestion happens and it is not proven by anyone
%% every player but SUGGESTER can't have suggested cards,
unproven(SUGGESTER, CARDS) :- 
	order(PLAYERS), member(P, PLAYERS), not(P = SUGGESTER), 
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
	suggestions(P, CURRENT), not(member(PROOF, CURRENT)), 
	append(CURRENT, [PROOF], NEW), 
	retract(suggestions(P, CURRENT)), assert(suggestions(P, NEW)), 
	fail; true.

%% same as other one, only we are making the suggestion 
%% and therefore see which card the prover showed
proven_to_me(SUGGESTER, PROVER, CARDS, SHOWN_CARD) :-
	set_known(PROVER, [SHOWN_CARD]),
	unset_possible_for_all_between(SUGGESTER, PROVER, CARDS),
	check_for_solution(), 
	analyze_proofs_all(), 
	fail; true.