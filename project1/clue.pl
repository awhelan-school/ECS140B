:- use_module(library(apply)).

:- dynamic rooms/1.
:- dynamic weapons/1.
:- dynamic order/1.
:- dynamic cards/1.
:- dynamic suspects/1.
% player(X, [known cards], [possible cards]).
:- dynamic player/3.
:- dynamic weapons_base/1.
:- dynamic suspects_base/1.
:- dynamic rooms_base/1.

% Record all suggestions
% suggestion(PLAYER_NAME,[[Suggestion1], [Suggestion2]])
:- dynamic suggestions/2.

% This holds the difinitive 3 solution Cards
:- dynamic solutions/1.

% Inplay Helper predicate that returns all possible cards in single list_known
inplay(Cards) :- rooms(R), weapons(W), suspects(S),
                 append(R, W, RW), append(RW, S, Cards).

:- initialization(init).

is_member(_, Y) :-
  rooms_base(R), member(Y, R);
  weapons_base(W), member(Y, W);
  suspects_base(S), member(Y, S).

is_member2(_, Y) :-
  rooms_base(R), member(Y, R);
  weapons_base(W), member(Y, W);
  suspects_base(S), member(Y, S);
  order(O), member(Y, O).

showPlayers([]).
showPlayers([H|T]) :- player(H,Y,Z), writeln(H), writeln(Y), writeln(Z),
                      showPlayers(T).



init :-
  /*
  write("What are the room names?"),nl,
  readln(RoomList),
  assert(rooms(RoomList)),
  assert(rooms_base(RoomList)),

  write("What weapons were used?"),nl,
  readln(WeaponList),
  assert(weapons(WeaponList)),
  assert(weapons_base(WeaponList)),

  write("Who are the suspects?"),nl,
  readln(SuspectList),
  assert(suspects(SuspectList)),
  assert(suspects_base(SuspectList)),

  write("What is the order of the players?\n
  Denote yourself with an '*'.\n (i.e) {PersonA, PersonB, *, PersonC}\n"),nl,
  readln(PlayerList),
  assert(order(PlayerList)),

  write("What cards are you holding?"),nl,
  readln(CardList),
  assert(cards(CardList)),
  */

  assert(rooms(['R1','R2','R3','R4','R5','R6'])),
  assert(rooms_base(['R1','R2','R3','R4','R5','R6'])),
  assert(weapons(['W1','W2','W3','W4','W5','W6'])),
  assert(weapons_base(['W1','W2','W3','W4','W5','W6'])),
  assert(suspects(['S1','S2','S3','S4','S5','S6'])),
  assert(suspects_base(['S1','S2','S3','S4','S5','S6'])),
  assert(cards(['C1','C2','C3','C4'])),
  assert(order(['Alex','Ben', '*', 'Charlie'])),

  % Remove Starting Cards from Possible
  cards(C),

  rooms(R), weapons(W), suspects(S),
  subtract(R, C, RF), retract(rooms(R)), assert(rooms(RF)),
  subtract(W, C, WF), retract(weapons(W)), assert(weapons(WF)),
  subtract(S, C, SF), retract(suspects(S)), assert(suspects(SF)),

  % Init All Player Cards
  append(RF, WF, RFWF), append(RFWF, SF, All),
  order(O),initPlayers(O, [], All), initPlayers('*', C, []),
  prompt.

% Helper function
% Sample Usage: order(Players), member(Player, Players), control(Player),!.
control('*').

initPlayers([], [], _).
initPlayers([H|T], [], A) :- assert(player(H, [], A)), assert(suggestions(H, [])),
                             initPlayers(T, [], A).
initPlayers('*', C, _) :- retract(player('*', _, _)),
                          assert(player('*', C, [])).

showData :-
  write('Possible Rooms:'), rooms(R), writeln(R),
  write('Possible Weapons:'), weapons(W), writeln(W),
  write('Possible Suspects:'), suspects(S), writeln(S).


prompt :-
  % Call Anayze
  endgame;
  write("\n"),
  write("[1] Make a Suggestion\n"),
  write("[2] Observe a Suggestion\n"),
  write("[3] Show Possible Cards\n"),
  write("[4] Show Players\n"),
  write("[5] End Game\n"),
  write("Choice: "),nl,
  readln(Choice),member(C, Choice),nl,
  choice(C),
  prompt.


choice(5):- fail.
choice(4) :- order(O), showPlayers(O).
choice(3) :- showData.

% Observed Suggestion
% Format Q1 : [PLAYER, S1, S2, S3]
% Format Q2 : [PLAYER] | [No]
choice(2) :- write("What did you observe?\n"), readln(In),
             filterObservation(In, Out),
             write("Did anyone show a card? If not say no\n"),
             readln(In2), filterObservation(In2, Prover),
             recordSuggestion(Prover, Out).

% Player Suggestion
choice(1) :- write("What is your suggestion?\n"),readln(In),nl,
             filterSuggestion(In, Suggestion),
             write("Did you learn anything? If not say No.\n"),nl,
             readln(In2), parseSuggestion(In2, Suggestion, Proof), writeln(Proof),
             updateCards(Proof, Suggestion).

% If nobody showed a card
recordSuggestion([], [_|Cards]) :- updateCards([], Cards).

% Record Suggestion for Person who Proved
recordSuggestion(Player, [Suggestor|Cards]) :-

  member(Prover, Player),
  term_string(Prover, _),term_string(Suggestor, _),

  suggestions(Prover, X), append(X, [Cards], XT),
  retract(suggestions(Prover, _)),
  assert(suggestions(Prover, XT)),

  % Remove Cards in between if any
  removeCardsBetween(Suggestor, Prover, Cards), fail;true.


% If Nothing Proven
updateCards([], SuggestedCards) :-
  player(Other,_,_), removePossibleCards(Other, SuggestedCards), fail; true,
  removeGlobalCards(SuggestedCards).

updateCards([Person|Card], SuggestedCards) :-
  % If suggestion is proven remove from global possabilities
  removeGlobalCards(Card),

  addKnownCards(Person,Card),
  removeCardsBetween(*, Person, SuggestedCards),fail;true.


removeGlobalCards(Card) :- rooms(R), weapons(W), suspects(S),
  subtract(R, Card, RF), retract(rooms(R)), assert(rooms(RF)),
  subtract(W, Card, WF), retract(weapons(W)), assert(weapons(WF)),
  subtract(S, Card, SF), retract(suspects(S)), assert(suspects(SF)).

addKnownCards(Person, Card) :-
  % Need to get name as "Name" instead of Name
  term_string(Person, _),
  % Update Person Who Shows you a card
  player(Person, Known, Possible),
  % Update Their Known Cards
  append(Known, Card, NewKnown),
  % Update the remaining possible cards
  subtract(Possible, NewKnown, NewPossible),
  % Reassert the person
  retract(player(Person, Known, Possible)),
  assert(player(Person, NewKnown, NewPossible)),
  % Also we can remove known card from all other possabilities
  player(Others, _, _), removePossibleCards(Others, Card),fail;true.

removePossibleCards(Person, SuggestedCards) :-
  % Need to get name as "Name" instead of Name
  term_string(Person, _),
  % Get Person
  player(Person, Known, Possible),
  % Remove Cards
  subtract(Possible, SuggestedCards, NewPossible),
  % Reassert the person
  retract(player(Person, _, _)),
  assert(player(Person, Known, NewPossible)).

removeCardsBetween(Person1, Person2, Cards) :-
  % Update Players in Between (i.e. remove possible cards)
  player_between(Person1, Person2, PersonBW),term_string(PersonBW, _),
  removePossibleCards(PersonBW, Cards).


parseSuggestion(In, S, Proof) :- filterObservation(In, Proof).

filterSuggestion(In, Out) :- include(is_member(_), In, Out).
filterObservation(In, Out) :- include(is_member2(_), In, Out).


%% return players between START and END,
%% if END is after START, return players that are after START AND before END
player_between(START, END, P) :- order(PLAYERS),
	append(_, [START|AFTER_S], PLAYERS), member(END, AFTER_S),
	append(BEFORE_E, [END|_], PLAYERS), member(P, AFTER_S), member(P, BEFORE_E).
%% if END is before END, return players after START OR before END
player_between(START, END, P) :- order(PLAYERS),
	append(BEFORE_S, [START|AFTER_S], PLAYERS), member(END, BEFORE_S),
	append(BEFORE_E, [END|_], PLAYERS), (member(P, AFTER_S); member(P, BEFORE_E)).

endgame :- rooms(R), suspects(S), weapons(W),
           length(R, RL),length(S, SL),length(W, WL),
           RL == 1, SL == 1, WL == 1,
           format("Hey, Kurt, all that's left is ~w, ~w, and the ~w it's time to make a choice!\n",
           [S,W,R]),
           halt.

endgame([S,W,R]) :- format("Hey, Kurt, all that's left is ~w, ~w, and the ~w it's time to make a choice!\n",
                            [S,W,R]),
                            halt.
