:- use_module(library(apply)).

:- dynamic rooms/1.
:- dynamic weapons/1.
:- dynamic order/1.
:- dynamic cards/1.
:- dynamic suspects/1.
:- dynamic player/3.
:- dynamic weapons_base/1.
:- dynamic suspects_base/1.
:- dynamic rooms_base/1.

is_member(X, Y) :-
  rooms_base(R), member(Y, R);
  weapons_base(W), member(Y, W);
  suspects_base(S), member(Y, S).

is_member2(X, Y) :-
  rooms_base(R), member(Y, R);
  weapons_base(W), member(Y, W);
  suspects_base(S), member(Y, S);
  order(O), member(Y, O).


% player(X, [known cards], [possible cards]).

:- initialization(init).

init :-
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

  write("What is the order of the players?"),nl,
  readln(PlayerList),
  assert(order(PlayerList)),

  write("What cards are you holding?"),nl,
  readln(CardList),
  assert(cards(CardList)),

  cards(C),
  rooms(R), subtract(R, C, RF), retract(rooms(R)), assert(rooms(RF)),
  weapons(W), subtract(W, C, WF), retract(weapons(W)), assert(weapons(WF)),
  suspects(S), subtract(S, C, SF), retract(suspects(S)), assert(suspects(SF)),

  % Init All Player Cards
  append(RF, WF, RFWF), append(RFWF, SF, All),
  order(O),initPlayers(O, [], All),
  prompt.

initPlayers([], [], _).
initPlayers([H|T], [], A) :- assert(player(H, [], A)), initPlayers(T, [], A).


showData :-
  write('Possible Rooms:'), rooms(R), writeln(R),
  write('Possible Weapons:'), weapons(W), writeln(W),
  write('Possible Suspects:'), suspects(S), writeln(S).


prompt :-
  endgame;
  write("\n\n\n\n"),
  write("[1] Make a Suggestion\n"),
  write("[2] Observe a Suggestion\n"),
  write("[3] Show Data\n"),
  write("[4] End Game\n"),
  write("Choice: "),nl,
  readln(Choice),member(C, Choice),nl,
  choice(C),
  prompt.

choice(4) :- .
choice(3) :- showData.
choice(2) :- write("What did you observe?\n"), readln(In),
             filterObservation(In, Out), writeln(Out).
choice(1) :- write("What is your suggestion?\n"),readln(In),nl,
             filterSuggestion(In, Suggestion),writeln(Suggestion),
             write("Did you learn anything? If not say No.\n"),nl,
             readln(In2), parseSuggestion(In2, Proof), updateCards(Proof).

updateCards([Person|Card]) :- rooms(R), weapons(W), suspects(S),
                  subtract(R, Card, RF), retract(rooms(R)), assert(rooms(RF)),
                  subtract(W, Card, WF), retract(weapons(W)), assert(weapons(WF)),
                  subtract(S, Card, SF), retract(suspects(S)), assert(suspects(SF)),
                  term_string(Person, P),
                  % Update Person Who Shows you a card
                  player(Person, Known, Possible),
                  % Update Their Known Cards
                  append(Known, Card, NewKnown),
                  % Update the remaining possible cards
                  subtract(Possible, NewKnown, NewPossible),
                  % Reassert the person
                  retract(player(Person, _, _)),
                  assert(player(Person, NewKnown, NewPossible)).


parseSuggestion(In)   :- writeln("YAY you win the game").
parseSuggestion([no]) :- writeln("YAY you win the game").
parseSuggestion([N])  :- writeln("YAY you win the game").
parseSuggestion([n])  :- writeln("YAY you win the game").

parseSuggestion(In, Proof) :- filterObservation(In, Proof).

filterSuggestion(In, Out) :- include(is_member(X), In, Out).
filterObservation(In, Out) :- include(is_member2(X), In, Out).

endgame :- rooms(R), suspects(S), weapons(W),
           length(R, RL),length(S, SL),length(W, WL),
           RL == 1, SL == 1, WL == 1,
           format("Hey, Kurt, all that's left is ~w, ~w, and the ~w it's time to make a choice!\n",
           [S,W,R]),
           halt.
