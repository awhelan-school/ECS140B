
:- dynamic rooms/1.
:- dynamic weapons/1.
:- dynamic order/1.
:- dynamic cards/1.
:- dynamic suspects/1.
:- dynamic player/3.


% player(X, [known cards], [possible cards]).

:- initialization(init).

init :-
  write("What are the room names?"),nl,
  readln(RoomList),
  assert(rooms(RoomList)),

  write("What weapons were used?"),nl,
  readln(WeaponList),
  assert(weapons(WeaponList)),

  write("Who are the suspects?"),nl,
  readln(SuspectList),
  assert(suspects(SuspectList)),

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
  order(O),initPlayers(O, [], All).
  % prompt.

initPlayers([], [], _).
initPlayers([H|T], [], A) :- assert(player(H, [], A)), initPlayers(T, [], A).


showData :-
  write('Possible Rooms:'), rooms(R), writeln(R),
  write('Possible Weapons:'), weapons(W), writeln(W),
  write('Possible Suspects:'), suspects(S), writeln(S).

/*
prompt :-
  endgame;
  write("\n\n\n\n"),
  write("[1] Make a Suggestion\n"),
  write("[2] Observe a Sugstion\n"),
  write("[3] Show Data\n"),
  write("[4] End Game\n"),
  write("\n\n Choice ==> "),
  readln(Choice),member(C, Choice),
  choice(C),
  prompt.

choice(4) :- halt.
choice(3) :- showData.
choice(2) :- write("Say PX SX WX RX\n").
choice(1) :- write("Say Sx Wx Rx\n").

endgame :- rooms(R), suspects(S), weapons(W),
           length(R, RL),length(S, SL),length(W, WL),
           RL == 1, SL == 1, WL == 1,
           format("Hey, Kurt, all that's left is ~w, ~w, and the ~w it's time to make a choice!\n",
           [S,W,R]),
           halt.
*/
