
:- dynamic rooms/1.
:- dynamic weapons/1.
:- dynamic players/1.
:- dynamic cards/1.
:- dynamic suspects/1.

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
  assert(players(PlayerList)),

  write("What cards are you holding?"),nl,
  readln(CardList),
  assert(cards(CardList)),

  cards(C),
  rooms(R), subtract(R, C, RF), retract(rooms(R)), assert(rooms(RF)),
  weapons(W), subtract(W, C, WF), retract(weapons(W)), assert(weapons(WF)),
  suspects(S), subtract(S, C, SF), retract(suspects(S)), assert(suspects(SF)).

showData :-
  write('Possible Rooms:'), rooms(R), writeln(R),
  write('Possible Weapons:'), weapons(W), writeln(W),
  write('Possible Suspects:'), suspects(S), writeln(S).

:- initialization(init).
