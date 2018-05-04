::- use_module(library(apply)).

rooms(['R1', 'R2', 'R3']).
weapons(['W1', 'W2', 'W3']).
suspects(['S1', 'S2', 'S3']).


is_member(X, Y) :-
  rooms(R), member(Y, R);
  weapons(W), member(Y, W);
  suspects(S), member(Y, S).

test :-
  write("Make a prediction!\n"), readln(In),
  include(is_member(X), In, Out), writeln(Out).
