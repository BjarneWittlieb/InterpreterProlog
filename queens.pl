% Peano representation of natural numbers:
% 0 is a number (Prolog: o)
% if n is a number, then s(n) is a number
% represent 3 by s(s(s(o)))

isPeano(o).
isPeano(s(N)) :- isPeano(N).

% add(X,Y,Z) <=> Z is the sum of X and Y
add(o   ,Y,Y).
add(s(X),Y,s(Z)) :- add(X,Y,Z).

% sub(X,Y,Z) <=> Z is the difference of X and Y
sub(X,Y,Z) :- add(Y,Z,X).

% mult(X,Y,Z) <=> Z=X*Y
mult(o   ,_,o).
mult(s(X),Y,Z) :- mult(X,Y,XY), add(XY,Y,Z).

% leq(X,Y) <=> X<=Y
leq(o,_).
leq(s(X),s(Y)) :- leq(X,Y).
