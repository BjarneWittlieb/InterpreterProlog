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

% gint alle Möglichkeiten an, N Damen auf einem N x N Schachbrett zu positionieren, sodass sich keine Damen schlagen können
queens(N,L) :- permutations(N,L,N), allSafe(L).

% gibt alle Permutationen der Länge M an, vorausgesetzt das erste und dritte Argument ist gleich
permutations(o,[],_).
permutations(s(N),[L|Ls],M) :- leq(s(o),L), leq(L,M), permutations(N,Ls,M), different(L,Ls).

% X is ungleich jede Element in der Liste
different(_,[]).
different(X,[Y|Ys]) :- X \= Y, different(X,Ys).

allSafe([]).
allSafe([Q|Qs]) :- safe(Q,Qs,s(o)), allSafe(Qs).

% P ist der Linienabstand von Q und Q1
safe(_, []     , _).
safe(Q, [Q1|Qs], P) :- differentDiags(Q, Q1, P), safe(Q, Qs, s(P)).

% Unterschiedliche Diagonalen?
differentDiags(Q, Q1, P) :-
  add(Q1, P, Q1PP), Q \= Q1PP, % unterschiedliche Diagonale \
  add(Q , P, QPP ), QPP \= Q1. % unterschiedliche Diagonale /
  
% N = 1: 1 Lösung
% N = 2: 0 Lösungen
% N = 3: 0 Lösungen
% N = 4: 2 Lösungen
% N = 5: 10 Lösungen
% N = 6: 4 Lösungen