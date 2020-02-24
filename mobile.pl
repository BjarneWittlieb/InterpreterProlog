add(o   ,Y,Y).
add(s(X),Y,s(Z)) :- add(X,Y,Z).

mobile(fisch(X),X).
mobile(bruecke(X,Y),Z) :- add(s(o),NN,Z), add(N,N,NN), mobile(X,N), mobile(Y,N).
