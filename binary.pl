add(o,X,X).
add(pos(X),o,pos(X)).
add(pos(X),pos(Y),pos(Z)) :- lessP(pos(X),pos(Z)), lessP(pos(Y), pos(Z)), addP(X,Y,Z).

addP(i,i,o(i)).
addP(i,o(N),i(N)).
addP(i,i(N),o(M)) :- addP(i,N,M).
addP(o(N),i,i(N)).
addP(i(N),i,o(M)) :- addP(i,N,M).
addP(o(N),o(M),o(Res)) :- addP(N,M,Res).
addP(i(N),o(M),i(Res)) :- addP(N,M,Res).
addP(o(N),i(M),i(Res)) :- addP(N,M,Res).
addP(i(N),i(M),o(Res)) :- addP(N,M,Res1), addP(i,Res1,Res).

lessP(pos(i),pos(o(_))).
lessP(pos(i),pos(i(_))).
lessP(pos(o(N)),pos(i(N))).
lessP(pos(o(N)),pos(o(M))) :- lessP(pos(N),pos(M)).
lessP(pos(i(N)),pos(o(M))) :- lessP(pos(N),pos(M)).
lessP(pos(i(N)),pos(i(M))) :- lessP(pos(N),pos(M)).
lessP(pos(o(N)),pos(i(M))) :- lessP(pos(N),pos(M)).

