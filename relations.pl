% Aufgabe 1


% Definition der Ehefrau-Ehemann-Relation:
ehemann(christine, heinz).
ehemann(maria,     fritz).
ehemann(monika,    herbert).
ehemann(angelika,  hubert).
ehemann(claudia,   karl).

% Definition der Kind-Mutter-Relation:
mutter(herbert,  christine).
mutter(angelika, christine).
mutter(hubert,   maria).
mutter(karl,     maria).
mutter(susanne,  monika).
mutter(norbert,  monika).
mutter(andreas,  angelika).
mutter(anna,     claudia).

vater(Kind,Vater) :- ehemann(Mutter,Vater), mutter(Kind,Mutter).

grossvater(E,G) :- vater(E,V), vater(V,G).
grossvater(E,G) :- mutter(E,M), vater(M,G).

% Eigene Definitionen
% Definition der Großmutter Relation
grossmutter(E, G) :- mutter(E, M), mutter(M, G).
grossmutter(E, G) :- vater(E, V), mutter(V, G).

% Definition der Geschwister Relation
geschwister(P, G) :- mutter(P, M), mutter(G, M), vater(P, V), vater(G, V), P \= G.

% Relation für Tante oder Onkel sein. Inklusive angeheiratet
% Das funktioniert, da diejenigen die keine Großmutter haben auch keine Tante haben (da sie nicht über die Mutter verbunden sind).
tanteOnkel(P, T) :- grossmutter(P, G), mutter(T, G).
tanteOnkel(P, T) :- grossmutter(P, G), mutter(M, G), ehemann(T, M).
tanteOnkel(P, T) :- grossmutter(P, G), mutter(V, G), ehemann(V, T).

% Negation ist nicht wirklich möglich weshalb die geschwister Relation immer reflexiv und ist und die Mutter auch immer eine Tante ist.

% Relation für alle weiblichen Menschen
weiblich(W) :- ehemann(W, _).
weiblich(susanne).
weiblich(anna).

% Relation für Tanten
tante(P, T) :- tanteOnkel(P, T), weiblich(T).