% alle Kandidaten
kandidat(malfoy).
kandidat(potter).
kandidat(granger).
kandidat(weasley).

% Mögliche-Vorstände-Relation mit Vorsitzendem, Kassenwart und Sekretär
% Sicher stellen dass alle Vorstandsmitglieder auch Kanditaten sind (Suchbereich), alle Mitglieder member sind und kein mitglied doppelt vorkommt
possibleVorstand(V, K, S) :- kandidat(V), kandidat(K), kandidat(S), notEqual(V, K), notEqual(V, S), notEqual(K, S).

% eine NotEqual Relation.
notEqual(X, Y) :- kandidat(X), kandidat(Y), \=(X, Y).

% Hilfsrelation member
member(P, [S|_]) :- =(P, S).
member(P, [_|L]) :- member(P, L).

% Restrictions aus der Aufgabenstellung:
% Potter und Malfoy follen nicht gemeinsam in den Vorstand:
malfPottVorstand(V, K, S) :- inVorstand(V, K, S, granger), inVorstand(V, K, S, weasley).

inVorstand(V, _, _, X) :- =(V, X).
inVorstand(_, K, _, X) :- =(K, X).
inVorstand(_, _, S, X) :- (S, X).

% Malfoy ist nur im Vorstand, wenn granger vorsitzende ist (das ist aber nicht gerade lore):
malfGrangerVorstand(granger, malfoy, _).
malfGrangerVorstand(granger, _, malfoy).
malfGrangerVorstand(V, K, S) :- notEqual(malfoy, V), notEqual(malfoy, K), notEqual(malfoy, S).

% Weasly ist nur im Vorstand wenn auch Potter im Vorstand ist:
weaslVorstand(V, K, S) :- inVorstand(V, K, S, potter), inVorstand(V, K, S, weasley).
weaslVorstand(V, K, S) :- notEqual(V, weasley), notEqual(K, weasley), notEqual(S, weasley).

% Potter will nicht in den Vorstand wenn Granger nur Sekretärin ist:
pottGranVorstand(_, _, S) :- notEqual(S, granger).
pottGranVorstand(V, K, granger) :- notEqual(V, potter), notEqual(K, potter).

% Granger ist nur im Vorstand wenn Weasly kein Vorsitzender ist
granWeaslVorstand(V, _, _) :- notEqual(V, weasley).
granWeaslVorstand(weasley, K, S) :- notEqual(K, granger), notEqual(S, granger).

% Kombiniere alle Eigenschaften:
vorstand(V, K, S) :- possibleVorstand(V, K, S), malfPottVorstand(V, K, S), malfGrangerVorstand(V, K, S), weaslVorstand(V, K, S), pottGranVorstand(V, K, S), granWeaslVorstand(V, K, S).