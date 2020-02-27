{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck
import Data.List

import SLDResolution
import Substitutions
import Type
import Prettyprinting
import Parser
import Prettyprinting
import Unification
import Vars

-- This is just useful
instance Parse Term where
    parse string = case parse (string ++ ".") :: Either String Goal of
        Right (Goal [term]) -> Right term
        Left str -> Left str
        _ -> Left "Something went horribly wrong here."


-- Fast helper Functions for parsing (which are just more convinient if you know what you are doing)
fromString :: String -> Goal
fromString s = case parse s of
    Right g -> g
    _       -> error "Parse error!"

substFromStrings :: [String] -> Subst
substFromStrings strList = Subst (fmap tupleFromString strList) where
    tupleFromString:: String -> (VarName, Term)
    tupleFromString str = let tuple = strSplit "->" str in
        case parse (strTrim (snd tuple)) of
            Right term  -> (strTrim (fst tuple), term)
            _           -> error "Parse error!"


-- String helper Functions for the Substitutions pseudo parser
strBreak :: Char -> String -> (String , String)
strBreak c toSplit = (span (/= c) toSplit)

strSplit :: String -> String -> (String , String)
strSplit splitter toSplit = case stripPrefix splitter (snd break) of
    Just str -> (fst break, str)
    _ -> (toSplit, "")
    where
        break = strBreak (splitter!!0) toSplit

strTrim :: String -> String
strTrim "" = ""
strTrim (' ':str) = strTrim str
strTrim (c:str) = c:(strTrim str)



instance Eq Subst where
    s1 == s2 = (pretty s1) == (pretty s2)

normify :: [VarName] -> Subst -> Subst
normify vs s = let s' = simplify vs (repeatSubst s) in order2 (renameSubst vs (order2 (applySub (order1 vs s') s'))) where 
  order1 :: [VarName] -> Subst -> Subst
  order1 v (Subst []) = empty
  order1 v' (Subst ((v, Var w):xs)) | (elem w v') && v > w = Subst ((v, Var w):((w, Var v):(fromSubst (order1 v' (Subst xs)))))
  order1 v (Subst (x:xs)) = order1 v (Subst xs)
  order2 :: Subst -> Subst
  order2 (Subst []) = empty
  order2 (Subst ((v, t):xs)) = Subst ((fromSubst (order2 (Subst (filter (\(x, _) -> x < v) xs)))) ++
   (v, t):(fromSubst (order2 (Subst (filter (\(x, _) -> x > v) xs)))))

eqSubsts :: Goal -> [Subst] -> [Subst] -> Bool
eqSubsts g s1 s2 = let vs = allVars g in
 (length s1 == length s2) && foldr (&&) True (fmap (\(x, y) -> (normify vs (Subst x)) == (normify vs (Subst y))) (zip (fmap (fromSubst) s1) (fmap (fromSubst) s2)))


-- Helper Functions for certain solution testing
testForSolution :: Prog -> Goal -> Strategy -> [Subst] -> Bool
testForSolution p g strat subs = eqSubsts g (solve strat p g) subs

testForSoltionLength :: Prog -> Goal -> Strategy -> Int -> Bool
testForSoltionLength p g strat l = (length (solve strat p g)) == l

testNoSolution :: Prog -> Goal -> Strategy -> Bool
testNoSolution p g strat = case solve strat p g of
    [] -> True
    _ -> False

testIfEmpty :: Prog -> Goal -> Strategy -> Bool
testIfEmpty p g strat = case solve strat p g of
    [Subst []] -> True
    _ -> False

{-

Testing functions for the unification algorithm.

 -}

twoVarsTerm = fromString "=(A, B)."
unify_twoVars :: Strategy -> Bool
unify_twoVars strat = case solve strat (Prog []) twoVarsTerm of
    [Subst [("A", (Var "B"))]] -> True
    [Subst [("B", (Var "A"))]] -> True
    _ -> False
prop_dfs_unify_twoVars = unify_twoVars dfs
prop_bfs_unify_twoVars = unify_twoVars bfs
prop_idfs_unify_twoVars = unify_twoVars idfs

occurTerm = fromString "=(A, f(A))."
prop_dfs_unify_occur    = testNoSolution (Prog []) occurTerm dfs
prop_bfs_unify_occur    = testNoSolution (Prog []) occurTerm bfs
prop_idfs_unify_occur   = testNoSolution (Prog []) occurTerm idfs

firstEmpty = fromString "=(_, A)."
prop_dfs_unify_anonym1 = testIfEmpty (Prog []) firstEmpty dfs
prop_bfs_unify_anonym1 = testIfEmpty (Prog []) firstEmpty bfs
prop_idfs_unify_anonym1 = testIfEmpty (Prog []) firstEmpty idfs
secondEmpty = fromString "=(A, _)."
prop_dfs_unify_anonym2 = testIfEmpty (Prog []) secondEmpty dfs
prop_bfs_unify_anonym2 = testIfEmpty (Prog []) secondEmpty bfs
prop_idfs_unify_anonym2 = testIfEmpty (Prog []) secondEmpty idfs
bothEmpty = fromString "=(_,_)."
prop_dfs_unify_anonym3 = testIfEmpty (Prog []) bothEmpty dfs
prop_bfs_unify_anonym3 = testIfEmpty (Prog []) bothEmpty bfs
prop_idfs_unify_anonym3 = testIfEmpty (Prog []) bothEmpty idfs

multSubs = fromString "=(f(A,B),f(f(C),g(D)))."
expectedMultSubs = [substFromStrings ["A -> f(C)", "B -> g(D)"]]
prop_dfs_unify_multiplesubs1 = testForSolution (Prog []) multSubs dfs expectedMultSubs
prop_bfs_unify_multiplesubs1 = testForSolution (Prog []) multSubs bfs expectedMultSubs
prop_idfs_unify_multiplesubs1 = testForSolution (Prog []) multSubs idfs expectedMultSubs

multSubsEmpty = fromString "=(f(_,_),f(f(C),g(D)))."
prop_dfs_unify_multiplesubs_empty = testIfEmpty (Prog []) multSubsEmpty dfs
prop_bfs_unify_multiplesubs_empty = testIfEmpty (Prog []) multSubsEmpty bfs
prop_idfs_unify_multiplesubs_empty = testIfEmpty (Prog []) multSubsEmpty idfs

multSubsDeeper = fromString "=(f(A,B),f(f(C),g(A)))."
expectedMultSubsDeeper  = [substFromStrings ["A -> f(C)", "B -> g(f(C))"]]
prop_dfs_unify_multiplesubs2 = testForSolution (Prog []) multSubsDeeper dfs expectedMultSubsDeeper
prop_bfs_unify_multiplesubs2 = testForSolution (Prog []) multSubsDeeper bfs expectedMultSubsDeeper
prop_idfs_unify_multiplesubs2 = testForSolution (Prog []) multSubsDeeper idfs expectedMultSubsDeeper

noSolution1 = fromString "=(f(A,B),g(C,D))."
prop_dfs_unify_nosolution1 = testNoSolution (Prog []) noSolution1 dfs
prop_bfs_unify_nosolution1 = testNoSolution (Prog []) noSolution1 bfs
prop_idfs_unify_nosolution1 = testNoSolution (Prog []) noSolution1 idfs

noSolution2 = fromString "=(f(A,B),f(C,D,E))."
prop_dfs_unify_nosolution2 = testNoSolution (Prog []) noSolution2 dfs
prop_bfs_unify_nosolution2 = testNoSolution (Prog []) noSolution2 bfs
prop_idfs_unify_nosolution2 = testNoSolution (Prog []) noSolution2 idfs

complicated1 = fromString "=(p(A,B,C,D,E,F,G,H,I,J,K,L,M),p(f(B,B),f(C,C),f(D,D),f(E,E),f(F,F),f(G,G),f(H,H),f(I,I),f(J,J),f(K,K),f(L,L),f(M,M),f(N,N)))."
prop_dfs_unify_complicated1 = testForSoltionLength (Prog []) complicated1 dfs 1
prop_bfs_unify_complicated1 = testForSoltionLength (Prog []) complicated1 bfs 1
prop_idfs_unify_complicated1 = testForSoltionLength (Prog []) complicated1 idfs 1

complicated2 = fromString "=(p(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R),p(f(B,B),f(C,C),f(D,D),f(E,E),f(F,F),f(G,G),f(H,H),f(I,I),f(J,J),f(K,K),f(L,L),f(M,M),f(N,N),f(O,O),f(P,P),f(Q,Q),f(R,R),f(S,S)))."
prop_dfs_unify_complicated2 = testForSoltionLength (Prog []) complicated2 dfs 1
--prop_bfs_unify_complicated2 = testForSoltionLength (Prog []) complicated2 bfs 1
--prop_idfs_unify_complicated2 = testForSoltionLength (Prog []) complicated2 idfs 1

-- This takes far too long
-- complicated3 = fromString "=(p(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y),p(f(B,B),f(C,C),f(D,D),f(E,E),f(F,F),f(G,G),f(H,H),f(I,I),f(J,J),f(K,K),f(L,L),f(M,M),f(N,N),f(O,O),f(P,P),f(Q,Q),f(R,R),f(S,S),f(T,T),f(U,U),f(V,V),f(W,W),f(X,X),f(Y,Y),f(Z,Z)))."
-- prop_dfs_unify_complicated3 = testForSoltionLength (Prog []) complicated3 dfs 1
-- prop_bfs_unify_complicated3 = testForSoltionLength (Prog []) complicated3 bfs 1
-- prop_idfs_unify_complicated3 = testForSoltionLength (Prog []) complicated3 idfs 1


{-

Testing 3 cases for ananomys cariables

-}
anonymousProgram :: Prog
anonymousProgram = case parse "p(A, B) :- a(A), b(B).\nq :- p(_, _).\na(a).\nb(b)." of
    Right p -> p
    _       -> Prog []

anonymousSolution = fromString "p(A,B)."
expectedAnanSol = [substFromStrings ["A -> a", "B -> b"]]
prop_dfs_anonymous_solution = testForSolution anonymousProgram anonymousSolution dfs expectedAnanSol
prop_bfs_anonymous_solution = testForSolution anonymousProgram anonymousSolution bfs expectedAnanSol
prop_idfs_anonymous_solution = testForSolution anonymousProgram anonymousSolution idfs expectedAnanSol

anonymousBlank1 = fromString "p(_,_)."
prop_dfs_anonymous_blank1 = testIfEmpty anonymousProgram anonymousBlank1 dfs
prop_bfs_anonymous_blank1 = testIfEmpty anonymousProgram anonymousBlank1 bfs
prop_idfs_anonymous_blank1 = testIfEmpty anonymousProgram anonymousBlank1 idfs

anonymousBlank2 = fromString "q."
prop_dfs_anonymous_blank2 = testIfEmpty anonymousProgram anonymousBlank2 dfs
prop_bfs_anonymous_blank2 = testIfEmpty anonymousProgram anonymousBlank2 bfs
prop_idfs_anonymous_blank2 = testIfEmpty anonymousProgram anonymousBlank2 idfs

{-

Testing the arithmecy functions.

-}

arithmecyProgram :: Prog
arithmecyProgram = case parse "factorial(0, 1).\nfactorial(N, F) :- >(N, 0), is(N1, -(N, 1)), factorial(N1, F1), is(F, *(F1, N))." of
    Right p -> p
    _       -> Prog []

is1 = fromString "is(42,+(21,21))."
prop_dfs_arithmecy_is1 = testIfEmpty arithmecyProgram is1 dfs
prop_bfs_arithmecy_is1 = testIfEmpty arithmecyProgram is1 bfs
prop_idfs_arithmecy_is1 = testIfEmpty arithmecyProgram is1 idfs

is2 = fromString "is(+(21,21),42)."
prop_dfs_arithmecy_is2 = testNoSolution arithmecyProgram is2 dfs
prop_bfs_arithmecy_is2 = testNoSolution arithmecyProgram is2 bfs
prop_idfs_arithmecy_is2 = testNoSolution arithmecyProgram is2 idfs

is3 = fromString "is(+(21,21),+(21,21))."
prop_dfs_arithmecy_is3 = testNoSolution arithmecyProgram is3 dfs
prop_bfs_arithmecy_is3 = testNoSolution arithmecyProgram is3 bfs
prop_idfs_arithmecy_is3 = testNoSolution arithmecyProgram is3 idfs

equal1 = fromString "=(+(21,21),+(21,21))."
prop_dfs_arithmecy_equal1 = testIfEmpty arithmecyProgram equal1 dfs
prop_bfs_arithmecy_equal1 = testIfEmpty arithmecyProgram equal1 bfs
prop_idfs_arithmecy_equal1 = testIfEmpty arithmecyProgram equal1 idfs

equal2 = fromString "=(+(21,21),42)."
prop_dfs_arithmecy_equal2 = testNoSolution arithmecyProgram equal2 dfs
prop_bfs_arithmecy_equal2 = testNoSolution arithmecyProgram equal2 bfs
prop_idfs_arithmecy_equal2 = testNoSolution arithmecyProgram equal2 idfs

zerodivision = fromString "is(_,mod(10,0))."
prop_dfs_arithmecy_zerodivision = testNoSolution arithmecyProgram zerodivision dfs
prop_bfs_arithmecy_zerodivision = testNoSolution arithmecyProgram zerodivision bfs
prop_idfs_arithmecy_zerodivision = testNoSolution arithmecyProgram zerodivision idfs

factorial1 = fromString "factorial(0,F)."
expectedFactorial1 = [substFromStrings ["F -> 1"]]
prop_dfs_arithmecy_factorial1 = testForSolution arithmecyProgram factorial1 dfs expectedFactorial1
prop_bfs_arithmecy_factorial1 = testForSolution arithmecyProgram factorial1 bfs expectedFactorial1
prop_idfs_arithmecy_factorial1 = testForSolution arithmecyProgram factorial1 idfs expectedFactorial1

factorial2 = fromString "factorial(N,F)."
expectedFactorial2 = [substFromStrings ["N -> 0", "F -> 1"]]
prop_dfs_arithmecy_factorial2 = testForSolution arithmecyProgram factorial2 dfs expectedFactorial2
prop_bfs_arithmecy_factorial2 = testForSolution arithmecyProgram factorial2 bfs expectedFactorial2
prop_idfs_arithmecy_factorial2 = testForSolution arithmecyProgram factorial2 idfs expectedFactorial2

factorial3 = fromString "factorial(30,F)."
expectedFactorial3 = [substFromStrings ["F -> 265252859812191058636308480000000"]]
prop_dfs_arithmecy_factorial3 = testForSolution arithmecyProgram factorial3 dfs expectedFactorial3
prop_bfs_arithmecy_factorial3 = testForSolution arithmecyProgram factorial3 bfs expectedFactorial3
prop_idfs_arithmecy_factorial3 = testForSolution arithmecyProgram factorial3 idfs expectedFactorial3


{-

Testing the List cases.

-}

listProgram :: Prog
listProgram = case parse "append([], L, L).\nappend([E|R], L, [E|RL]) :- append(R, L, RL).\nlast(L, E) :- append(_, [E], L).\nreverse([], []).\nreverse([E|R], L) :- reverse(R, UR), append(UR, [E], L).\nmember(E, [E|_]).\nmember(E, [_|R]) :- member(E,R).\nperm([], []).\nperm(L, [E|R]) :- delete(E, L, LwithoutE), perm(LwithoutE, R).\ndelete(E, L, R) :- append(L1, [E|L2], L), append(L1, L2, R).\nsort(L, S) :- perm(L, S), sorted(S).\nsorted([]).\nsorted([_]).\nsorted([E1|[E2|L]]) :- =<(E1, E2), sorted([E2|L]).\nlength([], 0).\nlength([_|Xs], N) :- length(Xs, N1), is(N, +(N1, 1)).\nlengthP([], o).\nlengthP([_|Xs], s(N)) :- lengthP(Xs, N)." of
    Right p -> p
    _       -> Prog []

subst_append1 = [Subst [("Xs", Comb "2" []),("Ys", Comb "1" [])]]
prop_dfs_append1 = testForSolution listProgram (fromString "append(Xs,Ys,[2,1]), append(Ys,Xs,[1,2]).") dfs subst_append1
prop_bfs_append1 = testForSolution listProgram (fromString "append(Xs,Ys,[2,1]), append(Ys,Xs,[1,2]).") bfs subst_append1
prop_idfs_append1 = testForSolution listProgram (fromString "append(Xs,Ys,[2,1]), append(Ys,Xs,[1,2]).") idfs subst_append1

--subst_append2 = [Subst [("X", Comb "[]" []), ("Y", Comb "." [Comb "1" [], Comb "." [Comb "2" [], Comb "[]" []]])],
--  Subst [("X", Comb "." [Comb "1" [], Comb "[]" []]), ("Y", Comb "." [Comb "2" [], Comb "[]" []])]
--"{X -> [], Y -> [1, 2]}",
-- % "{X -> [1], Y -> [2]}", and "{X -> [1, 2], Y -> []}".
-- prop_dfs_append2 = testForSolution listProgram (fromString "append(X,Y,[1,2])." dfs subst_append2)
-- prop_bfs_append2 = testForSolution listProgram (fromString "append(X,Y,[1,2])." bfs subst_append2)
-- prop_idfs_append2 = testForSolution listProgram (fromString "append(X,Y,[1,2])." idfs subst_append2)



return []
runTests = $quickCheckAll