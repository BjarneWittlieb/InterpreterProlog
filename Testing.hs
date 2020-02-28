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
    parse "[]" = Right (Comb "[]" [])
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

-- Breaks a String at the first occurence of the character char into two substrings
-- strBreak 'c' "abcdecf" == ("ab", "cdecf")
strBreak :: Char -> String -> (String , String)
strBreak c toSplit = (span (/= c) toSplit)

-- NOT a real Split!
-- Splits a string at a specific substring if it contains this substring
-- Assumes that at the first occurence of the first character of the substring splitter#
--      the splitter also starts
-- strSplit "abf -as- awfj" "-as-"== ("abf ", " awfj")
-- strSplit "hello world" "lost" == ("hello world", "")
-- strSplit "hey - -> no" "->" == ("hey - -> no", "")   !! THIS IS THE EDGE CASE FOR EXAMPLE
strSplit :: String -> String -> (String , String)
strSplit splitter toSplit = case stripPrefix splitter (snd break) of
    Just str -> (fst break, str)
    _ -> (toSplit, "")
    where
        break = strBreak (splitter!!0) toSplit

-- Trims the given string, that is removing all whitspace chars at the beginning and end
strTrim :: String -> String
strTrim str = strTrimBegin (strTrimEnd str)

-- Removes all whitespaces of a string at the beginning
strTrimBegin :: String -> String
strTrimBegin (' ':str) = strTrimBegin str
strTrimBegin ('\n':str) = strTrimBegin str
strTrimBegin str = str

-- Removes all whitespaces of a string at the end
strTrimEnd :: String -> String
strTrimEnd str = reverse (strTrimBegin (reverse str))



instance Eq Subst where
    s1 == s2 = (pretty s1) == (pretty s2)

-- converts a substitution into a standartized form
normify :: [VarName] -> Subst -> Subst
normify vs s = let s' = simplify vs (repeatSubst s) in order2 (renameSubst vs (order2 (applySub (order1 vs s') s'))) where 
  order1 :: [VarName] -> Subst -> Subst
  order1 v (Subst []) = empty
  order1 v' (Subst ((v, Var w):xs)) | (elem w v') && v > w = let sub = Subst [(v, Var w),(w, Var v)] in compose (order1 v' (applySub sub (Subst xs))) sub
  order1 v (Subst (x:xs)) = order1 v (Subst xs)
  order2 :: Subst -> Subst
  order2 (Subst []) = empty
  order2 (Subst ((v, t):xs)) = Subst ((fromSubst (order2 (Subst (filter (\(x, _) -> x < v) xs)))) ++
   (v, t):(fromSubst (order2 (Subst (filter (\(x, _) -> x > v) xs)))))


-- checks, if the normified versions of the corresponding substitutions in the lists are equal
eqSubsts :: Goal -> [Subst] -> [Subst] -> Bool
eqSubsts g s1 s2 = let vs = allVars g in
 (length s1 == length s2) && foldr (&&) True (fmap (\(x, y) -> (normify vs (Subst x)) == (normify vs (Subst y))) (zip (fmap (fromSubst) s1) (fmap (fromSubst) s2)))


-- Helper Functions for certain solution testing

-- Tests whether the solve returns expted list of substitutoins
-- The substitions have to be in right order, though they will be normified
testForSolution :: Prog -> Goal -> Strategy -> [Subst] -> Bool
testForSolution p g strat subs = eqSubsts g (solve strat p g) subs

-- Testcase for checking whether the first substituions of the solve are equal to the tested substitutions
-- Especially useful for cases where there are infinite solutions
testForSolutionInf :: Prog -> Goal -> Strategy -> [Subst] -> Bool
testForSolutionInf p g strat subs = eqSubsts g (take (length subs) (solve strat p g)) subs

-- Tests whether the solve returns a specific amount of solutions
testForSolutionLength :: Prog -> Goal -> Strategy -> Int -> Bool
testForSolutionLength p g strat l = (length (solve strat p g)) == l

-- Tests whether the goal cannot be proven
testNoSolution :: Prog -> Goal -> Strategy -> Bool
testNoSolution p g strat = case solve strat p g of
    [] -> True
    _ -> False

-- Tests whether the goal can be proven but only the empty substituion is returned
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
prop_dfs_unify_complicated1 = testForSolutionLength (Prog []) complicated1 dfs 1
prop_bfs_unify_complicated1 = testForSolutionLength (Prog []) complicated1 bfs 1
prop_idfs_unify_complicated1 = testForSolutionLength (Prog []) complicated1 idfs 1

complicated2 = fromString "=(p(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R),p(f(B,B),f(C,C),f(D,D),f(E,E),f(F,F),f(G,G),f(H,H),f(I,I),f(J,J),f(K,K),f(L,L),f(M,M),f(N,N),f(O,O),f(P,P),f(Q,Q),f(R,R),f(S,S)))."
prop_dfs_unify_complicated2 = testForSolutionLength (Prog []) complicated2 dfs 1
prop_bfs_unify_complicated2 = testForSolutionLength (Prog []) complicated2 bfs 1
prop_idfs_unify_complicated2 = testForSolutionLength (Prog []) complicated2 idfs 1

-- This takes far too long
-- complicated3 = fromString "=(p(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y),p(f(B,B),f(C,C),f(D,D),f(E,E),f(F,F),f(G,G),f(H,H),f(I,I),f(J,J),f(K,K),f(L,L),f(M,M),f(N,N),f(O,O),f(P,P),f(Q,Q),f(R,R),f(S,S),f(T,T),f(U,U),f(V,V),f(W,W),f(X,X),f(Y,Y),f(Z,Z)))."
-- prop_dfs_unify_complicated3 = testForSolutionLength (Prog []) complicated3 dfs 1
-- prop_bfs_unify_complicated3 = testForSolutionLength (Prog []) complicated3 bfs 1
-- prop_idfs_unify_complicated3 = testForSolutionLength (Prog []) complicated3 idfs 1


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
listString = "append([], L, L).\n" ++
    "append([E|R], L, [E|RL]) :- append(R, L, RL).\n" ++
    "last(L, E) :- append(_, [E], L).\n" ++
    "reverse([], []).\n" ++
    "reverse([E|R], L) :- reverse(R, UR), append(UR, [E], L).\n" ++
    "member(E, [E|_]).\n" ++
    "member(E, [_|R]) :- member(E,R).\n" ++
    "perm([], []).\n" ++
    "perm(L, [E|R]) :- delete(E, L, LwithoutE), perm(LwithoutE, R).\n" ++
    "delete(E, L, R) :- append(L1, [E|L2], L), append(L1, L2, R).\n" ++
    "sort(L, S) :- perm(L, S), sorted(S).\nsorted([]).\n" ++
    "sorted([_]).\n" ++
    "sorted([E1|[E2|L]]) :- =<(E1, E2), sorted([E2|L]).\n" ++
    "length([], 0).\n" ++
    "length([_|Xs], N) :- length(Xs, N1), is(N, +(N1, 1)).\n" ++
    "lengthP([], o).\n" ++
    "lengthP([_|Xs], s(N)) :- lengthP(Xs, N)."

listProgram :: Prog 
listProgram = case parse listString of
    Right p -> p
    _       -> Prog []

subst_append1 = fmap substFromStrings [["Xs -> .(2,[])", "Ys -> .(1,[])"]]
prop_dfs_append1 = testForSolution listProgram (fromString "append(Xs,Ys,[2,1]), append(Ys,Xs,[1,2]).") dfs subst_append1
prop_bfs_append1 = testForSolution listProgram (fromString "append(Xs,Ys,[2,1]), append(Ys,Xs,[1,2]).") bfs subst_append1
prop_idfs_append1 = testForSolution listProgram (fromString "append(Xs,Ys,[2,1]), append(Ys,Xs,[1,2]).") idfs subst_append1

subst_append2 = fmap substFromStrings [["X -> []", "Y -> .(1, [2])"], ["X -> .(1,[])", "Y -> .(2,[])"],["X -> .(1, [2])", "Y -> []"]]
prop_dfs_append2 = testForSolution listProgram (fromString "append(X,Y,[1,2]).") dfs subst_append2
prop_bfs_append2 = testForSolution listProgram (fromString "append(X,Y,[1,2]).") bfs subst_append2
prop_idfs_append2 = testForSolution listProgram (fromString "append(X,Y,[1,2]).") idfs subst_append2

subst_append3 = fmap substFromStrings [[],[],[]]
prop_dfs_append3 = testForSolution listProgram (fromString "append(_,_,[1,2]).") dfs subst_append3
prop_bfs_append3 = testForSolution listProgram (fromString "append(_,_,[1,2]).") bfs subst_append3
prop_idfs_append3 = testForSolution listProgram (fromString "append(_,_,[1,2]).") idfs subst_append3

subst_last = fmap substFromStrings [["X -> 3"]]
prop_dfs_last = testForSolution listProgram (fromString "last([1,2,3],X).") dfs subst_last
prop_bfs_last = testForSolution listProgram (fromString "last([1,2,3],X).") bfs subst_last
prop_idfs_last = testForSolution listProgram (fromString "last([1,2,3],X).") idfs subst_last

subst_last2 = fmap substFromStrings [["Xs -> .(3,[])"], ["Xs -> .(A,[3])"], ["Xs -> .(A, [B,3])"]]
prop_dfs_last2 = testForSolutionInf listProgram (fromString "last(Xs,3).") dfs subst_last2
prop_bfs_last2 = testForSolutionInf listProgram (fromString "last(Xs,3).") bfs subst_last2
prop_idfs_last2 = testForSolutionInf listProgram (fromString "last(Xs,3).") idfs subst_last2

subst_reverse1 = fmap substFromStrings [["Xs -> .(3,[2,1])"]]
prop_dfs_reverse1 = testForSolution listProgram (fromString "reverse([1,2,3],Xs).") dfs subst_reverse1
prop_bfs_reverse1 = testForSolution listProgram (fromString "reverse([1,2,3],Xs).") bfs subst_reverse1
prop_idfs_reverse1 = testForSolution listProgram (fromString "reverse([1,2,3],Xs).") idfs subst_reverse1
prop_dfs_reverse2 = testForSolutionInf listProgram (fromString "reverse(Xs,[1,2,3]).") dfs subst_reverse1
prop_bfs_reverse2 = testForSolutionInf listProgram (fromString "reverse(Xs,[1,2,3]).") bfs subst_reverse1
prop_idfs_reverse2 = testForSolutionInf listProgram (fromString "reverse(Xs,[1,2,3]).") idfs subst_reverse1

subst_reverse3 = fmap substFromStrings [["Xs -> []"], ["Xs -> .(A,[])"], ["Xs -> .(A,[A])"], ["Xs -> .(A,[B,A])"]]
prop_dfs_reverse3 = testForSolutionInf listProgram (fromString "reverse(Xs,Xs).") dfs subst_reverse3
prop_bfs_reverse3 = testForSolutionInf listProgram (fromString "reverse(Xs,Xs).") bfs subst_reverse3
prop_idfs_reverse3 = testForSolutionInf listProgram (fromString "reverse(Xs,Xs).") idfs subst_reverse3

subst_member1 = fmap substFromStrings [["X -> 1"], ["X -> 2"], ["X -> 3"]]
prop_dfs_member1 = testForSolution listProgram (fromString "member(X,[1,2,3]).") dfs subst_member1
prop_bfs_member1 = testForSolution listProgram (fromString "member(X,[1,2,3]).") bfs subst_member1
prop_idfs_member1 = testForSolution listProgram (fromString "member(X,[1,2,3]).") idfs subst_member1

subst_member2 = fmap substFromStrings [["Xs -> .(X,A)"], ["Xs -> .(A,.(X,B))"]]
prop_dfs_member2 = testForSolutionInf listProgram (fromString "member(X,Xs).") dfs subst_member2
prop_bfs_member2 = testForSolutionInf listProgram (fromString "member(X,Xs).") bfs subst_member2
prop_idfs_member2 = testForSolutionInf listProgram (fromString "member(X,Xs).") idfs subst_member2

subst_delete = fmap substFromStrings [["X -> 1", "Y -> .(2,[L])"], ["X -> 2", "Y -> .(1,[L])"], ["X -> L", "Y -> .(1,[2])"]]
prop_dfs_delete = testForSolution listProgram (fromString "delete(X,[1,2,L],Y).") dfs subst_delete
prop_bfs_delete = testForSolution listProgram (fromString "delete(X,[1,2,L],Y).") bfs subst_delete
prop_idfs_delete = testForSolution listProgram (fromString "delete(X,[1,2,L],Y).") idfs subst_delete

subst_sort = fmap substFromStrings [["Xs -> .(1,[2,3])"]]
prop_dfs_sort = testForSolution listProgram (fromString "sort([3,1,2],Xs).") dfs subst_sort
prop_bfs_sort = testForSolution listProgram (fromString "sort([3,1,2],Xs).") bfs subst_sort
prop_idfs_sort = testForSolution listProgram (fromString "sort([3,1,2],Xs).") idfs subst_sort

subst_append4 = fmap substFromStrings [["X -> []", "Y -> []"], ["X -> .(A,[])", "Y -> []"], ["X -> .(A,[B])", "Y -> []"]]
prop_dfs_append4 = testForSolutionInf listProgram (fromString "append(X,Y,X).") dfs subst_append4
prop_bfs_append4 = testForSolutionInf listProgram (fromString "append(X,Y,X).") bfs subst_append4
prop_idfs_append4 = testForSolutionInf listProgram (fromString "append(X,Y,X).") idfs subst_append4

subst_length = fmap substFromStrings [["Xs -> .(A,[B])"]]
prop_dfs_length = testForSolutionInf listProgram (fromString "length(Xs,2).") dfs subst_length
prop_bfs_length = testForSolutionInf listProgram (fromString "length(Xs,2).") bfs subst_length
prop_idfs_length = testForSolutionInf listProgram (fromString "length(Xs,2).") idfs subst_length

subst_lengthP = fmap substFromStrings [["Xs -> .(A,[B])"]]
prop_dfs_lengthP = testForSolution listProgram (fromString "lengthP(Xs,s(s(o))).") dfs subst_lengthP
prop_bfs_lengthP = testForSolution listProgram (fromString "lengthP(Xs,s(s(o))).") bfs subst_lengthP
prop_idfs_lengthP = testForSolution listProgram (fromString "lengthP(Xs,s(s(o))).") idfs subst_lengthP


{-

Testing the Higher order terms using familyProgram.

-}
familyString = "ehemann(christine, heinz).\n" ++
    "ehemann(maria, fritz).\n" ++
    "ehemann(monika, herbert).\n" ++
    "ehemann(angelika, hubert).\n" ++
    "mutter(herbert, christine).\n" ++
    "mutter(angelika, christine).\n" ++
    "mutter(hubert, maria).\n" ++
    "mutter(susanne, monika).\n" ++
    "mutter(norbert, monika).\n" ++
    "mutter(andreas, angelika).\n" ++
    "vater(K, V) :- ehemann(M, V), mutter(K, M).\n" ++
    "elter(K, E) :- vater(K, E).\n" ++
    "elter(K, E) :- mutter(K, E).\n" ++
    "grossvater(E, G) :- elter(E, F), vater(F, G).\n" ++
    "grossvaeter(Gs) :- findall([E, G], grossvater(E, G), Gs).\n" ++
    "vorfahre(N, V) :- vorfahre(N, V2), vorfahre(V2, V).\n" ++
    "vorfahre(N, V) :- elter(N, V).\n" ++
    "geschwister(S, P) :- mutter(S, M), mutter(P,M), \\+(=(P, S))."

familyProgram:: Prog
familyProgram = case parse familyString of
    Right p -> p
    _       -> Prog []

subst_vorfahre = fmap substFromStrings 
  [["X -> herbert", "Y -> christine"], ["X -> angelika", "Y -> christine"], ["X -> hubert", "Y -> maria"], ["X -> susanne", "Y -> monika"],
  ["X -> norbert", "Y -> monika"], ["X -> andreas", "Y -> angelika"], ["X -> herbert", "Y -> heinz"], ["X -> angelika", "Y -> heinz"],
  ["X -> hubert", "Y -> fritz"], ["X -> susanne", "Y -> herbert"], ["X -> norbert", "Y -> herbert"], ["X -> andreas", "Y -> hubert"],
  ["X -> andreas", "Y -> christine"], ["X -> susanne", "Y -> christine"], ["X -> norbert", "Y -> christine"], ["X -> andreas", "Y -> maria"],
  ["X -> andreas", "Y -> heinz"], ["X -> susanne", "Y -> heinz"], ["X -> norbert", "Y -> heinz"], ["X -> andreas", "Y -> fritz"]]
prop_bfs_vorfahre = testForSolutionInf familyProgram (fromString "vorfahre(X,Y).") bfs subst_vorfahre
prop_idfs_vorfahre = testForSolutionInf familyProgram (fromString "vorfahre(X,Y).") idfs subst_vorfahre

subst_geschwister = fmap substFromStrings [["X -> herbert", "Y -> angelika"], ["X -> angelika", "Y -> herbert"], ["X -> susanne", "Y -> norbert"], ["X -> norbert", "Y -> susanne"]]
prop_dfs_geschwister = testForSolution familyProgram (fromString "geschwister(X,Y).") dfs subst_geschwister
prop_bfs_geschwister = testForSolution familyProgram (fromString "geschwister(X,Y).") bfs subst_geschwister
prop_idfs_geschwister = testForSolution familyProgram (fromString "geschwister(X,Y).") idfs subst_geschwister

subst_grossvaeter = fmap substFromStrings [["Xs -> .([susanne, heinz], [[norbert, heinz], [andreas, fritz], [andreas, heinz]])"]]
subst_grossvaeter' = fmap substFromStrings [["Xs -> .([andreas, heinz], [[susanne, heinz], [norbert, heinz], [andreas, fritz]])"]]
prop_dfs_grossvaeter = testForSolution familyProgram (fromString "grossvaeter(Xs).") dfs subst_grossvaeter
prop_bfs_grossvaeter = testForSolution familyProgram (fromString "grossvaeter(Xs).") bfs subst_grossvaeter'
prop_idfs_grossvaeter = testForSolution familyProgram (fromString "grossvaeter(Xs).") idfs subst_grossvaeter'



return []
runTests = $quickCheckAll