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


listProgram :: Prog
listProgram = case parse "append([], L, L).\nappend([E|R], L, [E|RL]) :- append(R, L, RL).\nlast(L, E) :- append(_, [E], L).\nreverse([], []).\nreverse([E|R], L) :- reverse(R, UR), append(UR, [E], L).\nmember(E, [E|_]).\nmember(E, [_|R]) :- member(E,R).\nperm([], []).\nperm(L, [E|R]) :- delete(E, L, LwithoutE), perm(LwithoutE, R).\ndelete(E, L, R) :- append(L1, [E|L2], L), append(L1, L2, R).\nsort(L, S) :- perm(L, S), sorted(S).\nsorted([]).\nsorted([_]).\nsorted([E1|[E2|L]]) :- =<(E1, E2), sorted([E2|L]).\nlength([], 0).\nlength([_|Xs], N) :- length(Xs, N1), is(N, +(N1, 1)).\nlengthP([], o).\nlengthP([_|Xs], s(N)) :- lengthP(Xs, N)." of
    Right p -> p
    _       -> Prog []

familyProgram:: Prog
familyProgram = case parse "ehemann(christine, heinz).\nehemann(maria, fritz).\nehemann(monika, herbert).\nehemann(angelika, hubert).\nmutter(herbert, christine).\nmutter(angelika, christine).\nmutter(hubert, maria).\nmutter(susanne, monika).\nmutter(norbert, monika).\nmutter(andreas, angelika).\nvater(K, V) :- ehemann(M, V), mutter(K, M).\nelter(K, E) :- vater(K, E).\nelter(K, E) :- mutter(K, E).\ngrossvater(E, G) :- elter(E, F), vater(F, G).\ngrossvaeter(Gs) :- findall([E, G], grossvater(E, G), Gs).\nvorfahre(N, V) :- vorfahre(N, V2), vorfahre(V2, V).\nvorfahre(N, V) :- elter(N, V).\ngeschwister(S, P) :- mutter(S, M), mutter(P,M), \\+(=(P, S))." of
    Right p -> p
    _       -> Prog []

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

testForSolutionInf :: Prog -> Goal -> Strategy -> [Subst] -> Bool
testForSolutionInf p g strat subs = eqSubsts g (take (length subs) (solve strat p g)) subs

testNoSolution :: Goal -> Strategy -> Bool
testNoSolution g strat = case solve strat (Prog []) g of
    [] -> True
    _ -> False

testIfEmpty :: Goal -> Strategy -> Bool
testIfEmpty g strat = case solve strat (Prog []) g of
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
prop_dfs_unify_occur    = testNoSolution occurTerm dfs
prop_bfs_unify_occur    = testNoSolution occurTerm bfs
prop_idfs_unify_occur   = testNoSolution occurTerm idfs

firstEmpty = fromString "=(_, A)."
prop_dfs_firstanonym = testIfEmpty firstEmpty dfs
prop_bfs_firstanonym = testIfEmpty firstEmpty bfs
prop_idfs_firstanonym = testIfEmpty firstEmpty idfs
secondEmpty = fromString "=(A, _)."
prop_dfs_secondanonym = testIfEmpty secondEmpty dfs
prop_bfs_secondanonym = testIfEmpty secondEmpty bfs
prop_idfs_secondanonym = testIfEmpty secondEmpty idfs
bothEmpty = fromString "=(_,_)."
prop_dfs_bothanonym = testIfEmpty bothEmpty dfs
prop_bfs_bothanonym = testIfEmpty bothEmpty bfs
prop_idfs_bothanonym = testIfEmpty bothEmpty idfs

multSubs = fromString "=(f(A,B),f(f(C),g(D)))."
expectedMultSubs = [substFromStrings ["A -> f(C)", "B -> g(D)"]]
prop_dfs_multiplesubs = testForSolution (Prog []) multSubs dfs expectedMultSubs
prop_bfs_multiplesubs = testForSolution (Prog []) multSubs bfs expectedMultSubs
prop_idfs_multiplesubs = testForSolution (Prog []) multSubs idfs expectedMultSubs

multSubsEmpty = fromString "=(f(_,_),f(f(C),g(D)))."
prop_dfs_multiplesubsempty = testIfEmpty multSubsEmpty dfs
prop_bfs_multiplesubsempty = testIfEmpty multSubsEmpty bfs
prop_idfs_multiplesubsempty = testIfEmpty multSubsEmpty idfs

multSubsDeeper = fromString "=(f(A,B),f(f(C),g(A)))."
expectedMultSubsDeeper  = [substFromStrings ["A -> f(C)", "B -> g(f(C))"]]
prop_dfs_multiplesubsdeep = testForSolution (Prog []) multSubsDeeper dfs expectedMultSubsDeeper
prop_bfs_multiplesubsdeep = testForSolution (Prog []) multSubsDeeper bfs expectedMultSubsDeeper
prop_idfs_multiplesubsdeep = testForSolution (Prog []) multSubsDeeper idfs expectedMultSubsDeeper


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