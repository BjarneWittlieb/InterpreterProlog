{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck
import Data.List

import SLDResolution
import Substitutions
import Type
import Parser
import Prettyprinting
import Unification
import Vars

data Peano = O | S Peano deriving Show

peanoProgram :: Prog
peanoProgram = case parse "add(o   ,Y,Y).\nadd(s(X),Y,s(Z)) :- add(X,Y,Z)." of
    Right p -> p
    _       -> Prog []

listProgram :: Prog
listProgram = case parse "append([], L, L).\nappend([E|R], L, [E|RL]) :- append(R, L, RL).\nlast(L, E) :- append(_, [E], L).\nreverse([], []).\nreverse([E|R], L) :- reverse(R, UR), append(UR, [E], L).\nmember(E, [E|_]).\nmember(E, [_|R]) :- member(E,R).\nperm([], []).\nperm(L, [E|R]) :- delete(E, L, LwithoutE), perm(LwithoutE, R).\ndelete(E, L, R) :- append(L1, [E|L2], L), append(L1, L2, R).\nsort(L, S) :- perm(L, S), sorted(S).\nsorted([]).\nsorted([_]).\nsorted([E1|[E2|L]]) :- =<(E1, E2), sorted([E2|L]).\nlength([], 0).\nlength([_|Xs], N) :- length(Xs, N1), is(N, +(N1, 1)).\nlengthP([], o).\nlengthP([_|Xs], s(N)) :- lengthP(Xs, N)." of
    Right p -> p
    _       -> Prog []


intToPeano :: Int -> Peano
intToPeano x | x <= 0  = O
             | x > 0   = S (intToPeano (x-1))

addP :: Peano -> Peano -> Peano
addP O x     = x
addP (S y) x = S (addP y x)

peanoToInt :: Peano -> Int
peanoToInt O     = 0
peanoToInt (S p) = 1 + (peanoToInt p)

peanoToTerm :: Peano -> Term
peanoToTerm O     = Comb "o" []
peanoToTerm (S p) = Comb "s" [(peanoToTerm p)]

termToPeano :: Term -> Maybe Peano
termToPeano (Comb "o" [])   = Just O
termToPeano (Comb "s" [x])  = (termToPeano x) >>= (Just . S)
termToPeano _               = Nothing

fromString :: String -> Goal
fromString s = case parse s of
    Right g -> g
    _       -> error "Parse error!"

instance Arbitrary Peano where
    arbitrary = do
        ranInt <- arbitrary
        return (intToPeano ranInt)

instance Eq Peano where
    O == O          = True
    (S x) == (S y)  = x == y
    _ == _          = False

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

one_solution :: Strategy -> Peano -> Peano -> Bool
one_solution strat x y = case solve strat peanoProgram (Goal [(Comb "add" [(peanoToTerm x) ,(peanoToTerm y), (Var "X")])]) of
    [(Subst [("X", term)])] -> case termToPeano term of
        Just p -> p == (addP x y)
        Nothing -> False
    _ -> False


prop_dfs_one_solution = one_solution dfs
prop_bfs_one_solution = one_solution bfs
prop_idfs_one_solution = one_solution idfs


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



testForSolution :: Prog -> Goal -> Strategy -> [Subst] -> Bool
testForSolution p g strat subs = eqSubsts g (solve strat p g) subs


testNoSolution :: Goal -> Strategy -> Bool
testNoSolution g strat = case solve strat (Prog []) g of
    [] -> True
    _ -> False

testIfEmpty :: Goal -> Strategy -> Bool
testIfEmpty g strat = case solve strat (Prog []) g of
    [Subst []] -> True
    _ -> False


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