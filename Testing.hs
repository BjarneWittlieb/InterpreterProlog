{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck

import SLDResolution
import Substitutions
import Type
import Parser

data Peano = O | S Peano deriving Show

peanoProgram :: Prog
peanoProgram = case parse "add(o   ,Y,Y).\nadd(s(X),Y,s(Z)) :- add(X,Y,Z)." of
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

instance Arbitrary Peano where
    arbitrary = do
        ranInt <- arbitrary
        return (intToPeano ranInt)

instance Eq Peano where
    O == O          = True
    (S x) == (S y)  = x == y
    _ == _          = False

one_solution :: Strategy -> Peano -> Peano -> Bool
one_solution strat x y = case solve strat peanoProgram (Goal [(Comb "add" [(peanoToTerm x) ,(peanoToTerm y), (Var "X")])]) of
    [(Subst [("X", term)])] -> case termToPeano term of
        Just p -> p == (addP x y)
        Nothing -> False
    _ -> False

prop_dfs_one_solution = one_solution dfs
prop_bfs_one_solution = one_solution bfs
prop_idfs_one_solution = one_solution idfs


twoVarsTerm :: Goal
twoVarsTerm = case parse "=(A, B)" of
    Right g -> g
    _       -> error "HELP"
unify_twoVars :: Strategy -> Bool
unify_twoVars strat = case solve strat (Prog []) twoVarsTerm of
    [Subst [("A", (Var "B"))]] -> True
    [Subst [("B", (Var "A"))]] -> True
    _ -> False

prop_dfs_unify_twoVars = unify_twoVars dfs
prop_bfs_unify_twoVars = unify_twoVars bfs
prop_idfs_unify_twoVars = unify_twoVars idfs

occurTerm :: Goal
occurTerm = case parse "=(A, f(A))" of
    Right g -> g
    _       -> error "HELP"
unify_occur :: Strategy -> Bool
unify_occur strat = case solve strat (Prog []) occurTerm of
    [] -> True
    _ -> False

prop_dfs_unify_occur = unify_occur dfs
prop_bfs_unify_occur = unify_occur bfs
prop_idfs_unify_occur = unify_occur idfs

return []
runTests = $quickCheckAll