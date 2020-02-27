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

instance Arbitrary Peano where
    arbitrary = do
        ranInt <- arbitrary
        return (intToPeano ranInt)

instance Eq Peano where
    O == O          = True
    (S x) == (S y)  = x == y
    _ == _          = False

prop_one_solution :: Peano -> Peano -> Bool
prop_one_solution x y = case solve dfs peanoProgram (Goal [(Comb "add" [(peanoToTerm x) ,(peanoToTerm y), (Var "X")])]) of
    [(Subst [("X", term)])] -> case termToPeano term of
        Just p -> p == (addP x y)
        Nothing -> False
    _ -> False

prop_append1_dfs :: Bool
prop_append1_dfs solve dfs 

return []
runTests = $quickCheckAll