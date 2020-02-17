module Unification where

import Data.Maybe

import Prettyprinting
import Substitutions
import Type


-- Calculates the first different values of the disagreement set for two terms
ds :: Term -> Term -> Maybe (Term, Term)
ds (Var "_") y = Just ((Var "_"), y)
ds x (Var "_") = Just (x, (Var "_"))
ds (Var x) (Var y)         | x == y                     = Nothing
                           | otherwise                  = Just ((Var x), (Var y))
ds (Comb f xs) (Comb g ys) | f /= g                     = Just ((Comb f xs), (Comb g ys))
                           | (length xs) /= (length ys) = Just ((Comb f xs), (Comb g ys))
                           | otherwise                  = listToMaybe (catMaybes (fmap (uncurry ds) xys)) where
                               xys = throwTogether xs ys
                               -- throwTogether assumes that the terms have equal length
                               throwTogether :: [a] -> [b] -> [(a, b)]
                               throwTogether [] []         = []
                               throwTogether (x:xs) (y:ys) = (x, y):(throwTogether xs ys)

termTest1 = (Comb "." [Comb "true" [], Comb "h" [Var "E", Comb "i" [Var "F"]]])
termTest2 = (Comb "." [Var "H", Comb "true" [], Comb "j" [Var "I"]])
termTest3 = (Comb "." [Var "H", Comb "true" [], Comb "j" [Var "M"]])