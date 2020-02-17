module Unification where

import Data.Maybe

import Prettyprinting
import Substitutions
import Type


-- Calculates the first different values of the disagreement set for two terms
ds :: Term -> Term -> Maybe (Term, Term)
-- Edge cases for the underscore
ds (Var "_") y = Just ((Var "_"), y)
ds x (Var "_") = Just (x, (Var "_"))
-- To variables are equal if and only if they have the same name
ds (Var x) (Var y)         | x == y                     = Nothing
                           | otherwise                  = Just ((Var x), (Var y))
-- When one is a variable and the other is a term put the variable allways left
ds (Var x) t = Just ((Var x), t)
ds t (Var x) = Just ((Var x), t)
-- two share the ds of their inner parts when they are not equal or have not equal length
ds (Comb f xs) (Comb g ys) | f /= g                     = Just ((Comb f xs), (Comb g ys))
                           | (length xs) /= (length ys) = Just ((Comb f xs), (Comb g ys))
                           | otherwise                  = listToMaybe (catMaybes (fmap (uncurry ds) xys)) where
                               xys = throwTogether xs ys
                               -- throwTogether assumes that the terms have equal length
                               throwTogether :: [a] -> [b] -> [(a, b)]
                               throwTogether [] []         = []
                               throwTogether (x:xs) (y:ys) = (x, y):(throwTogether xs ys)


-- The unification algorithm
unify :: Term -> Term -> Maybe Subst
-- When either the symbols are not equal or the predicates take a different
-- amount of arguments unification is not possible
unify (Comb f xs) (Comb g ys) | f /= g                     = Nothing
                              | (length xs) /= (length ys) = Nothing
unify t1 t2                                                = unifyStep t1 t2 empty where
    unifyStep :: Term -> Term -> Subst -> Maybe Subst
    -- When ds is empty then we are Finnished
    unifyStep t1 t2 s | isNothing (ds t1 t2) = Just s
    -- When ds is not empty the Subst is changed to the newer version
                      | otherwise            = unifyStepAcc t1 t2 s x where
                          x = (fromJust (ds t1 t2))
                          unifyStepAcc :: Term -> Term -> Subst -> (Term, Term) -> Maybe Subst
                          unifyStepAcc t1 t2 s (Var v, q) = unifyStep t3 t4 s2 where
                              s2 = compose (single v q) s
                              t3 = apply s2 t1
                              t4 = apply s2 t2


termTest1 = (Comb "." [Comb "true" [], Comb "h" [Var "E", Comb "i" [Var "F"]]])
termTest2 = (Comb "." [Var "H", Comb "true" [], Comb "j" [Var "I"]])
termTest3 = (Comb "." [Var "H", Comb "true" [], Comb "j" [Var "M"]])