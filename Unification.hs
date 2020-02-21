module Unification where

import Data.Maybe

import Substitutions
import Type


-- Calculates the first different values of the disagreement set for two terms
ds :: Term -> Term -> Maybe (Term, Term)
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
                               -- aka zip
                               -- throwTogether assumes that the terms have equal length
                               throwTogether :: [a] -> [b] -> [(a, b)]
                               throwTogether [] []           = []
                               throwTogether (x:xs1) (y:ys1) = (x, y):(throwTogether xs1 ys1)
                               throwTogether _ _             = []


-- The unification algorithm
unify :: Term -> Term -> Maybe Subst
-- When either the symbols are not equal or the predicates take a different
-- amount of arguments unification is not possible
unify term1 term2 = unifyStep term1 term2 empty where
    unifyStep :: Term -> Term -> Subst -> Maybe Subst
    unifyStep (Comb f xs) (Comb g ys) _ | f /= g                     = Nothing
                                        | (length xs) /= (length ys) = Nothing 
    -- When ds is empty then we are finished
    -- add case for more efficiency      VVV
    unifyStep u1 u2 subst | isNothing (ds u1 u2) = Just subst
    -- When ds is not empty the Subst is changed to the newer version
                      | otherwise            = unifyStepAcc u1 u2 subst x where
                          x = (fromJust (ds u1 u2))
                          unifyStepAcc :: Term -> Term -> Subst -> (Term, Term) -> Maybe Subst
                          -- if the ds has a variable in its first argument, the substitution is possible
                          unifyStepAcc t1 t2 s (Var v, q) = unifyStep t3 t4 s2 where
                              s2 = compose (single v q) s
                              t3 = apply (single v q) t1
                              t4 = apply (single v q) t2
                          -- otherwise a substitution isn't possible
                          unifyStepAcc _ _ _ _ = Nothing