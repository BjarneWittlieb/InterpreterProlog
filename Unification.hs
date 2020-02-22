module Unification(unify) where

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
                           | otherwise                  = listToMaybe (catMaybes (fmap (uncurry ds) (zip xs ys)))


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
    unifyStep t1 t2 subst = case (ds t1 t2) of
                               Nothing -> Just subst
                               Just (Var v, q) -> let sub = single v q in unifyStep (apply sub t1) (apply sub t2) (compose sub subst)
                               Just _ -> Nothing