module Unification(unify, applySub) where

import Data.Maybe

import Substitutions
import Type
import Vars


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
unify t1 t2 = case (ds t1 t2) of
    Nothing -> Just empty
    Just (Var v, q) | elem v (allVars q) -> Nothing
    Just (Var v, q) -> let sub = single v q in fmap (repComp sub) (unify (apply sub t1) (apply sub t2))
    Just _ -> Nothing

-- substitutes variables in a substitution
applySub :: Subst -> Subst -> Subst
applySub s (Subst ys) = foldr repComp empty (catMaybes (fmap (\(v, t) -> (uncurry unify) (apply s (Var v), apply s t)) ys))
