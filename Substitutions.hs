module Substitutions where

import Type
import Prettyprinting
import Data.List


data Subst = Subst [(VarName, Term)]
  deriving Show

empty :: Subst
empty = Subst []

single :: VarName -> Term -> Subst
single v t = Subst [(v, t)]

-- Applies A Substitution to a Term
apply :: Subst -> Term -> Term
apply (Subst []) t = t
apply (Subst ((x,y):xs)) (Var v) | v == x = y
                                 | otherwise = apply (Subst xs) (Var v)
apply s (Comb f xs) = Comb f (fmap (apply s) xs)

-- Composing to Substitions
-- Note that only in the right side Terms of the first substition are updatet!
compose :: Subst -> Subst -> Subst
compose (Subst xs) (Subst ys) = Subst (substitutedSet ++ filteredSet) where
  -- Substitudes all terms on the right side from the first Substitution with the second
  substitutedSet = (fmap (\(x, y) -> (x, apply (Subst ys) y)) xs)
  -- Filters the substitutions from the first substitution out of the second (based on the vars on the left side)
  filteredSet = filter (\(x, y) -> (not (elem x (fmap fst xs)))) ys

instance Pretty Subst where
  pretty (Subst xs) = "{" ++ (intercalate ", " (fmap (\(x, y) -> x ++ " -> " ++ (pretty y)) xs)) ++ "}"

-- Substitutions for Testing
s1 = Subst [("A",Comb "f" [Var "B", Var "_", Comb "true" []])]
s2 = Subst [("B",Comb "." [Comb "true" [], Var "D"])]