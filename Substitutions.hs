module Substitutions(Subst(Subst), empty, single, multiple, Substitutable, apply, compose, restrictTo, fromSubst, isEmpty, repeatSubst, repComp) where

import Type
import Prettyprinting
import Data.List
import Vars


-- Instanciating Subst
data Subst = Subst [(VarName, Term)]
  deriving Show       

instance Pretty Subst where
  pretty (Subst xs) = "{" ++ (intercalate ", " (fmap (\(x, y) -> x ++ " -> " ++ (pretty y)) xs)) ++ "}"

instance Vars Subst where
  allVars (Subst xs) = nub (concat (fmap (\(x, y) -> x:allVars(y)) xs))


-- Special Substitutions
empty :: Subst
empty = Subst []

single :: VarName -> Term -> Subst
single v (Var w) | v == w = empty
single v t = Subst [(v, t)]

multiple :: [VarName] -> [Term] -> Subst
multiple vs ts = foldr (\(x, y) -> repComp (single x y)) empty (zip vs ts)

-- Applies A Substitution
class Substitutable a where 
  apply :: Subst -> a -> a

instance Substitutable Term where 
  apply (Subst []) t = t
  apply (Subst ((x,y):xs)) (Var v) | v == x = y
                                   | otherwise = apply (Subst xs) (Var v)
  apply s (Comb f xs) = Comb f (fmap (apply s) xs)

instance Substitutable Rule where
  apply s (Rule t ts) = Rule (apply s t) (fmap (apply s) ts)

instance Substitutable Prog where
  apply s (Prog rs) = Prog (fmap (apply s) rs)

instance Substitutable Goal where
  apply s (Goal ts) = Goal (fmap (apply s) ts)

instance Substitutable a => Substitutable [a] where
  apply s xs = fmap (apply s) xs

-- Composing to Substitions
-- Note that only in the right side Terms of the first substition are updated!
compose :: Subst -> Subst -> Subst
compose (Subst xs) (Subst ys) = Subst (substitutedSet ++ filteredSet) where
  -- Substitutes all terms on the right side from the first Substitution with the second
  substitutedSet = (fmap (\(x, y) -> (x, apply (Subst xs) y)) ys)
  -- Filters the substitutions from the first substitution out of the second one (based on the vars on the left side)
  filteredSet = filter (\(x, _) -> (not (elem x (fmap fst ys)))) xs

-- restricts a substitution to a set of variables
restrictTo :: [VarName] -> Subst -> Subst
restrictTo _ (Subst []) = empty
restrictTo vs (Subst (x:xs)) | (elem (fst x) vs) = let Subst ys = restrictTo vs (Subst xs) in Subst (x:ys)
                             | otherwise = restrictTo vs (Subst xs) 

repeatSubst :: Subst -> Subst
repeatSubst s = repeatSubstAcc s s where
  repeatSubstAcc :: Subst -> Subst -> Subst
  repeatSubstAcc s1 s2 | disjunct s1 s2 = s2
                       | otherwise = repeatSubstAcc s1 (compose s1 s2)
  disjunct :: Subst -> Subst -> Bool
  disjunct (Subst xs) (Subst ys) = (filter ((flip elem) (concat (fmap (\(_, y) -> allVars y) ys))) (fmap fst xs)) == []

repComp :: Subst -> Subst -> Subst
repComp = (.) (.) (.) repeatSubst compose

fromSubst :: Subst -> [(VarName, Term)]
fromSubst (Subst x) = x

isEmpty :: Subst -> Bool
isEmpty (Subst []) = True
isEmpty _ = False