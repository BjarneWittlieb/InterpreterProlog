module Substitutions where

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
  allVars (Subst xs) = killDuplicates (foldr (++) [] (fmap (\(x, y) -> x:allVars(y)) xs))


-- Kann raus, übers Ziel hinaus
instance Eq Subst where
  (Subst xs) == (Subst ys) = foldr (&&) True ((fmap (inSubst xs) ys) ++ (fmap (inSubst ys) xs)) where
    inSubst :: [(VarName, Term)] -> (VarName, Term) -> Bool
    inSubst [] _ = False
    inSubst ((v', t'):zs) (v, t) = ((v == v') && (termEq t t')) || (inSubst zs (v, t))

-- checks, if two terms are equal
termEq :: Term -> Term -> Bool
termEq (Var x) (Var y) = x == y
termEq (Comb f xs) (Comb g ys) = f == g && (foldr (&&) True (fmap (uncurry termEq) (zip xs ys)))
termEq _ _ = False

-- Special Substitutions
empty :: Subst
empty = Subst []

single :: VarName -> Term -> Subst
single v t = Subst [(v, t)]

multiple :: [VarName] -> [Term] -> Subst
multiple [] _ = empty
multiple _ [] = empty
multiple (v:vs) (t:ts) = compose (single v t) (multiple vs ts)

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

-- simplifies a substitution to generate a better output
simplify :: [VarName] -> Subst -> Subst
simplify vars s = compose (invert (fst subSplit)) (snd subSplit) where
  subSplit = splitTrivialSubst s vars
  splitTrivialSubst :: Subst -> [VarName] -> (Subst, Subst)
  splitTrivialSubst (Subst []) _ = (empty, empty)
  splitTrivialSubst (Subst ((v, Var w):xs)) vs | (not (elem w vs)) = (Subst ((v, Var w):(fromSubst (fst rest))), (snd rest)) where
    rest = splitTrivialSubst (compose (single w (Var v)) (Subst xs)) vs
  splitTrivialSubst (Subst (x:xs)) vs = (\a (b, c) -> (b, Subst (a:(fromSubst c)))) x (splitTrivialSubst (Subst xs) vs)
  -- inverts a substitutiom (if possible
  invert :: Subst -> Subst
  invert (Subst ((v, Var w):xs)) = Subst ((w, Var v):(fromSubst (invert (Subst xs))))
  invert _ = empty


fromSubst :: Subst -> [(VarName, Term)]
fromSubst (Subst x) = x