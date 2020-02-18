module Rename where

import Data.List
import Type
import Vars
import Substitutions


-- Replaces the "_" variable with other fitting variables, the variables in the specifid list won't be used
replaceUnderscore :: Term -> [VarName] -> (Term, [VarName])
-- The actual replacing
replaceUnderscore (Var "_") vs = ((Var s), (s:vs))  where
  s = (filter (\x -> (not (elem x vs))) freshVars)!!0
-- Nothing to replace
replaceUnderscore (Var x) vs = ((Var x), vs)
-- Replace within list
replaceUnderscore (Comb f xs) vs = ((Comb f (fst rlist)), snd rlist) where
  rlist = replaceList xs vs


-- Replaces the "_" variable in a list of terms
replaceList :: [Term] -> [VarName] -> ([Term], [VarName])
replaceList [] vs = ([], vs)
replaceList xs vs = convertType (replaceListAcc xs vs) where
  -- replaces the "_" variable in each term in the list
  replaceListAcc :: [Term] -> [VarName] -> [(Term,[VarName])]
  replaceListAcc [] _ = []
  replaceListAcc (x:xs) vs = let y = replaceUnderscore x vs in (y):(replaceListAcc xs (vs ++ (snd y)))
  -- converts the return type into something more useful
  convertType :: (Eq b) => [(a,[b])] -> ([a],[b])
  convertType [] = ([],[])
  convertType (x:xs) = combine x (convertType xs)
  combine (a1, b1s) (a2s, b2s) = (a1:a2s, nub (b1s ++ b2s)) 

-- Renames all variables in a rule, variables from the specified list won't be used
-- returns the changed Rule and a superset of the input list, including all variables in the changed Rule
rename :: Rule -> [VarName] -> (Rule, [VarName])
rename r vs = (applyToRule (substitute ruleVars substVars) rule, nub(vs ++ substVars)) where
  -- replaces all underscore variables first
  (rule, vars) = (replaceUnderscoreRule r vs)
  -- a list of all variables in the Rule
  ruleVars = allVars rule
  -- a list of all variables, that will be used in the substituted Rule
  substVars = take (length ruleVars) (filter (\x -> not (elem x vars)) freshVars)
  -- applies a substitution to a Rule
  applyToRule :: Subst -> Rule -> Rule
  applyToRule s (Rule t ts) = Rule (apply s t) (fmap (apply s) ts)
  -- replaces all underscore variables in a Rule
  replaceUnderscoreRule :: Rule -> [VarName] -> (Rule, [VarName])
  replaceUnderscoreRule (Rule t ts) vs = let (t', xs) = replaceUnderscore t (vs ++ (allVars (Rule t ts)))
                                         in let (ts', ys) = replaceList ts xs
                                            in (Rule t' ts', ys)
  -- substitutes a list of variables with a different list of variables
  substitute :: [VarName] -> [VarName] -> Subst
  substitute [] _ = empty
  substitute _ [] = empty
  substitute (x:xs) (y:ys) = compose (single x (Var y)) (substitute xs ys)  
