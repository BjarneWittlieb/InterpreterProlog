module Rename where

import Data.List
import Type
import Vars
import Substitutions 
import Prettyprinting

-- Renames all variables in a rule, variables from the specified list won't be used
-- returns the changed Rule and a superset of the input list, including all variables in the changed Rule
rename :: Rule -> [VarName] -> (Rule, [VarName])
rename r vs = (apply (multiple ruleVars (fmap (\x -> (Var x)) substVars)) rule, nub(vs ++ substVars)) where
  -- replaces all underscore variables first
  (rule, vars) = (replaceUnderscoreRule r vs)
  -- a list of all variables in the Rule
  ruleVars = allVars rule
  -- a list of all variables, that will be used in the substituted Rule
  substVars = take (length ruleVars) (filter (\x -> not (elem x vars)) freshVars)
  -- replaces all underscore variables in a Rule
  replaceUnderscoreRule :: Rule -> [VarName] -> (Rule, [VarName])
  replaceUnderscoreRule (Rule t ts) vs = let (t', xs) = replaceUnderscore t (vs ++ (allVars (Rule t ts)))
                                         in let (ts', ys) = replaceList ts xs
                                            in (Rule t' ts', ys) 

  -- Replaces the "_" variable with other fitting variables, the variables in the specified list won't be used
  replaceUnderscore :: Term -> [VarName] -> (Term, [VarName])
  -- The actual replacing
  replaceUnderscore (Var "_") vs = let newVar = (filter (\x -> (not (elem x vs))) freshVars)!!0 in ((Var newVar), (newVar:vs))
  -- Nothing to replace
  replaceUnderscore (Var x) vs = ((Var x), vs)
  -- Replace within list
  replaceUnderscore (Comb f xs) vs = let rlist = replaceList xs vs in ((Comb f (fst rlist)), snd rlist)
  
  -- Replaces the "_" variable in a list of terms
  replaceList :: [Term] -> [VarName] -> ([Term], [VarName])
  replaceList [] vs = ([], vs)
  replaceList xs vs = convertType (replaceListAcc xs vs)
  -- replaces the "_" variable in each term in the list
  replaceListAcc :: [Term] -> [VarName] -> [(Term,[VarName])]
  replaceListAcc [] _ = []
  replaceListAcc (x:xs) vs = let y = replaceUnderscore x vs in (y):(replaceListAcc xs (vs ++ (snd y)))
  -- converts the return type of replaceListAcc into something more useful
  convertType :: (Eq b) => [(a,[b])] -> ([a],[b])
  convertType [] = ([],[])
  convertType (x:xs) = combine x (convertType xs)
  combine (a1, b1s) (a2s, b2s) = (a1:a2s, nub (b1s ++ b2s))

-- pretty (fst (rename (Rule (Var "A") [(Var "A"), (Comb "f" [Var "B", Var "_", Comb "true" []])]) []))
