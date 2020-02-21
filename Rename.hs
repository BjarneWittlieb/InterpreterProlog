module Rename where

import Data.List
import Type
import Vars
import Substitutions
import Control.Monad.State.Lazy

-- rename will rename all variables in the type, while giving each "_" variable a unique name
-- a list of varNames, that shouldn't be used in the renaming process can be inputted and the function will return a superset
-- of that list, that will contain all the new substituted variables
class Renameable a where
  rename :: a -> State [VarName] a

-- Replaces the "_" variable with other fitting variables, the variables in the specified list won't be used
replaceUnderscore :: Term -> State [VarName] Term
-- The actual replacing
replaceUnderscore (Var "_") = state (\vs -> let newVar = head (filter (\x -> (not (elem x vs))) freshVars) in (Var newVar, newVar:vs))
-- Nothing to replace
replaceUnderscore (Var x) = state (\vs -> ((Var x), vs))
-- Replace within list
replaceUnderscore (Comb f xs) = state (\vs -> let rlist = runState (replaceList xs) vs in ((Comb f (fst rlist)), snd rlist))
  
replaceList :: [Term] -> State [VarName] [Term]
replaceList ys = foldr (flip (>>=)) (pure []) (fmap (\t -> (\xs -> state (\vs -> let (x, v) = runState (replaceUnderscore t) vs in (x:xs, v)))) ys)

-- Renames all variables in a rule, variables from the specified list won't be used
-- returns the changed Rule and a superset of the input list, including all variables in the changed Rule
instance Renameable Rule where 
  rename r = state f where
    f vnames = (apply (multiple ruleVars (fmap (\x -> (Var x)) substVars)) rule, nub(vnames ++ substVars)) where
      -- replaces all underscore variables first
      (rule, vars) = runState (replaceUnderscoreRule r) vnames
      -- a list of all variables in the Rule
      ruleVars = allVars rule
      -- a list of all variables, that will be used in the substituted Rule
      substVars = take (length ruleVars) (filter (\x -> not (elem x vars)) freshVars)
      -- replaces all underscore variables in a Rule
      replaceUnderscoreRule :: Rule -> State [VarName] Rule
      replaceUnderscoreRule (Rule t ts) = (replaceUnderscore t) >>= (\x -> (state (\vs -> let (xs, v) = runState (replaceList ts) vs in (Rule x xs, v))))


instance Renameable Term where
  rename t = state (\vs -> let (Rule x _, ys) = runState (rename (Rule t [])) vs in (x, ys))

instance Renameable Goal where
  rename (Goal ts) = state (\vs -> let (ts', v) = runState (rename ts) vs in (Goal ts', v))

instance Renameable Prog where
  rename (Prog rs) = state (\vs -> let (rs', v) = runState (rename rs) vs in (Prog rs', v))

instance (Renameable a) => Renameable [a] where
  rename xs = foldr (flip (>>=)) (pure []) (fmap (\x -> (\ys -> state (\vs -> let (y, v) = runState (rename x) vs in (y:ys, v)))) xs)

-- helper functions to get the inside of Prog, Goal, Rule
fromProg :: Prog -> [Rule]
fromProg (Prog rs) = rs

fromGoal :: Goal -> [Term]
fromGoal (Goal ts) = ts

fromRule :: Rule -> [Term]
fromRule (Rule t ts) = t:ts  

{-module Rename where

import Data.List
import Type
import Vars
import Substitutions

-- rename will rename all variables in the type, while giving each "_" variable a unique name
-- a list of varNames, that shouldn't be used in the renaming process can be inputted and the function will return a superset
-- of that list, that will contain all the new substituted variables
class Renameable a where
  rename :: a -> [VarName] -> (a, [VarName])

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
replaceList ys vs = convertType (replaceListAcc ys vs) where
    -- replaces the "_" variable in each term in the list
    replaceListAcc :: [Term] -> [VarName] -> [(Term,[VarName])]
    replaceListAcc [] _ = []
    replaceListAcc (x:xs) vs1 = let y = replaceUnderscore x vs1 in (y):(replaceListAcc xs (vs1 ++ (snd y)))
    -- converts the return type of replaceListAcc into something more useful
    convertType :: (Eq b) => [(a,[b])] -> ([a],[b])
    convertType [] = ([],[])
    convertType (x:xs) = combine x (convertType xs)
    combine (a1, b1s) (a2s, b2s) = (a1:a2s, nub (b1s ++ b2s))

 


-- Renames all variables in a rule, variables from the specified list won't be used
-- returns the changed Rule and a superset of the input list, including all variables in the changed Rule
instance Renameable Rule where 
  rename r variables = (apply (multiple ruleVars (fmap (\x -> (Var x)) substVars)) rule, nub(variables ++ substVars)) where
    -- replaces all underscore variables first
    (rule, vars) = (replaceUnderscoreRule r variables)
    -- a list of all variables in the Rule
    ruleVars = allVars rule
    -- a list of all variables, that will be used in the substituted Rule
    substVars = take (length ruleVars) (filter (\x -> not (elem x vars)) freshVars)
    -- replaces all underscore variables in a Rule
    replaceUnderscoreRule :: Rule -> [VarName] -> (Rule, [VarName])
    replaceUnderscoreRule (Rule t ts) vs = let (t', xs) = replaceUnderscore t (vs ++ (allVars (Rule t ts)))
                                           in let (ts', ys) = replaceList ts xs
                                              in (Rule t' ts', ys)

instance Renameable Term where
  rename t vs = let (Rule x _, ys) = (rename (Rule t []) vs) in (x, ys)

instance Renameable Goal where
  rename (Goal []) vs = (Goal [], vs)
  rename (Goal (t:ts)) vs = let (Rule x xs, ys) = (rename (Rule t ts) vs) in (Goal (x:xs), ys)

instance Renameable Prog where
  rename (Prog []) vs = (Prog [], vs)
  rename (Prog (r:rs)) vs = (Prog ((fst firstRule):(fromProg (fst rest))), nub ((snd firstRule) ++ (snd rest))) where
    firstRule = rename r vs
    rest = rename (Prog rs) vs

instance (Renameable a) => Renameable [a] where
  rename [] vs      = ([], vs)
  rename (x:xs) vs  = ((firstRenamed:otherRenamed), finalVs) where
    -- First renaming the first one and storing new Variables
    firstResult   = rename x vs
    firstRenamed  = fst firstResult
    firstVs       = snd firstResult
    -- Then renaming the other ones recursively
    otherResult   = rename xs firstVs
    otherRenamed  = fst otherResult
    finalVs       = snd otherResult

-- helper functions to get the inside of Prog, Goal, Rule
fromProg :: Prog -> [Rule]
fromProg (Prog rs) = rs

fromGoal :: Goal -> [Term]
fromGoal (Goal ts) = ts

fromRule :: Rule -> [Term]
fromRule (Rule t ts) = t:ts  
-}