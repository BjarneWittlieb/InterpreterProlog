module Rename(Renameable, rename, replaceList, fromGoal, fromProg, fromRule, listState, repeatState) where

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
replaceUnderscore (Var "_") = state (\vs -> (Var (head vs), tail vs))
-- Nothing to replace
replaceUnderscore (Var x) = pure (Var x)
-- Replace within list
replaceUnderscore (Comb f xs) = state (\vs -> let rlist = runState (replaceList xs) vs in ((Comb f (fst rlist)), snd rlist))
  
replaceList :: [Term] -> State [VarName] [Term]
replaceList ys = listState ys replaceUnderscore

listState :: [a] -> (a -> State b c) -> (State b [c])
listState xs st = foldl (>>=) (pure []) (fmap (\x -> (\ys -> state (\zs -> let (y, z) = runState (st x) zs in (ys ++ [y], z)))) xs)

repeatState ::  (a -> Bool) -> (b -> State a b) -> b -> State a b
repeatState f g x = state h where
  h y | f y = runState (g x) y
      | otherwise = (x, y)

-- Renames all variables in a rule, variables from the specified list won't be used
-- returns the changed Rule and a superset of the input list, including all variables in the changed Rule
instance Renameable Rule where 
  rename r = state f where
    f vnames = (apply (multiple ruleVars (fmap (\x -> (Var x)) (fst substVars))) rule, snd substVars) where
      -- replaces all underscore variables first
      (rule, vars) = runState (replaceUnderscoreRule r) (filter (\x -> not (elem x (allVars r))) vnames)
      -- a list of all variables in the Rule
      ruleVars = allVars rule
      -- a list of all variables, that will be used in the substituted Rule
      substVars = splitAt (length ruleVars) vars
      
-- replaces all underscore variables in a Rule
replaceUnderscoreRule :: Rule -> State [VarName] Rule
replaceUnderscoreRule (Rule t ts) = (replaceUnderscore t) >>= (\x -> (state (\vs -> let (xs, v) = runState (replaceList ts) vs in (Rule x xs, v))))

instance Renameable Term where
  rename t = state (\vs -> let (Rule x _, ys) = runState (rename (Rule t [])) vs in (x, ys))

instance Renameable Goal where
  rename (Goal ts) = state (\vs -> let (Rule _ ts', v) = runState (rename (Rule (Comb "" []) ts)) vs in (Goal ts', v))

instance Renameable Prog where
  rename (Prog rs) = state (\vs -> let (rs', v) = runState (rename rs) vs in (Prog rs', v))

instance (Renameable a) => Renameable [a] where
  rename xs = listState xs rename

-- helper functions to get the inside of Prog, Goal, Rule
fromProg :: Prog -> [Rule]
fromProg (Prog rs) = rs

fromGoal :: Goal -> [Term]
fromGoal (Goal ts) = ts

fromRule :: Rule -> [Term]
fromRule (Rule t ts) = t:ts  