module Rename where

import Type
import Vars


-- Replaces the "_" Variable with other fitting variables
replaceUnderscore :: Term -> [VarName] -> (Term, [VarName])
replaceUnderscore t vs = replaceAcc t ((allVars t) ++ vs) where
    -- Replaces a "_" with a free variable and returns a tuple containing
    -- the new Term and the new List with variables that are now used in the term
    replaceAcc :: Term -> [VarName] -> (Term, [VarName])
    -- The actual replacing
    replaceAcc (Var "_") vs = ((Var s), (s:vs))  where
        s = (filter (\x -> (not (elem x vs))) freshVars)!!0
    -- Nothing to replace
    replaceAcc (Var x) vs = ((Var x), vs)
    -- Replace within list
    replaceAcc (Comb f xs) vs = ((Comb f (fst rlist)), snd rlist) where
        rlist = convertType (replaceList xs vs)
        -- Same as replaceAcc but for Termlists instead
        replaceList :: [Term] -> [VarName] -> [(Term, [VarName])]
        replaceList [] vs = []
        replaceList (x:xs) vs = let y = replaceAcc x vs in (y):(replaceList xs (vs ++ (snd y)))
        -- converts the output of replaceList into a type, that is useful for replaceAcc
        convertType :: [(a,[b])] -> ([a],[b])
        convertType [] = ([],[])
        convertType (x:xs) = combine x (convertType xs)
        combine (a1, b1s) (a2s, b2s) = (a1:a2s, b1s ++ b2s)

-- What varnames not to pick for the renaming of the variable in given rule
-- Returns a tuple containing the renamed rule and the superset of the fiven varname containing
--  containing also the variable names
-- rename :: Rule -> [VarName] -> (Rule, [VarName])

--(Comb "." [Var "_", Comb "true" [], Comb "j" [Var "_"]])