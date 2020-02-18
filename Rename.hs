module Rename where

import Type
import Vars


-- Replaces the "_" Variable with other fitting variables
replaceUnderscore :: Term -> Term
replaceUnderscore t = fst (replaceAcc t (allVars t)) where
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
        rlist = replaceList xs vs
        -- Same as replaceAcc but for Termlists instead
        replaceList :: [Term] -> [VarName] -> ([Term], [VarName])
        replaceList [] vs = ([], vs)
        replaceList (x:xs) vs = ((firstTerm:finalTerms), finalVs) where
            -- The result of the term replacement from the first term of the list
            firstResult :: (Term, [VarName])
            firstResult = (replaceAcc x vs)
            firstVs :: [VarName]
            firstVs = snd firstResult
            firstTerm :: Term
            firstTerm = fst firstResult
            -- The result from replacing all other terms of the list
            otherResult ::  ([Term], [VarName])
            otherResult = replaceList xs firstVs
            finalTerms :: [Term]
            finalTerms = fst otherResult
            finalVs :: [VarName]
            finalVs = snd otherResult

-- What varnames not to pick for the renaming of the variable in given rule
-- Returns a tuple containing the renamed rule and the superset of the fiven varname containing
--  containing also the variable names
rename :: [VarName] -> Rule -> Rule