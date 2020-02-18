module SLDResolution where

import Type
import Vars
import Substitutions
import Rename


-- Data representation of an SLD Tree
data SLDTree = Node Term [Maybe (Subst, [SLDTree])]

-- A Goal consisits of multiple terms so we return multiple sld trees?
sld :: Prog -> Goal -> [SLDTree]
sld prog goal = fst (sldWithVar vars p g) where
    vars = killDuplicates ((allVars prog) ++ (allVars goal)) 
    sldWithVar :: [VarName] -> Prog -> Goal -> (SLDTree, [VarName])
    sldWithVar vars prog Goal ts = fmap (sldTerm vars prog) ts
    -- Creates the sld tree with only on term.
    sldTerm :: [VarName] -> Prog -> Term -> (SLDTree, [VarName])

-- Takes a list of terms from a rule
fromRule :: Rule -> [Term]
fromRule (Rule t ts) = t:ts

-- Just renames a bunch of terms accordingly
renameTerms :: [Term] -> [VarName] -> ([Term], [Varname])
renameTerms [] vs = ([], vs)
renameTerms t:ts vs = (fromRule (fst result), snd result) where
    result = rename (Rule t ts) vs

-- Renames a list of rules accordingly
renameRules :: [Rule] -> [VarName] -> ([Rule], [VarName])
renameRules [] vs = ([], vs)
renameRules r:rs vs = ((firstR:finalRs), finalVs) where
    firstResult = rename r vs
    firstR = fst firstResult
    firstVs = snd firstResult
    finalResult = renameRules rs firstVs
    finalRs = fst finalResult
    finalVs = snd finalResult

-- Renames all variables within a programm accordingly
renameProgs :: Prog -> [VarName] -> (Prog, [VarName])
renameProgs (Prog rs) vs   = ((Prog renamedRs), finalVs) where
    result = renameRules rs
    renamedRs = fst result
    finalVs = snd result

rename :: Rule -> [VarName] -> (Rule, [VarName]) 