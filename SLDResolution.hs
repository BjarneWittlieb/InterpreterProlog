module SLDResolution where

import Type
import Vars
import Substitutions
import Rename


-- Data representation of an SLD Tree
data SLDTree = Node [Term] [Maybe (Subst, SLDTree)]

-- A Goal consisits of multiple terms so we return multiple sld trees?
sld :: Prog -> Goal -> SLDTree
sld prog goal = fst (sldWithVar vars p g) where
    vars = killDuplicates ((allVars prog) ++ (allVars goal)) 
    sldWithVar :: [VarName] -> Prog -> Goal -> (SLDTree, [VarName])
    sldWithVar vars prog goal = ((SLDTree tsRenamed appliedProgramm), finalVars) where
        -- Renaming Goal / Terms in Goals
        tsRenameResult = renameTerms ts vars
        tsRenamed = fst tsRenameResult
        varsFirst = snd tsRenameResult
        -- Renaming the Program after Terms where renamed
        progRenamedResult = renameProgs prog varsFirst
        progRenamed = fst progRenamedResult
        varsAfterProg = snd progRenamedResult
        -- applying the whole Programm to the Goal
        resultFinnished = searchGoal vars goal prog
        appliedProgramm = fst resultFinnished
        finalVars       = snd resultFinnished

        searchGoal :: [VarName] -> Goal -> Prog -> ([Maybe (Subst, SLDTree)], [VarName])
        searchGoal :: vs (Goal []) prog      = ([], vs)
        searchGoal :: vs (Goal t:ts) Prog rs = (finalList, finalVs) where
            -- Creating tree for the first term in goal
            resultFirst = programToList vs t rs (Prog rs)
            listFirs    = fst resultFirst
            vsFirst     = snd resultFirst
            -- Creating tree for all others recursivly
            resultOther = searchGoal vsFirst (Goal ts) (Prog rs)
            finalList   = (fst resultOther) ++ (resultFirst)
            finalVs     = snd resultOther

        programToList :: [VarName] -> Term -> [Rule] -> Prog -> ([Maybe (Subst, SLDTree)], [VarName])
        programToList vs goalTerm []     prog = ([], vs)
        programToList vs goalTerm (r:rs) prog = ((treeFromR:restList), finalVs) where
            -- Applying the first rule with a given program
            resultFirst       = ruleToTree vs goalTerm r prog
            treeFromR         = fst resultFirst
            firstVs           = snd resultFirst
            -- Applying all other Rules recursively
            restListResult    = programToList firstVs goalTerm rs prog
            restList          = fst restListResult
            finalVs           = snd restListResult
        
        -- Takes a term to pattern match
        -- Takes a rule to apply (try pattern matching)
        -- Takes a program with whom to continue in the rest of the Term
        ruleToTree :: [VarName] -> Term -> Rule -> Prog -> (Maybe (Subst, SLDTree), VarName)
        ruleToTree vs goalTerm (Rule t ts) prog = ((subst >>= (\s -> Just (s, tree)), vsAfter) where
            subst = unify goalTerm t
            result = sldWithVar vs prog (Goal ts)
            tree = fst result
            vsAfter = snd result