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
    sldWithVar vars prog (Goal ts) = SLDTree tsRenamed appliedProgramm where
        -- Renaming Goal / Terms in Goals
        tsRenameResult = renameTerms ts vars
        tsRenamed = fst tsRenameResult
        varsFirst = snd tsRenameResult
        -- Renaming the Program after Terms where renamed
        progRenamedResult = renameProgs prog varsFirst
        progRenamed = fst progRenamedResult
        varsAfterProg = snd progRenamedResult
        -- Takes a term to pattern match
        -- Takes a rule to apply (try pattern matching)
        -- Takes a program with whom to continue in the rest of the Term
        ruleToTree :: Term -> Rule -> Prog -> Maybe (Subst, SLDTree)
        ruleToTree goalTerm (Rule t ts) prog = subst >>= (\s -> Maybe (s, tree)) where
            subst = unify goalTerm t
            tree = sld prog (Goal ts)

type Strategy = SLDTree -> [Subst]

dfs :: Strategy
dfs (Node [] _) = empty
dfs (Node _ []) = []
dfs (Node ts (Nothing:ms)) = dfs (Node ts ss)
dfs (Note ts ((Just (s, sld)):ms) = (fmap (compose s) (dfs sld)) ++ dfs (Node ts ms)

bfs :: Strategy
bfs sld = bfsAcc [(sld, empty)] where
  bfsAcc :: [(SLDTree, Subst)] -> [Subst]
  bfsAcc s = foldr (++) [] (fmap oneStep s)
  oneStep :: (SLDTree, Subst) -> [(SLDTree, Subst)]
  oneStep ((Node [] _), s) = [s]
  oneStep ((Node _ []), s) = []
  oneStep ((Node ts (Nothing:ms)), s) = oneStep (Node 

solve :: Strategy -> Prog -> Goal -> [Subst]
solve s p g = s (sld p g) 