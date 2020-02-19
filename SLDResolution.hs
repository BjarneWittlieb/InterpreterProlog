module SLDResolution where

import Type
import Vars
import Substitutions
import Rename
import Unification


-- Data representation of an SLD Tree
data SLDTree = Node [Term] [Maybe (Subst, SLDTree)]
  deriving Show

-- A Goal consisits of multiple terms so we return multiple sld trees?
sld :: Prog -> Goal -> SLDTree
sld prog goal = fst (sldWithVar vars prog goal) where
    vars = killDuplicates ((allVars prog) ++ (allVars goal)) 
    sldWithVar :: [VarName] -> Prog -> Goal -> (SLDTree, [VarName])
    sldWithVar vars prog (Goal ts) = ((Node tsRenamed appliedProgramm), finalVars) where
        -- Renaming Goal / Terms in Goals
        tsRenameResult = rename ts vars
        tsRenamed = fst tsRenameResult
        varsFirst = snd tsRenameResult
        -- Renaming the Program after Terms where renamed
        progRenamedResult = rename prog varsFirst
        progRenamed = fst progRenamedResult
        varsAfterProg = snd progRenamedResult
        -- applying the whole Programm to the Goal

        resultFinished = searchGoal vars (Goal ts) prog
        appliedProgramm = fst resultFinished
        finalVars       = snd resultFinished

        searchGoal :: [VarName] -> Goal -> Prog -> ([Maybe (Subst, SLDTree)], [VarName])
        searchGoal vs (Goal []) prog      = ([], vs)
        searchGoal vs (Goal (t:ts)) (Prog rs) = (finalList, finalVs) where
            -- Creating tree for the first term in goal
            resultFirst = programToList vs t rs (Prog rs)
            vsFirst     = snd resultFirst
            -- Creating tree for all others recursivly
            resultOther = searchGoal vsFirst (Goal ts) (Prog rs)
            finalList   = (fst resultOther) ++ (fst resultFirst)
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
        ruleToTree :: [VarName] -> Term -> Rule -> Prog -> (Maybe (Subst, SLDTree), [VarName])

        ruleToTree vs goalTerm (Rule t ts) prog = (subst >>= (\s -> Just (s, tree)), vsAfter) where
            subst = unify goalTerm t
            result = sldWithVar vs prog (Goal ts)
            tree = fst result
            vsAfter = snd result


type Strategy = SLDTree -> [Subst]

-- depth-first search
dfs :: Strategy
dfs sld = filterVars sld (dfs2 sld) where
  dfs2 (Node [] _) = [empty]
  dfs2 (Node _ []) = []
  dfs2 (Node ts (Nothing:ms)) = dfs2 (Node ts ms)
  dfs2 (Node ts ((Just (s, sld)):ms)) = (fmap (compose s) (dfs2 sld)) ++ dfs2 (Node ts ms)

-- breadth-first search
bfs :: Strategy
bfs sld = filterVars sld (fst (bfsAcc [(sld, empty)]))
  -- increses the depth of the search one step at a time
bfsAcc :: [(SLDTree, Subst)] -> ([Subst], [(SLDTree, Subst)])
bfsAcc [] = ([], [])
bfsAcc s = let nextStep = foldr concatPair ([],[]) (fmap oneStep s)
           in concatPair (fst nextStep, []) (bfsAcc (snd nextStep))
concatPair :: ([a], [b]) -> ([a], [b]) -> ([a], [b])
concatPair (a1, b1) (a2, b2) = (a1 ++ a2, b1 ++ b2)
-- does one step of the SLD-Resolution
oneStep :: (SLDTree, Subst) -> ([Subst], [(SLDTree, Subst)])
oneStep ((Node [] _), s) = ([s],[])
oneStep ((Node _ []), s) = ([],[])
oneStep (Node ts (Nothing:ms), s) = oneStep (Node ts ms, s)
oneStep (Node ts ((Just (s1, sld)):ms), s2) = let rest = oneStep (Node ts ms, s2) 
                                                in (fst rest, (sld, compose s2 s1):(snd rest))

-- iterative depth-first search
idfs :: Strategy
idfs sld = filterVars sld (idfsAcc 0 sld) where
  idfsAcc :: Int -> Strategy
  idfsAcc i sld = let (sol, b) = bdfs i sld
                  in if b then sol ++ (idfsAcc (i + 1) sld) else sol
  -- bounded depth-first search, returns, if there could be more solutions at a higher depth
  bdfs :: Int -> SLDTree -> ([Subst], Bool)
  bdfs i _ | i < 0 = ([], True)
  bdfs i (Node [] _) | i == 0 = ([empty], False)
                     | i < 0 = ([], False)
  bdfs _ (Node _ []) = ([], False)
  bdfs i (Node ts (Nothing:ms)) = bdfs i (Node ts ms)
  bdfs i (Node ts ((Just (s, sld)):ms)) = let sol = (bdfs (i - 1) sld)
                                          in (\(a, b) (c, d) -> (a ++ c, b || d)) (fmap (compose s) (fst sol), snd sol) (bdfs i (Node ts ms))

filterVars :: SLDTree -> [Subst] -> [Subst]
filterVars (Node ts _) s = fmap (restrictTo (allVars (Goal ts))) s

solve :: Strategy -> Prog -> Goal -> [Subst]
solve s p g = s (sld p g) 
