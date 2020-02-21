module SLDResolution where

import Type
import Vars
import Substitutions
import Rename
import Unification
import Data.Maybe
import Control.Monad.State.Lazy

-- Data representation of an SLD Tree
-- aka goal
data SLDTree = Node Goal [(Subst, SLDTree)]
  deriving Show

-- creates a SLD tree out of a program and a goal
sld :: Prog -> Goal -> SLDTree
sld program finalGoal = fst (runState (sldWithVar program noUnderscoreGoal) variables) where
    variables = (allVars program) ++ (allVars noUnderscoreGoal)
    noUnderscoreGoal = Goal (fst (runState (replaceList (fromGoal finalGoal)) (allVars finalGoal)))
    -- main function, that does the SLD resultion, tracks all currently used variables along the way
    sldWithVar :: Prog -> Goal -> State [VarName] SLDTree
    sldWithVar pr goal = state (\vars -> let (appliedProgramm, finalVars) = runState ((rename pr) >>= (programToList goal)) vars in
            ((Node goal appliedProgramm), finalVars)) where
         -- searches through all rules and tries to apply them to the first term
        programToList :: Goal -> Prog -> State [VarName] [(Subst, SLDTree)]
        programToList g (Prog rs) = state (\s -> (catMaybes (fst (st s)), snd (st s))) where
            st = runState (listState rs (resolutionStep g (Prog rs)))
        -- Takes a term to pattern match
        -- Takes a rule to apply (try pattern matching)
        -- Takes a program and a goal with which to continue further
        resolutionStep :: Goal -> Prog -> Rule -> State [VarName] (Maybe (Subst, SLDTree))
        resolutionStep (Goal []) _ _ = pure Nothing
        resolutionStep (Goal (x:xs)) p (Rule t ts) = if (isNothing subst) then pure Nothing
            else state g where
            g vs = let (tree, vsAfter) = runState (sldWithVar p (Goal (apply (fromJust subst) (ts ++ xs)))) vs in
                (Just (fromJust subst, tree), vsAfter)
            subst = unify x t

-- State b c = state b -> (c, b)
-- listState :: [a] -> (a -> State b c) -> (State b [c])
-- a = Term, b = ([VarName], (Goal, Goal))

type Strategy = SLDTree -> [Subst]

-- depth-first search
dfs :: Strategy
dfs (Node (Goal []) _) = [empty]
dfs (Node _ []) = []
dfs (Node goal ((s, tree):ms)) = (fmap (\x -> compose x s) (dfs tree)) ++ dfs (Node goal ms)

-- breadth-first search
bfs :: Strategy
bfs tree = fst (bfsAcc [(tree, empty)]) where
  -- increses the depth of the search one step at a time
  bfsAcc :: [(SLDTree, Subst)] -> ([Subst], [(SLDTree, Subst)])
  bfsAcc [] = ([], [])
  bfsAcc s = let nextStep = foldr concatPair ([],[]) (fmap oneStep s)
             in concatPair (fst nextStep, []) (bfsAcc (snd nextStep))
  concatPair :: ([a], [b]) -> ([a], [b]) -> ([a], [b])
  concatPair (a1, b1) (a2, b2) = (a1 ++ a2, b1 ++ b2)
  -- does one step of the SLD-Resolution
  oneStep :: (SLDTree, Subst) -> ([Subst], [(SLDTree, Subst)])
  oneStep ((Node (Goal []) _), s) = ([s],[])
  oneStep ((Node _ []), _) = ([],[])
  oneStep (Node goal ((s1, tree1):ms), s2) = let rest = oneStep (Node goal ms, s2) 
                                                  in (fst rest, (tree1, compose s1 s2):(snd rest))

-- iterative depth-first search
idfs :: Strategy
idfs tree1 = idfsAcc 0 tree1 where
  idfsAcc :: Int -> Strategy
  idfsAcc i tree = let (sol, b) = bdfs i tree
                  in if b then sol ++ (idfsAcc (i + 1) tree) else sol
  -- bounded depth-first search, returns, if there could be more solutions at a higher depth
  bdfs :: Int -> SLDTree -> ([Subst], Bool)
  bdfs i _ | i < 0 = ([], True)
  bdfs i (Node (Goal []) _) | i == 0 = ([empty], False)
                     | i < 0 = ([], False)
  bdfs _ (Node _ []) = ([], False)
  bdfs i (Node goal ((s, tree):ms)) = let sol = (bdfs (i - 1) tree)
    in (\(a, b) (c, d) -> (a ++ c, b || d)) (fmap (\x -> compose x s) (fst sol), snd sol) (bdfs i (Node goal ms))

-- solves a goal with a strategie using all rules from a program
solve :: Strategy -> Prog -> Goal -> [Subst]
solve s p g = s (sld p g) 

{-
module SLDResolution where

import Type
import Vars
import Substitutions
import Rename
import Unification
import Data.Maybe

-- Data representation of an SLD Tree
-- aka goal
data SLDTree = Node [Term] [Maybe (Subst, SLDTree)]
  deriving Show

-- creates a SLD tree out of a program and a goal
sld :: Prog -> Goal -> SLDTree
sld program finalGoal = fst (sldWithVar variables program noUnderscoreGoal) where
    variables = killDuplicates ((allVars program) ++ (allVars noUnderscoreGoal)) 
    noUnderscoreGoal = Goal (fst (replaceList (fromGoal finalGoal) (allVars finalGoal)))
    -- main function, that does the SLD resultion, tracks all currently used variables along the way
    sldWithVar :: [VarName] -> Prog -> Goal -> (SLDTree, [VarName])
    sldWithVar vars prog (Goal terms) = ((Node terms appliedProgramm), finalVars) where
        -- Renaming the Program
        progRenamedResult = rename prog vars
        progRenamed = fst progRenamedResult
        varsAfterProg = snd progRenamedResult
        -- applying the whole Programm to the Goal
        resultFinished = searchGoal varsAfterProg (Goal terms) (Goal []) progRenamed
        appliedProgramm = fst resultFinished
        finalVars       = snd resultFinished
        -- searches for all possible substitutions
        searchGoal :: [VarName] -> Goal -> Goal -> Prog -> ([Maybe (Subst, SLDTree)], [VarName])
        searchGoal vs (Goal []) _ _      = ([], vs)
        searchGoal vs (Goal (t:ts)) (Goal bs) (Prog rs) = (finalList, finalVs) where
            -- Creating tree for the first term in goal
            resultFirst = programToList vs t (Goal (bs ++ ts)) rs (Prog rs)
            vsFirst     = snd resultFirst
            -- Creating tree for all others recursivly
            resultOther = searchGoal vsFirst (Goal ts) (Goal (bs ++ [t])) (Prog rs)
            finalList   = (fst resultFirst) ++ (fst resultOther)
            finalVs     = snd resultOther
        -- searches through all rules and tries to apply them to a term
        programToList :: [VarName] -> Term -> Goal -> [Rule] -> Prog -> ([Maybe (Subst, SLDTree)], [VarName])
        programToList vs _ _ [] _ = ([], vs)
        programToList vs goalTerm goal (r:rs) p = ((treeFromR:restList), finalVs) where
            -- Applying the first rule with a given program
            resultFirst       = ruleToTree vs goalTerm goal r p
            treeFromR         = fst resultFirst
            firstVs           = snd resultFirst
            -- Applying all other Rules recursively
            restListResult    = programToList firstVs goalTerm goal rs p
            restList          = fst restListResult
            finalVs           = snd restListResult
        
        -- Takes a term to pattern match
        -- Takes a rule to apply (try pattern matching)
        -- Takes a program with whom to continue in the rest of the Term
        ruleToTree :: [VarName] -> Term -> Goal -> Rule -> Prog -> (Maybe (Subst, SLDTree), [VarName])
        ruleToTree vs goalTerm (Goal xs) (Rule t ts) p = if (isNothing subst) then (Nothing, vs)
            else (Just (fromJust subst, tree), vsAfter) where
            subst = unify goalTerm t
            result = sldWithVar vs p (Goal (apply (fromJust subst) (ts ++ xs)))
            tree = fst result
            vsAfter = snd result


type Strategy = SLDTree -> [Subst]

-- depth-first search
dfs :: Strategy
dfs (Node [] _) = [empty]
dfs (Node _ []) = []
dfs (Node ts (Nothing:ms)) = dfs (Node ts ms)
dfs (Node ts ((Just (s, tree)):ms)) = (fmap (\x -> compose x s) (dfs tree)) ++ dfs (Node ts ms)

-- breadth-first search
bfs :: Strategy
bfs tree = fst (bfsAcc [(tree, empty)]) where
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
  oneStep ((Node _ []), _) = ([],[])
  oneStep (Node ts (Nothing:ms), s) = oneStep (Node ts ms, s)
  oneStep (Node ts ((Just (s1, tree1)):ms), s2) = let rest = oneStep (Node ts ms, s2) 
                                                  in (fst rest, (tree1, compose s1 s2):(snd rest))

-- iterative depth-first search
idfs :: Strategy
idfs tree1 = idfsAcc 0 tree1 where
  idfsAcc :: Int -> Strategy
  idfsAcc i tree = let (sol, b) = bdfs i tree
                  in if b then sol ++ (idfsAcc (i + 1) tree) else sol
  -- bounded depth-first search, returns, if there could be more solutions at a higher depth
  bdfs :: Int -> SLDTree -> ([Subst], Bool)
  bdfs i _ | i < 0 = ([], True)
  bdfs i (Node [] _) | i == 0 = ([empty], False)
                     | i < 0 = ([], False)
  bdfs _ (Node _ []) = ([], False)
  bdfs i (Node ts (Nothing:ms)) = bdfs i (Node ts ms)
  bdfs i (Node ts ((Just (s, tree)):ms)) = let sol = (bdfs (i - 1) tree)
                                          in (\(a, b) (c, d) -> (a ++ c, b || d)) (fmap (\x -> compose x s) (fst sol), snd sol) (bdfs i (Node ts ms))

-- solves a goal with a strategie using all rules from a program
solve :: Strategy -> Prog -> Goal -> [Subst]
solve s p g = s (sld p g)
-}