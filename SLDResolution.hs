module SLDResolution(Strategy, dfs, bfs, idfs, solve) where
-- NOT exporting the SLDTree constructor, cause it doesnt matter

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
sld :: Prog -> Strategy -> Goal -> SLDTree
sld program strategy finalGoal = fst (runState (sldWithVar program noUnderscoreGoal strategy) variables) where
    variables = (allVars program) ++ (allVars noUnderscoreGoal)
    noUnderscoreGoal = Goal (fst (runState (replaceList (fromGoal finalGoal)) (allVars finalGoal)))
    -- main function, that does the SLD resultion, tracks all currently used variables along the way
    sldWithVar :: Prog -> Goal -> Strategy -> State [VarName] SLDTree
    sldWithVar pr goal strat = state (\vars -> let (appliedProgramm, finalVars) = runState ((rename pr) >>= (programToList goal strat)) vars in
            ((Node goal appliedProgramm), finalVars)) where
         -- searches through all rules and tries to apply them to the first term
        programToList :: Goal -> Strategy -> Prog -> State [VarName] [(Subst, SLDTree)]
        programToList g stra (Prog rs) = state (\s -> (catMaybes (fst (st s)), snd (st s))) where
            st = runState (listState rs (resolutionStep g (Prog rs) stra))
        -- Takes a term to pattern match
        -- Takes a rule to apply (try pattern matching)
        -- Takes a program and a goal with which to continue further
        resolutionStep :: Goal -> Prog -> Strategy ->  Rule -> State [VarName] (Maybe (Subst, SLDTree))
        resolutionStep (Goal []) _ _  _ = pure Nothing
        resolutionStep (Goal (x:xs)) p stra (Rule t ts) = if (isNothing subst) then pure Nothing
            else state g where
            g vs = let (tree, vsAfter) = runState (sldWithVar p (Goal (apply (fromJust subst) (ts ++ xs))) stra) vs in
                (Just (fromJust subst, tree), vsAfter)
            subst = unify x t

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
solve s p g = s (sld p s g) 
