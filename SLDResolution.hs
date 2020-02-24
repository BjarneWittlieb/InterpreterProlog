module SLDResolution(Strategy, dfs, bfs, idfs, solve, simplify, sld) where
-- NOT exporting the SLDTree constructor, cause it doesnt matter

import Type
import Vars
import Substitutions
import Rename
import Unification

import Data.Maybe
import Control.Monad.State.Lazy
import Text.Read


-- Data representation of an SLD Tree
data SLDTree = Node Goal [(Subst, SLDTree)]
  deriving Show

-- creates a SLD tree out of a program and a goal
sld :: Strategy -> Prog -> Goal -> SLDTree
sld strategy program finalGoal = resolution (filter (\x -> not (elem x (allVars finalGoal))) freshVars) strategy program noUnderscoreGoal where
    noUnderscoreGoal = Goal (fst (runState (replaceList (fromGoal finalGoal)) (filter (\x -> not (elem x (allVars finalGoal))) freshVars)))
    -- main function, that does the SLD resultion
    resolution :: [VarName] -> Strategy -> Prog -> Goal -> SLDTree
    resolution vars strat pr goal = Node goal (applyRules (snd renamedProgram) strat (fst renamedProgram) goal) where
        renamedProgram = (runState (rename pr) vars)

        -- Applies the Rules of the Program to the goal
        applyRules :: [VarName] -> Strategy -> Prog -> Goal -> [(Subst, SLDTree)]
        -- Higher order term 'call'
        applyRules vs stra p (Goal ((Comb "call" ((Comb f xs):ys)):ts)) = let s = (solve stra p (Goal [Comb f (xs ++ ys)])) in
            (fmap (\sub -> (sub, resolution vs stra p (Goal ts))) s)
        -- Higher order term '\+', the finite not
        applyRules vs stra p (Goal ((Comb "\\+" [x]):ts)) = case solve stra p (Goal [Comb "call" [x]]) of
                                                              [] -> [(empty, resolution vs stra p (Goal ts))]
                                                              _ -> []
        -- Higher order term 'findall'
        applyRules vs stra p (Goal ((Comb "findall" [x, y, z]):ts)) = 
          case unify z (listToTerm (fmap (((flip apply) x).(simplify (allVars (Goal [x, y, z])))) (solve stra p (Goal [Comb "call" [y]])))) of
            Nothing -> []
            Just s -> [(s, resolution vs stra p (apply s (Goal ts)))]  
        -- The evalutaion of terms 'is'
        applyRules vs stra p (Goal ((Comb "is" [x, y]):ts)) =
          case ((eval y) >>= (\s -> pure (Comb (show s) []))) >>= (unify x) of
            Nothing -> []
            Just s -> [(s, resolution vs stra p (apply s (Goal ts)))]
        -- Boolean eval predicates
        applyRules vs stra p (Goal ((Comb c [x, y]):ts)) | fst (isComparison c) = 
          case pure (snd (isComparison c)) <*> (eval x) <*> (eval y) of
            Just True -> [(empty, resolution vs stra p (Goal ts))]
            _ -> []
        applyRules vs stra p (Goal ((Comb "=" [x, y]):ts)) =
          case unify x y of
            Nothing -> []
            Just s -> [(s, resolution vs stra p (apply s (Goal ts)))]
        applyRules vs stra p (Goal ((Comb "\\=" [x, y]):ts)) = applyRules vs stra p (Goal ((Comb "\\+" [(Comb "=" [x, y])]):ts))
        -- Normal case
        applyRules vs stra (Prog rs) g = catMaybes (fmap (resolutionStep vs stra (Prog rs) g) rs) 

        -- Converts a list of terms into the prolog predicate for lists with terms as entries as before respectively
        listToTerm :: [Term] -> Term
        listToTerm [] = Comb "[]" []
        listToTerm (t:ts) = Comb "." [t, listToTerm ts]
        -- This method pretty much is the same as before
        resolutionStep :: [VarName] -> Strategy -> Prog -> Goal -> Rule -> Maybe (Subst, SLDTree)
        resolutionStep _ _ _ (Goal []) _ = Nothing
        resolutionStep vs stra p (Goal (t:t')) (Rule t1 ts) = let subst = unify t t1 in 
            if (isNothing subst) then Nothing
                                 else Just (fromJust subst, resolution vs stra p (Goal (apply (fromJust subst) (ts ++ t'))))


-- Evaluates an arethmetic term if possible
eval :: Term -> Maybe Int
eval (Comb "+" [x, y]) = pure (+) <*> (eval x) <*> (eval y)
eval (Comb "-" [x, y]) = pure (-) <*> (eval x) <*> (eval y)
eval (Comb "*" [x, y]) = pure (*) <*> (eval x) <*> (eval y)
eval (Comb "div" [x, y]) = pure (div) <*> (eval x) <*> (eval y)
eval (Comb "mod" [x, y]) = pure (mod) <*> (eval x) <*> (eval y)
eval (Comb num []) = (readMaybe num) :: Maybe Int
eval _ = Nothing

isComparison :: String -> (Bool, Int -> Int -> Bool)
isComparison "=:=" = (True, (==)) 
isComparison "=\\=" = (True, (/=))
isComparison "<" = (True, (<))
isComparison ">" = (True, (>))
isComparison ">=" = (True, (>=))
isComparison "=<" = (True, (<=))
isComparison _ = (False, (\_ _ -> False))

-- simplifies a substitution to generate a better output
simplify :: [VarName] -> Subst -> Subst
simplify vars s = let Subst s' = restrictTo vars (repeatSubst s) in
  renameSubst vars (fst (runState (repeatState (length s') (f vars) (Subst s')) (Subst s'))) where
  f :: [VarName] -> Subst -> State Subst Subst
  f vs sub = state g where
    g (Subst []) = (sub, empty)
    g (Subst ((v, Var w):xs)) | not (elem w vs) = (applySub (single w (Var v)) sub, applySub (single w (Var v)) (Subst xs))
    g (Subst (_:xs)) = (sub, Subst xs)
  renameSubst :: [VarName] -> Subst -> Subst
  renameSubst vs sub = applySub (multiple v (fmap (\x -> Var x) subVars)) sub where
      v = filter (\x -> not (elem x vs)) (allVars sub)
      subVars = take (length v) (filter (\x -> not (elem x vs)) freshVars)


type Strategy = SLDTree -> [Subst]

-- depth-first search
dfs :: Strategy
dfs (Node (Goal []) _) = [empty]
dfs (Node _ []) = []
dfs (Node goal ((s, tree):ms)) = (fmap (\x -> simplify (allVars goal) (compose x s)) (dfs tree)) ++ dfs (Node goal ms)

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
                                                  in (fst rest, (tree1, simplify (allVars goal) (compose s1 s2)):(snd rest))

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
    in (\(a, b) (c, d) -> (a ++ c, b || d)) (fmap (\x -> simplify (allVars goal) (compose x s)) (fst sol), snd sol) (bdfs i (Node goal ms))

-- solves a goal with a strategie using all rules from a program
solve :: Strategy -> Prog -> Goal -> [Subst]
solve s p g = s (sld s p g) 
