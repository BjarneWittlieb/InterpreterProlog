module SLDResolution(Strategy, dfs, bfs, idfs, solve, simplify) where
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
sld program strategy finalGoal = fst (runState (sldWithVar program (Goal (fst noUnderscoreGoal)) strategy) variables) where
    variables = filter (\x -> not (elem x (allVars program))) (snd noUnderscoreGoal)
    noUnderscoreGoal = (runState (replaceList (fromGoal finalGoal)) (filter (\x -> not (elem x (allVars finalGoal))) freshVars))
    -- main function, that does the SLD resultion, tracks all currently used variables along the way
    sldWithVar :: Prog -> Goal -> Strategy -> State [VarName] SLDTree
    sldWithVar pr goal strat = state (\vars -> let (appliedProgramm, finalVars) = runState ((rename pr) >>= (programToList goal strat)) vars in
      ((Node goal appliedProgramm), finalVars)) where

        programToList :: Goal -> Strategy -> Prog -> State [VarName] [(Subst, SLDTree)]
        programToList (Goal ((Comb "call" ((Comb f xs):ys)):ts)) stra p = let s = (solve stra p (Goal [Comb f (xs ++ ys)])) in
            pure (fmap (\sub -> (sub, sld p stra (Goal ts))) s)
        programToList (Goal ((Comb "\\+" [x]):ts)) stra p = case solve stra p (Goal [Comb "call" [x]]) of
                                                             [] -> pure [(empty, sld p stra (Goal ts))]
                                                             _ -> pure []
        programToList (Goal ((Comb "findall" [x, y, z]):ts)) stra p = 
          case unify z (listToTerm (fmap (((flip apply) x).(simplify (allVars (Goal [x, y, z])))) (solve stra p (Goal [Comb "call" [y]])))) of
            Nothing -> pure []
            Just s -> pure [(s, sld p stra (apply s (Goal ts)))]             
        programToList g stra (Prog rs)  = state (\s -> (catMaybes (fst (st s)), snd (st s))) where
            st = runState (listState rs (resolutionStep g stra (Prog rs)))
        listToTerm :: [Term] -> Term
        listToTerm [] = Comb "[]" []
        listToTerm (t:ts) = Comb "." [t, listToTerm ts]
        -- This method pretty much is the same as before
        resolutionStep :: Goal -> Strategy -> Prog -> Rule -> State [VarName] (Maybe (Subst, SLDTree))
        resolutionStep (Goal []) _ _ _ = pure Nothing
        resolutionStep (Goal (t:t')) stra p (Rule t1 ts) = if (isNothing subst) then pure Nothing
            else state g where
            g vs = let (tree, vsAfter) = runState (sldWithVar p (Goal (apply (fromJust subst) (ts ++ t'))) stra) vs in
                (Just (fromJust subst, tree), vsAfter)
            subst = unify t t1
        
{-
         -- searches through all rules and tries to apply them to the first term
        programToList :: Goal -> Strategy -> Prog -> State [VarName] [(Subst, SLDTree)]
        programToList (Goal []) _ _ = pure []
        programToList (Goal ((Comb "call" toProove):ts)) stra p = state (\ s -> (listToMaybe (solve stra p (Goal toProove)), s)) >> (programToList (Goal ts) stra p)
        programToList (Goal (t:ts)) stra p = (resolutionOneTerm t stra p) >> (programToList (Goal ts) stra p)

        resolutionOneTerm :: Term -> Strategy -> Prog -> State [VarName] [(Subst, SLDTree)]
        resolutionOneTerm t stra (Prog rs) = (listState rs (resolutionStep t stra (Prog rs))) >>= (return . catMaybes)

        -- This method pretty much is the same as before
        resolutionStep :: Term -> Strategy -> Prog -> Rule -> State [VarName] (Maybe (Subst, SLDTree))
        resolutionStep t stra p (Rule t1 ts) = if (isNothing subst) then pure Nothing
            else state g where
            g vs = let (tree, vsAfter) = runState (sldWithVar p (Goal (apply (fromJust subst) ts)) stra) vs in
                (Just (fromJust subst, tree), vsAfter)
            subst = unify t t1

-}
        -- Hallo Erik, ich habe hier ein kleine Nachricht für dich versteckt damit du weißt was hier abgeht
        -- Ich habe gerade nach einiger Zeit es endlich geschafft durch den ganzen Kram den du hier gemacht hast durchzublicken..
        -- Ich muss diesen Teil wegen den Higher-order Prädikaten leider umschreiben, denn für diese muss geprüft werden,
        --  BEVOR man das gesamte programm und dessen Regeln druchgeht ob es sich um ein Higher order prädikat handelt.
        -- Daher funktioniert deine Lösung mit listState leider nicht mehr, da es sonst keine Möglichkeit gibt.
        -- Kompilierst du den unten stehenden code, den ich auskommentiert habe, wirst du das Problem auch feststellen.

        -- Keine Ahnung, was du hier genau vorhast, aber wie das hier steht, scheint es nicht zu funktionieren.
        -- Wenn ich (>>) richtig verstehe, gibt programToList bei dieser implementierung immer pure [] zurück.
        -- Ich hab zunächst wieder eine Version, die zumindest für normale Prädikate funktioniert, hingeschrieben.
        -- Übrigens hab ich die replace Methoden grundlegend verändert: sie substituieren jetzt ausschließlich Variablen
        -- aus der [VarName] Liste, da dies deutlich effizienter ist.

        -- Ok, ich glaub ich hab call jetzt implementiert, man musste doch nur bei programToList einen Spezialfall hinzufügen?
{-
        programToList :: Goal -> Strategy -> Prog -> State [VarName] [(Subst, SLDTree)]
        programToList g stra (Prog rs)  = state (\s -> (catMaybes (fst (st s)), snd (st s))) where
            st = runState (listState rs (resolutionStep g (Prog rs) stra))
        -- Takes a term to pattern match
        -- Takes a rule to apply (try pattern matching)
        -- Takes a program and a goal with which to continue further
        
        resolutionStep :: Goal -> Prog -> Strategy -> Rule -> State [VarName] (Maybe (Subst, SLDTree))
        resolutionStep (Goal []) _ _ _                  = pure Nothing

        -- Higher order call
        resolutionStep (Goal ((Comb "call" highers):xs)) p stra _ = if (isNothing subst) then pure Nothing else state g where
          g vs = let (tree, vsAfter) = runState (sldWithVar p (Goal (apply (fromJust subst) xs)) stra) vs in
                (Just (fromJust subst, tree), vsAfter)
          subst = listToMaybe (solve stra p (Goal highers))      

        -- Regular case
        resolutionStep (Goal (x:xs)) p stra (Rule t ts) = if (isNothing subst) then pure Nothing
            else state g where
            g vs = let (tree, vsAfter) = runState (sldWithVar p (Goal (apply (fromJust subst) (ts ++ xs))) stra) vs in
                (Just (fromJust subst, tree), vsAfter)
            subst = unify x t
-}

-- simplifies a substitution to generate a better output
simplify :: [VarName] -> Subst -> Subst
simplify vars s = let Subst s' = restrictTo vars (repeatSubst s) in
  renameSubst vars (fst (runState (repeatState (length s') (f vars) (Subst s')) (Subst s'))) where
  f :: [VarName] -> Subst -> State Subst Subst
  f vs sub = state g where
    g (Subst []) = (sub, empty)
    g (Subst ((v, Var w):xs)) | not (elem w vs) = (applySub (single w (Var v)) sub, applySub (single w (Var v)) (Subst xs))
    g (Subst (x:xs)) = (sub, Subst xs)
  renameSubst :: [VarName] -> Subst -> Subst
  renameSubst vs sub = applySub (multiple v (fmap (\x -> Var x) subVars)) sub where
      v = filter (\x -> not (elem x vs)) (allVars sub)
      subVars = take (length v) (filter (\x -> not (elem x vs)) freshVars)


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
