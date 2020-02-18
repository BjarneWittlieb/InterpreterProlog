module SLDResolution where

import Type
import Substitutions


-- Data representation of an SLD Tree
data SLDTree = Node Term [(Subst, [SLDTree])]

-- A Goal consisits of multiple terms so we return multiple sld trees?
sld :: Prog -> Goal -> [SLDTree]
sld p (Goal ts) = fmap (fst (sldTerm vars p)) tsWithout where
    -- Passing down all currently used vars
    vars = killDuplicates ((Vars p) ++ (Vars (Goal tsWithout)))
    -- Replacing all variables "_" in ts
    tsWithout = fmap replaceUnderscore ts
    -- The same as the function above but for one term only
    -- Also the Variables used are passed down
    sldTerm :: [VarName] -> Prog -> Term -> (SLDTree, [VarName])
    sldTerm vs (Prog []) t = (Node t [], vs)
    -- Unification was not possible, thus trying next rule in programm
    sldTerm vs (Prog (r:rs)) t | isNothing unifier = (Node tReplaced 