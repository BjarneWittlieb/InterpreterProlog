module Vars where

import Type

add :: Int -> Int -> Int
add x y = x + y

class Vars a where
    allVars :: a -> [VarName]

instance Vars Term where
    allVars (Var x) = [x]
    allVars (Comb f xs) = killDuplicates (foldr (++) [] (fmap allVars xs))

instance Vars Rule where
    allVars (Rule t ts) = killDuplicates ((allVars t) ++ foldr (++) [] (fmap allVars ts))

instance Vars Prog where
    allVars (Prog rs) = killDuplicates (foldr (++) [] (fmap allVars rs))

instance Vars Goal where
    allVars (Goal ts) = killDuplicates (foldr (++) [] (fmap allVars ts))

killDuplicates :: (Eq a) => [a] -> [a]
killDuplicates []       = []
killDuplicates (x:xs)   = [x] ++ (filter (/= x) (killDuplicates xs))

-- For testing
t1 :: Term
t1 = (Comb "." [Comb "true" [], Comb "h" [Var "E", Comb "i" [Var "F"]]])

r1 = Rule t1 [t1, t1, t1]
p1 = Prog [r1, r1, r1]


firstChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ_"
allChars = firstChars ++ "abcdefghijklmnopqrstuvwxyz"

freshVars :: [VarName]
freshVars = [x:xs | xs <- [""] ++ allOptions, x <- firstChars] where
    allOptions :: [String]
    allOptions = aggregator1 0 where
        aggregator1 i = (aggregator2 i) ++ (aggregator1 (i+1))
        aggregator2 0 = [[c] | c <- allChars]
        aggregator2 i = [c:s | c <- allChars, s <- aggregator2 (i-1)]

allOptions :: [String]
allOptions = [[c] | c <- allChars] ++ [c:s | c <- allChars, s <- allOptions]