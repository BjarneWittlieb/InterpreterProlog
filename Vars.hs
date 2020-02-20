module Vars where

import Type

-- allVars is a function that will return all contained Variables within a certain type
-- The implementation also should eliminate duplicates
class Vars a where
    allVars :: a -> [VarName]

instance Vars Term where
    allVars (Var x) = [x]
    allVars (Comb _ xs) = killDuplicates (foldr (++) [] (fmap allVars xs))

instance Vars Rule where
    allVars (Rule t ts) = killDuplicates ((allVars t) ++ foldr (++) [] (fmap allVars ts))

instance Vars Prog where
    allVars (Prog rs) = killDuplicates (foldr (++) [] (fmap allVars rs))

instance Vars Goal where
    allVars (Goal ts) = killDuplicates (foldr (++) [] (fmap allVars ts))

-- Helper Function for eliminating duplicates within a list
killDuplicates :: (Eq a) => [a] -> [a]
killDuplicates []       = []
killDuplicates (x:xs)   = [x] ++ (filter (/= x) (killDuplicates xs))

-- Possible chars for Variables
firstChars :: String
firstChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ_"
allChars :: String
allChars = firstChars ++ "abcdefghijklmnopqrstuvwxyz"

-- A list of all possible Variables in Prolog
freshVars :: [VarName]
freshVars = [x:xs | xs <- [""] ++ allOptions, x <- firstChars] where
    allOptions :: [String]
    allOptions = aggregator1 0 where
        aggregator1 :: Int -> [String]
        aggregator1 i = (aggregator2 i) ++ (aggregator1 (i+1))
        aggregator2 :: Int -> [String]
        aggregator2 0 = [[c] | c <- allChars]
        aggregator2 i = [c:s | c <- allChars, s <- aggregator2 (i-1)]