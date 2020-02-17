module Pretty where

import Type

add :: Int -> Int -> Int
add x y = x + y

class Vars a where
    allVars :: a -> [VarName]

instance Vars Term where

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