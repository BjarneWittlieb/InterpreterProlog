module Main where


-- Welcomes the User and loops
main :: IO ()
main = do
    putStrLn "Welcome!"

-- Loops through the program
loop :: IO ()
loop = do
    putStr "?- "
    str <- getLine
    process str

process :: String -> IO ()
process str | head str == ':'   = processCommand (tail str)
            | otherwise         = putStr "Not implemented"

processCommand :: String -> IO ()
processCommand ('h':_) = do
    putStrLn "Help window for PROVE-MACHINE:"
    putStrLn "  <goal>      Proves the specefied goal with currently loaded Programms."
    putStrLn "  :h          Shows this help window."
    putStrLn "  :l <file>   Loads the specified file."
    putStrLn "  :q          Exits the interactive environtment"
    putStrLn "  :s <strat>  Sets the specified strategy"
    putStrLn "              Where <strat> is one of 'dfs', 'bfs' or 'idfs'."
    loop