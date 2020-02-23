module Main where


import Type
import SLDResolution
import Parser
import Substitutions
import Prettyprinting
import Vars
import System.IO


-- Welcomes the User and loops
main :: IO ()
main = do
    putStrLn "Welcome to PROVING MACHINE!\n\n"
    putStrLn "        Nd. 'ccccccccccccccccccccccccccc:.,0MMMM"
    putStrLn "        MWO;,dXMMMMM0l::cccccccccccccokNX;'0MMMM"
    putStrLn "        MMMNd,;kWMMM0;.,xOOOOOOOOOOOxl':x;'0MMMM"
    putStrLn "        MMMMWKc'cKWMMXo':0WMMMMMMMMMMWk''.'0MMMM"
    putStrLn "        MMMMMMWk;,dNMMWO:'dNMMMMMMMMMMNo..;KMMMM"
    putStrLn "        MMMMMMMMXo,;OWMMXd':d0WMMMMMMMMN0OKWMMMM"
    putStrLn "        MMMMMMMMMW0c'lKMMW0:.'oXMMMMMMMMMMMMMMMM"
    putStrLn "        MMMMMMMMMMMNk;,xNMMNKd,;OWMMMMMMMMMMMMMM"
    putStrLn "        MMMMMMMMMMMMMXo':0WMMW0,.xWMMMMMMMMMMMMM"
    putStrLn "        MMMMMMMMMMMMMMWk'.kWMNd';OWMMMMMMMMMMMMM"
    putStrLn "        MMMMMMMMMMMMMW0c'c0Oo;'oXMMMMMMMMMMMMMMM"
    putStrLn "        MMMMMMMMMMMMXo,;k0l''c0WMMMMMMMMMMMMMMMM"
    putStrLn "        MMMMMMMMMMNk;,dKx,;kXNMMMMMMMMMMMMWNNWMM"
    putStrLn "        MMMMMMMMW0c'l0O:'oXMMMMMMMMMMMMMW0c',OMM"
    putStrLn "        MMMMMMMXo':O0l':0WMMMMMMMMMMMMMWO,..'0MM"
    putStrLn "        MMMMMNx,,xKx,'oKNNNNNNNNNNNNNXOl';c.cNMM"
    putStrLn "        MMMW0:'lKWk,.,:::::::::::::::::ckKl.xMMM"
    putStrLn "        MMXo':0WMMWXXXXXXXXXXXXXXXXXXNWWM0,;KMMM"
    putStrLn "        Wk'.cO000000000000000000000000000l.oWMMM"
    putStrLn "        Nc  ............................. .kMMMM\n\n"
    -- Start with empty programm
    loop (Prog []) dfs

-- Now File stands for the currently loaded program

-- Loops through the program
loop :: Prog -> Strategy -> IO ()
loop file strat = do
    putStr "?- "
    -- Make sure that ?- is printed before input
    hFlush stdout
    str <- getLine
    process file strat str

-- processes an input string
process :: Prog -> Strategy -> String -> IO ()
-- if it starts with an ':', call processCommand
process file strat (':':cmd)  = processCommand file strat cmd
-- otherwise try to parse the string
process file strat cmd        = do
    let x = parse cmd
    case x of
        (Left s) -> do
            putStrLn s
            loop file strat
        -- if the parse was successful, solve the goal and call goThroughSubs to generate the output 
        (Right goal) -> do
            let goalVars = allVars goal in goThroughSubs (fmap (simplify goalVars) (solve strat file goal))
            loop file strat

goThroughSubs :: [Subst] -> IO ()
-- if there are no more substitutions, output 'false'
goThroughSubs [] = do
    putStrLn "false."
    return ()
-- otherwise output the first substitution:
-- if the substitution is empty, output 'true' and return
-- otherwise output the substitution and wait for user input
goThroughSubs (x:xs) =
    if isEmpty x then do putStrLn "true."
                         return ()
                 else do putStr (pretty x)
                         hFlush stdout
                         c <- getLine
                         parseLine c xs

-- parses user input while being in the process of outputting the substititutions
parseLine :: String -> [Subst] -> IO ()
-- return, if a the input is a '.'
parseLine ('.':_) _ = do
    return ()
-- print 'false' and return, if there are no more solutions
parseLine (';':_) [] = do
    putStrLn "false."
    return ()
-- call goThroughSubs, if the input is just a ',', which will output the next substitution and get the next user input
parseLine ";" s = goThroughSubs s
-- output the next substitution, if the input is a ','
parseLine (';':xs) (s:ss) =
    if isEmpty s then do putStrLn "true."
                         return ()
                 else do putStrLn (pretty s)
                         parseLine xs ss
-- otherwise print an error message and return
parseLine _ _ = do
    putStrLn "Expected either '.' or ';'!"
    return ()
    

processCommand :: Prog -> Strategy -> String -> IO ()

-- Help window
processCommand file strat ('h':_) = do
    putStrLn "Help window for PROVING MACHINE:"
    putStrLn "  <goal>      Proves the specefied goal with currently loaded Programms."
    putStrLn "  :h          Shows this help window."
    putStrLn "  :l <file>   Loads the specified file."
    putStrLn "  :q          Exits the interactive environtment"
    putStrLn "  :s <strat>  Sets the specified strategy"
    putStrLn "              Where <strat> is one of 'dfs', 'bfs' or 'idfs'."
    loop file strat

-- Qutting programm    
processCommand _ _ ('q':_) = do
    putStrLn "Goodbye!"

-- Setting strategy
processCommand file _ "s dfs" = do
    putStrLn "Changed Strategy to dfs."
    loop file dfs
processCommand file _ "s bfs" = do
    putStrLn "Changed Strategy to bfs."
    loop file bfs
processCommand file _ "s idfs" = do
    putStrLn "Changed Strategy to idfs."
    loop file idfs

-- Loading program
processCommand file strat ('l':' ':filePath) = do
    x <- parseFile filePath
    case x of
        (Left s) -> do
            putStrLn s
            loop file strat
        (Right prog) -> do
            putStrLn ("Loaded " ++ filePath ++ " successfully!")
            loop prog strat 

-- Command could not be recognized
processCommand file strat _ = do
    putStrLn "Command not found."
    putStrLn "Try ':h' to see a list of commands."
    loop file strat
