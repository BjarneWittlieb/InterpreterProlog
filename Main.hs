module Main where


import Type

-- Welcomes the User and loops
main :: IO ()
main = do
    putStrLn "Welcome to PROVIN MACHINE!\n\n"
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
    loop (Prog [])

-- Now File stands for the currently loaded program

-- Loops through the program
loop :: Prog -> IO ()
loop file = do
    putStr "?- "
    str <- getLine
    process file str

process :: Prog -> String -> IO ()
process file str | head str == ':'   = processCommand file (tail str)
                 | otherwise         = putStr "Not implemented"

processCommand :: Prog -> String -> IO ()
processCommand file ('h':_) = do
    putStrLn "Help window for PROVE-MACHINE:"
    putStrLn "  <goal>      Proves the specefied goal with currently loaded Programms."
    putStrLn "  :h          Shows this help window."
    putStrLn "  :l <file>   Loads the specified file."
    putStrLn "  :q          Exits the interactive environtment"
    putStrLn "  :s <strat>  Sets the specified strategy"
    putStrLn "              Where <strat> is one of 'dfs', 'bfs' or 'idfs'."
    loop file
processCommand file ('q':_) = do
    putStrLn "Goodbye!"