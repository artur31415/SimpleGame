module Main where
import System.IO

import Chess

main :: IO ()
main = do
    putStrLn "Hello, Haskell!"
    debug_piece <- White Bishop
    putStrLn show debug_piece
    putStr "input> "
    hFlush stdout
    u_input <- getLine
    putStrLn $ "new Command: " ++ u_input
    main
