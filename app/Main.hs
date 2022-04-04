module Main where
import System.IO

main :: IO ()
main = do
    putStrLn "Hello, Haskell!"
    putStr "input> "
    hFlush stdout
    u_input <- getLine
    putStrLn $ "new Command: " ++ u_input
    main
