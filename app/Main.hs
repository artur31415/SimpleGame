module Main where
import System.IO

import Chess

main :: IO ()
main = do
    putStrLn "Hello, Haskell!"
    let debug_piece = Piece White Tower
    let debug_p = Piece Black Pawn
    putStrLn $ show debug_piece
    putStrLn $ show debug_p
    putStrLn $ show initialPosition
    putStr "input> "
    hFlush stdout
    u_input <- getLine
    putStrLn $ "new Command: " ++ u_input
    main
