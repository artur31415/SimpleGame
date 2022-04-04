module Chess where

data PieceType = 
    Pawn
    |   Tower

instance Show PieceType where
    show Pawn = "p"
    show Tower = "t"

data Color = Black | White

data Piece = 

