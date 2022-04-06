module Chess where

import Data.Char

data PieceType = 
    Pawn
    |   Tower

instance Show PieceType where
    show Pawn = "p"
    show Tower = "t"

data Color = Black | White

data Piece = Piece Color PieceType

instance Show Piece where
    show (Piece White pt) = map Data.Char.toUpper $ show pt
    show (Piece Black pt) = show pt

data GameBoard = ChessBoard
    {
            asVector:: Data.Vector.Vector (Maybe Piece)
        ,   nexPlayer:: Color
    }

