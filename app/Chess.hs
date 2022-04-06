module Chess where

import Data.Char
import Data.Vector

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

instance Show ChessBoard where
   -- Slice the ChessBoard into a list of rows, reverse the rows (because by
   -- convention, chess boards are shown with the white camp downside)
   -- and turns each row to a String before merging these Strings. The
   -- removeLast is here only to get rid of the extra newline added by
   -- unlines.
   show cb = (unlines . Data.Vector.toList . Data.Vector.reverse . Data.Vector.imap showLine . slice8 . asVector) cb
      ++ "  " ++ concat (replicate 8 "+---") ++ "+\n    "
      ++ intercalate "   " (map ((:[]) . showFile) [0..7]) ++ " " ++
         show (nextMove cb)
      where
      showLine :: Int -> Data.Vector.Vector (Maybe Piece) -> String
      showLine rank v =
         "  " ++ concat (replicate 8 "+---") ++ "+\n" ++ (
         showRank rank : " | " ++
         (intercalate " | " . map (:[])) (Data.Vector.toList (Data.Vector.imap (showSquare rank) v))
         ++ " |")
      showSquare :: Int -> Int -> Maybe Piece -> Char
      showSquare rank file Nothing = if rank `mod` 2 == file `mod` 2
                                        then '-'
                                        else ' '
      showSquare _ _ (Just p) = head $ show p
      showRank :: Int -> Char
      showRank r = C.chr $ C.ord '1' + r
      showFile f = C.chr $ C.ord 'a' + f
      slice8 :: Data.Vector.Vector a -> Data.Vector.Vector (Data.Vector.Vector a)
      slice8 v
         | Data.Vector.null v = Data.Vector.empty
         | Data.Vector.length v < 8 = Data.Vector.singleton v
         | otherwise = h `Data.Vector.cons` slice8 t
            where (h, t) = Data.Vector.splitAt 8 v

