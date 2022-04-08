module Chess where

import Data.Char
import qualified Data.Vector as V
import Data.List

data PieceType = 
    Pawn
    |   Tower
    |   Knight
    |   Rook
    |   Bishop
    |   King
    |   Queen

instance Show PieceType where
    show Pawn = "p"
    show Tower = "t"
    show Knight = "k"
    show Rook = "r"
    show Bishop = "b"
    show King = "k"
    show Queen = "q"

data Color = Black | White

instance Show Color where
    show Black = show "Black"
    show White = show "White"

data Piece = Piece Color PieceType

instance Show Piece where
    show (Piece White pt) = map Data.Char.toUpper $ show pt
    show (Piece Black pt) = show pt

data ChessBoard = ChessBoard
    {
            asVector:: V.Vector (Maybe Piece)
        ,   nexPlayer:: Color
    }

instance Show ChessBoard where
   -- Slice the ChessBoard into a list of rows, reverse the rows (because by
   -- convention, chess boards are shown with the white camp downside)
   -- and turns each row to a String before merging these Strings. The
   -- removeLast is here only to get rid of the extra newline added by
   -- unlines.
   show cb = (unlines . V.toList . V.reverse . V.imap showLine . slice8 . asVector) cb
      ++ "  " ++ concat (replicate 8 "+---") ++ "+\n    "
      ++ intercalate "   " (map ((:[]) . showFile) [0..7]) ++ " " ++
         show (nexPlayer cb)
      where
      showLine :: Int -> V.Vector (Maybe Piece) -> String
      showLine rank v =
         "  " ++ concat (replicate 8 "+---") ++ "+\n" ++ (
         showRank rank : " | " ++
         (intercalate " | " . map (:[])) (V.toList (V.imap (showSquare rank) v))
         ++ " |")
      showSquare :: Int -> Int -> Maybe Piece -> Char
      showSquare rank file Nothing = if rank `mod` 2 == file `mod` 2
                                        then '-'
                                        else ' '
      showSquare _ _ (Just p) = head $ show p
      showRank :: Int -> Char
      showRank r = Data.Char.chr $ Data.Char.ord '1' + r
      showFile f = Data.Char.chr $ Data.Char.ord 'a' + f
      slice8 :: V.Vector a -> V.Vector (V.Vector a)
      slice8 v
         | V.null v = V.empty
         | V.length v < 8 = V.singleton v
         | otherwise = h `V.cons` slice8 t
            where (h, t) = V.splitAt 8 v


initialPosition :: ChessBoard
initialPosition = ChessBoard {
   asVector = V.fromList $ concat $
   [whiteRearRow, whiteFrontRow]
   ++
   replicate 4 emptyRow
   ++
   [blackFrontRow, blackRearRow]
   , nexPlayer = White
   }
   where
   whiteRearRow  = map (Just . Piece White) rearRow
   whiteFrontRow = replicate 8 $ Just $ Piece White Pawn
   emptyRow      = replicate 8 Nothing
   blackFrontRow = replicate 8 $ Just $ Piece Black Pawn
   blackRearRow  = map (Just . Piece Black) rearRow
   rearRow       = [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]
