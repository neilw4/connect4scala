module C4Scala.Define where

type Piece = Int  -- 1 if my piece
type Col = [Piece]
type Board = [Col]
type ColNo = Int
type Score = Int

data C4Node = C4Node { board   :: Board
                     , player  :: Piece
                     , count   :: Int
                     , lastCol :: ColNo
                     , width   :: Int
                     , height  :: Int
                     } deriving Eq