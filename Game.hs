module Game where

import Control.Monad (forever)
import Control.Monad.State
import qualified Data.Map as M
import Data.Char (toLower)
import Board

data Move = Move Cell (Int, Int)

data BoardState = BoardState {
  board :: Board
  -- whiteScore :: Int,
  -- blackScore :: Int,
  -- whiteMoves :: Int,
  -- blackMoves :: Int
  }

putPiece :: Move -> Board -> Board
putPiece (Move c pos) b =
  let newMap = M.update (const (Just c)) pos (posMap b)
  in Board newMap (size b)

checkMove :: Move -> Board -> Maybe Board -- consider switching to `Either`
checkMove move@(Move _ pos) b =
  M.lookup pos (posMap b) >>= (\oldCell -> case oldCell of
                                              Empty -> return (putPiece move b)
                                              _ -> Nothing)

parseCommand :: String -> Move
parseCommand s = case words . map toLower $ s of
  ["w", x, y] -> Move White (read x, read y)
  ["b", x, y] -> Move Black (read x, read y)

playGame :: StateT Board IO ()
playGame = do
  command <- lift getLine
  let move = parseCommand command
  theBoard <- get
  let newBoard = putPiece move theBoard
  put newBoard
  lift $ print newBoard
  playGame
  
