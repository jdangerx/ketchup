module Game where

import Control.Monad (forever)
import Control.Monad.State
import qualified Data.Map as M
import Data.Char (toLower)
import Board

data Move = Move Cell (Int, Int)

putPiece :: Move -> Board -> Board
putPiece (Move c pos) b =
  let newMap = M.update (const (Just c)) pos (board b)
  in Board newMap (size b)

checkMove :: Move -> Board -> Maybe Board -- consider switching to `Either`
checkMove move@(Move _ pos) b =
  M.lookup pos (board b) >>= (\oldCell -> case oldCell of
                                              Empty -> return (putPiece move b)
                                              _ -> Nothing)


parseCommand :: String -> Move
parseCommand s = case words . map toLower $ s of
  ["w", x, y] -> Move White (read x, read y)
  ["b", x, y] -> Move Black (read x, read y)

playGame :: IO ()
playGame = do
  gameBoard <- return $ makeBoard 4
  forever (do
              move <- getLine >>= (\s -> return $ parseCommand s)
              gameBoard <- return $ checkMove move gameBoard
              print gameBoard)
