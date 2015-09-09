module Game where

import Data.Maybe (fromMaybe)
import Control.Monad.State
import qualified Data.Map as M
import Data.Char (toLower)
import Board

data Move = Move Cell (Int, Int)
          | EndTurn

type Player = Cell

data GameState = GameState {
  board :: Board,
  whiteScore :: Int,
  blackScore :: Int,
  whoseTurn :: Player,
  movesLeft :: Int
  }
               deriving Show

otherPlayer :: GameState -> Player
otherPlayer gs = case whoseTurn gs of
  White -> Black
  Black -> White

scoreGame :: GameState -> Player -> Int
scoreGame _ _ = 5

makeMove :: [GameState] -> Move -> Maybe [GameState]
makeMove allGS@(gs:gss) EndTurn =
  let
    newMovesLeft = 2
    newWhiteScore = scoreGame gs White
    newBlackScore = scoreGame gs Black
    newGS = gs {whoseTurn = otherPlayer gs,
                whiteScore = newWhiteScore,
                blackScore = newBlackScore,
                movesLeft = newMovesLeft}
  in
   Just $ newGS : allGS
makeMove allGS@(gs:gss) (Move c pos) =
  let newMap = M.update (const (Just c)) pos (posMap (board gs))
      newMovesLeft = movesLeft gs - 1
      newGS = gs {
        board = Board newMap (size (board gs)),
        movesLeft = newMovesLeft
        }
  in
   if newMovesLeft == 0
   then makeMove (newGS : allGS) EndTurn
   else Just (newGS : allGS)

checkMove :: [GameState] -> Move -> Maybe Move
checkMove _ EndTurn = Just EndTurn
checkMove (gs:gss) move@(Move cell pos) =
  M.lookup pos (posMap (board gs))
  >>= (\oldCell -> case oldCell of
                   Empty -> Just move
                   _ -> Nothing)
  >>= (\m -> if cell == whoseTurn gs
            then Just m
            else Nothing)

parseCommand :: String -> Maybe Move
parseCommand s = case words . map toLower $ s of
  ["w", x, y] -> Just $ Move White (read x, read y)
  ["b", x, y] -> Just $ Move Black (read x, read y)
  "e":_ -> Just EndTurn
  _ -> Nothing

update :: String -> [GameState] -> Maybe [GameState]
update s gs = parseCommand s
              >>= checkMove gs
              >>= makeMove gs

initState :: GameState
initState = GameState (makeBoard 4) 0 0 White 1

playGame :: StateT [GameState] IO ()
playGame = do
  command <- lift getLine
  allGS <- get
  let newGSs@(curr:_) = fromMaybe allGS (update command allGS)
  lift $ print curr
  put newGSs
  playGame
