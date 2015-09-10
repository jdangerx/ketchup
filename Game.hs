module Game where

import Control.Monad.State
import Data.Char (toLower)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.List as L
import Board

data Move = Move Cell (Int, Int)
          | EndTurn

type Player = Cell

data GameState = GameState {
  board :: Board,
  whiteScore :: [Int],
  blackScore :: [Int],
  whoseTurn :: Player,
  movesLeft :: Int
  }
               deriving Show

otherPlayer :: GameState -> Player
otherPlayer gs = case whoseTurn gs of
  White -> Black
  Black -> White

isNeighbor :: (Int, Int) -> (Int, Int) -> Bool
isNeighbor (x, y) (x', y') =
  let dy = y' - y
      dx = x' - x
  in abs dy <= 1 && abs dx <= 1 && abs (dx - dy) <= 1

isNeighborOfList :: (Int, Int) -> [(Int, Int)] -> Bool
isNeighborOfList _ [] = False
isNeighborOfList pos (h:t) = isNeighbor pos h || isNeighborOfList pos t

expandGroup ::
  [(Int, Int)] -> [(Int, Int)] ->
  ([(Int, Int)], [(Int, Int)])
expandGroup group rest =
  let biggerGroup = foldr
                    (\pos gp -> if isNeighborOfList pos group
                               then pos:gp
                               else gp)
                    group rest
  in
   (biggerGroup, rest L.\\ biggerGroup)

groupByCont :: [[(Int, Int)]] -> [(Int, Int)] -> [[(Int, Int)]]
groupByCont [] [] = [[]]
groupByCont groups [] = groups
groupByCont groups ls@(h:t) =
  let newGroups = foldr (\group new -> fst (expandGroup group ls):new)
                  [] groups
      newRest = ls L.\\ concat newGroups
  in
   if newGroups == groups
   then groupByCont ([h]:groups) t
   else groupByCont newGroups newRest
  -- for each group, grab everyone who's a neighbor if nobody is a
  -- neighbor of any group, put someone in as a singleton, then run
  -- again.

scoreGame :: GameState -> Player -> [Int]
scoreGame gs p = L.sortBy (flip compare) . map length .
                 groupByCont [] .
                 M.keys $ M.filter (== p) (posMap . board $ gs)

didTriggerCatchup :: Int -> Int -> GameState -> Bool
didTriggerCatchup newWhite newBlack old =
  let
    oldWhite = maximum $ whiteScore old
    oldBlack = maximum $ blackScore old
  in
   case whoseTurn old of
    White -> newWhite >= oldBlack && newWhite > oldWhite && newWhite > 1
    Black -> newBlack >= oldWhite && newBlack > oldBlack && newBlack > 1

makeMove :: [GameState] -> Move -> Maybe [GameState]
makeMove allGS@(gs:gss) EndTurn =
  let
    newWScore = scoreGame gs White
    newBScore = scoreGame gs Black
    newMovesLeft = case gss of
      -- white's first move doesn't trigger this.
      _:_:_ -> if didTriggerCatchup (maximum newWScore) (maximum newBScore) gs
                 then 3
                 else 2
      _ -> 2
    newGS = gs {whoseTurn = otherPlayer gs,
                whiteScore = newWScore,
                blackScore = newBScore,
                movesLeft = newMovesLeft}
  in
   Just $ newGS : allGS
makeMove allGS@(gs:_) (Move c pos) =
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
checkMove (gs:_) move@(Move cell pos) =
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
initState = GameState (makeBoard 4) [0] [0] White 1

gameEnded :: GameState -> Bool
gameEnded = M.null . M.filter (== Empty) . posMap . board

getWinner :: GameState -> String
getWinner gs =
  let
    white = scoreGame gs White
    black = scoreGame gs Black
  in case compare white black of
      EQ -> "Tie game!" -- this cannot happen.
      GT -> "White victory!"
      LT -> "Black victory!"

playGame :: StateT [GameState] IO ()
playGame =
  do
    command <- lift getLine
    allGS <- get
    let newGSs@(curr:_) = fromMaybe allGS (update command allGS)
    lift $ print curr
    put newGSs
    if gameEnded curr
      then lift (print (getWinner curr))
      else playGame
      -- then lift $ print $ getWinner curr
      -- else playGame

main :: IO ()
main = do
  print initState
  execStateT playGame [initState]
  return ()
