module Main where

import Game
import Board
import Data.Maybe (fromMaybe)
import Control.Monad.State

-- playGame :: StateT [GameState] IO ()
-- playGame = do
--   allGS <- get
--   command <- lift getLine
--   let newGSs@(curr:_) = fromMaybe allGS (parseCommand command >>= (`update` allGS))
--   put newGSs
--   if gameEnded curr
--   then lift $ print $ getWinner curr
--   else print curr

main :: IO ()
main =
  forever $ do
    print initState
    execStateT playGame [initState]
