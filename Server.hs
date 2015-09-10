{-# LANGUAGE OverloadedStrings #-}
module Server where

import Board
import Game
-- import Control.Monad (forever)
import Control.Monad.State
import Data.Maybe (fromMaybe)
import Data.Text as T
import qualified Network.WebSockets as WS

step :: String -> StateT [GameState] IO ()
step command = do
  gss <- get
  put $ fromMaybe gss (update command gss)

go :: WS.Connection -> [GameState] -> IO ()
go conn states =
  do
    command <- WS.receiveData conn :: IO T.Text
    print . T.unpack $ T.append "command was " command
    newStates@(h:_) <- execStateT (step $ T.unpack command) states
    print h
    WS.sendTextData conn (T.pack $ show (posMap . board $ h))
    go conn newStates

gameLoop :: WS.Connection -> IO()
gameLoop conn =
  forever $ go conn [initState]
  -- runStateT (playGame conn) [initState]

app :: WS.ServerApp -- WS.PendingConnection -> IO ()
app pending =
  do
    conn <- WS.acceptRequest pending
    gameLoop conn

main :: IO ()
main = WS.runServer "127.0.0.1" 5000 app
