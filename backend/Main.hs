{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Text (Text)
import Network (listenOn, PortID(PortNumber))
import Network.Socket (accept, withSocketsDo, Socket)
import Network.WebSockets
import System.IO
import System.Environment
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans
import Control.Exception
import Control.Concurrent
import Control.Concurrent.STM

import qualified Data.Text as T
import qualified Data.Text.IO as T

type ClientMessage = ()
type Client = TChan ClientMessage
type Room = [Client]
data ServerData = ServerData {
  rooms :: [Room]
}

type ServerState = StateT ServerData IO

main :: IO ()
main = withSocketsDo $ do
  servSock <- listenOn $ PortNumber 9160
  acceptChan <- atomically newTChan
  forkIO $ acceptLoop servSock acceptChan
  evalStateT (mainLoop servSock acceptChan) (ServerData [])

acceptLoop :: Socket -> TChan Socket -> IO ()
acceptLoop serverSock acceptChan = do
  (clientSock, sockAddr) <- accept serverSock
  atomically $ writeTChan acceptChan clientSock
  acceptLoop serverSock acceptChan

mainLoop :: Socket -> TChan Socket -> ServerState ()
mainLoop serverSock acceptChan = do
  clientSock <- lift (atomically $ readTChan acceptChan)
  lift (forkIO $ runWithSocket clientSock application)
  mainLoop serverSock acceptChan


application :: Request -> WebSockets Hybi10 ()
application rq = do
  liftIO $ putStrLn "Ktos przylazl"
  acceptRequest rq
  getVersion >>= liftIO . putStrLn . ("Client version: " ++)
  send $ textData ("{\"type\": \"OK\"}" :: Text)
  msg <- receiveData
  liftIO $ T.putStrLn msg
  send $ close ("WON" :: Text)
  return ()