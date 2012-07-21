{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Text (Text)
import Network.WebSockets
import System.IO
import System.Environment
import Control.Monad
import Control.Monad.Trans
import Control.Exception
import Control.Concurrent
import Control.Concurrent.STM

import qualified Data.Text as T
import qualified Data.Text.IO as T

type Client = Sink Hybi00

main :: IO ()
main = do
  runServer "0.0.0.0" 9160 $ application

application :: Request -> WebSockets Hybi00 ()
application rq = do
  liftIO $ putStrLn "Ktos przylazl"
  acceptRequest rq
  getVersion >>= liftIO . putStrLn . ("Client version: " ++)
  sink <- getSink
  liftIO $ (forkIO $ handleClient sink) >> return ()

handleClient sink = do
  sendSink sink $ textData ("SIEMA CO TAM" :: Text)
  sendSink sink $ close ("WON" :: Text)