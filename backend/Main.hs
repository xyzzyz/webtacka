{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}
module Main where

import Text.JSON
import Data.String
import qualified Data.Map as Map
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

import qualified Data.ByteString.Lazy.Char8 as B

instance WebSocketsData String where
  fromLazyByteString = B.unpack
  toLazyByteString = B.pack

data ClientMessage = ConnectMessage String (TChan ServerMessage)

data ServerMessage = LoggedIn
                   | LoginErr String

makeJSONPacket :: String -> JSValue -> JSValue
makeJSONPacket typ dat = JSObject $ toJSObject [("type", JSString $ toJSString typ),
                                                ("data", dat)]

makeEmptyJSONPacket :: String -> JSValue
makeEmptyJSONPacket typ = makeJSONPacket typ (JSObject $ toJSObject [])

instance JSON ServerMessage where
  showJSON LoggedIn = makeEmptyJSONPacket "logged_in"
  showJSON (LoginErr str) = makeJSONPacket "login_err"
                            (JSObject $ toJSObject [("msg", JSString $ toJSString str)])

  readJSON = undefined -- we won't need this

data Client = Client {
  nick :: String,
  chan :: TChan ServerMessage
}

type Room = [Client]
data ServerData = ServerData {
  rooms :: Map.Map Int Room,
  clients :: Map.Map String Client
}

getClients :: ServerState (Map.Map String Client)
getClients = do
  e <- get
  return $ clients e

hasClient nick = Map.member nick `fmap` getClients

type ServerState = StateT ServerData IO
atomicallyState = liftIO . atomically

main :: IO ()
main = withSocketsDo $ do
  servSock <- listenOn $ PortNumber 9160
  chan <- atomically newTChan
  forkIO $ acceptLoop servSock chan
  evalStateT (mainLoop chan) (ServerData Map.empty Map.empty)

acceptLoop :: Socket -> TChan ClientMessage -> IO ()
acceptLoop serverSock chan = do
  (clientSock, sockAddr) <- accept serverSock
  forkIO $ runWithSocket clientSock (newClient chan)
  acceptLoop serverSock chan

mainLoop :: TChan ClientMessage -> ServerState ()
mainLoop chan = do
  msg <- atomicallyState $ readTChan chan
  handleClientMessage msg
  mainLoop chan

handleClientMessage (ConnectMessage nick clientChan) = do
  nickTaken <- hasClient nick
  if not nickTaken
    then addClient nick clientChan
    else atomicallyState $ writeTChan clientChan (LoginErr "nick already exists")
    where addClient nick clientChan = do
            e <- get
            put $ e { clients = Map.insert nick (Client nick clientChan) (clients e) }
            atomicallyState $ writeTChan clientChan LoggedIn


newClient :: TChan ClientMessage -> Request -> WebSockets Hybi10 ()
newClient chan rq = do
  liftIO $ putStrLn "Ktos przylazl"
  acceptRequest rq
  msg <- (receiveData :: WebSockets Hybi10 String)
  case decode msg :: Result JSValue of
    Error str -> liftIO $ putStrLn ("JSON error: " ++ str)
    Ok (JSObject obj) -> handleHelloMessage (fromJSObject obj)
  return ()
  where handleHelloMessage lst = case parseHelloMessage lst of
          Left err -> liftIO $ putStrLn ("Protocol error: " ++ err)
          Right nick -> do
            clientChan <- liftIO . atomically $ newTChan
            liftIO $ putStrLn (nick ++ " connected")
            liftIO . atomically $ writeTChan chan (ConnectMessage nick clientChan)
            clientLoop chan clientChan

clientLoop :: TChan ClientMessage -> TChan ServerMessage -> WebSockets Hybi10 ()
clientLoop serverChan clientChan = do
  msg <- liftIO . atomically $ readTChan clientChan
  handleServerMsg msg

handleServerMsg LoggedIn = do
  send $ textData (encode LoggedIn)

parseHelloMessage :: [(String, JSValue)] -> Either String String
parseHelloMessage obj = do
  dat <- checkMessageType "hello" obj
  case lookup "nick" dat of
    Just (JSString jsnick) -> return (fromJSString jsnick)
    Nothing -> fail "Nick not found"


checkMessageType typ obj = case (lookup "type" obj, lookup "data" obj) of
  (Just (JSString jstyp), Just (JSObject obj)) -> let typ' = fromJSString jstyp in
    if typ == typ'
    then return (fromJSObject obj)
    else fail ("type mismatch, got " ++ typ' ++ " but expected " ++ typ)
  (Nothing, _) -> fail "type not found"
  (_, _) -> fail "data not found"
