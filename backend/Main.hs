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
data ClientWireMessage = HelloMessage String

instance JSON ClientWireMessage where
  readJSON (JSObject obj) = do
    (typ, dat) <- getMessageTypeAndData (fromJSObject obj)
    case typ of
      "hello" -> HelloMessage `fmap` getStringFromData "nick" dat
  showJSON = undefined -- we won't need it

getStringFromData key dat = case lookup key dat of
  Just (JSString dat) -> return (fromJSString dat)
  Just _ -> fail "expected string"
  _ -> fail "key not found"

getMessageTypeAndData obj = case (lookup "type" obj, lookup "data" obj) of
  (Just (JSString jstyp), Just (JSObject obj)) ->
    Ok (fromJSString jstyp, (fromJSObject obj))
  (Nothing, _) -> Error "type not found"
  (_, _) -> Error "data not found"

wireMessageFromString :: String -> Result ClientWireMessage
wireMessageFromString str = decode str >>= readJSON 

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
  case wireMessageFromString msg of
    Ok (HelloMessage nick) -> handleHelloMessage nick
    Error str -> liftIO $ putStrLn ("JSON error: " ++ str)
    _ -> liftIO $ putStrLn ("Unexpected message")
  return ()
  where handleHelloMessage nick = do
          clientChan <- liftIO . atomically $ newTChan
          networkChan <- liftIO . atomically $ newTChan
          sink <- getSink
          liftIO $ putStrLn (nick ++ " connected")
          liftIO . atomically $ writeTChan chan (ConnectMessage nick clientChan)
          liftIO $ forkIO $ clientLoop clientChan chan networkChan sink
          clientNetworkLoop networkChan

clientLoop :: TChan ServerMessage
              -> TChan ClientMessage
              -> TChan ClientWireMessage
              -> Sink Hybi10  -> IO ()
clientLoop fromServerChan toServerChan fromNetworkChan toNetworkSink = do
  action <- atomically getMessage
  case action of
    Left server -> sendSink toNetworkSink . textData . encode . showJSON $ server
    Right network -> return ()
  clientLoop fromServerChan toServerChan fromNetworkChan toNetworkSink
  where getMessage = (Left `fmap` readTChan fromServerChan)
                     `orElse` (Right `fmap` readTChan fromNetworkChan)

clientNetworkLoop networkChan = do
  msg <- (receiveData :: WebSockets Hybi10 String)
  case wireMessageFromString msg of
    Ok wireMessage -> liftIO $ atomically $ writeTChan networkChan wireMessage
    Error str -> liftIO $ putStrLn ("JSON error: " ++ str)
  clientNetworkLoop networkChan

handleServerMsg msg = do
  send $ textData (encode msg)

