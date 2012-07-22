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
import System.Random
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

data ClientMessage = ClientConnected String (TChan ServerMessage)
                   | ClientAsksForRooms String
                   | ClientCreatesRoom String Int
                   | ClientJoinsRoom String Int
                   | ClientStarts String

data ClientWireMessage = Hello String
                       | GetRooms
                       | CreateRoom Int
                       | Join Int
                       | Start
                       deriving (Show)

instance JSON ClientWireMessage where
  readJSON (JSObject obj) = do
    (typ, dat) <- getMessageTypeAndData (fromJSObject obj)
    case typ of
      "hello" -> Hello `fmap` getStringFromData "nick" dat
      "get_rooms" -> Ok GetRooms
      "create_room" -> CreateRoom `fmap` getIntFromData "capacity" dat
      "join" -> Join `fmap` getIntFromData "room" dat
      "start" -> Ok Start
      s -> fail ("unknown message type: " ++ s)
  showJSON = undefined -- we won't need it

getFromData key dat =  case lookup key dat of
  Just dat -> return dat
  Nothing -> fail ("key not found: " ++ key)

getStringFromData key dat = do
  dat <- getFromData key dat
  case dat of
    JSString s -> return (fromJSString s)
    _ -> fail "expected string"

getIntFromData key dat = do
  dat <- getFromData key dat
  case dat of
    JSRational _ r -> return (floor r)
    _ -> fail "expected int"

getMessageTypeAndData obj = case (lookup "type" obj, lookup "data" obj) of
  (Just (JSString jstyp), Just (JSObject obj)) ->
    Ok (fromJSString jstyp, (fromJSObject obj))
  (Nothing, _) -> Error "type not found"
  (_, _) -> Error "data not found"

wireMessageFromString :: String -> Result ClientWireMessage
wireMessageFromString str = decode str >>= readJSON 

data ServerMessage = LoggedIn
                   | LoginErr String
                   | RoomsInfo [(Int, Int, [String])]
                   | Joined Int Bool
                   | JoinErr String
                   | RoomData [String]
                   | Prepare [(String, Float, Float, Float)]


makeJSONPacket :: String -> [(String, JSValue)] -> JSValue
makeJSONPacket typ dat = JSObject $ toJSObject [("type", JSString $ toJSString typ),
                                                ("data", JSObject $ toJSObject dat)]


makeEmptyJSONPacket :: String -> JSValue
makeEmptyJSONPacket typ = makeJSONPacket typ []

instance JSON ServerMessage where
  showJSON LoggedIn = makeEmptyJSONPacket "logged_in"
  showJSON (LoginErr str) = makeJSONPacket "login_err" [("error", JSString $ toJSString str)]
  showJSON (RoomsInfo rooms) = makeJSONPacket "room_list" [("rooms", JSArray $ map roomToJSON rooms)]
    where roomToJSON (id, capacity, nicks) =
            JSObject $ toJSObject [("id", JSRational False (fromIntegral id)),
                                   ("capacity", JSRational False (fromIntegral capacity)),
                                   ("nicks", JSArray $ map (JSString . toJSString) nicks)]
  showJSON (Joined id isOwner) = makeJSONPacket "joined"
                                 [("id", JSRational False (fromIntegral id)),
                                  ("owner", JSBool isOwner)]
  showJSON (RoomData nicks) = makeJSONPacket "room_data"
                              [("people", JSArray $ map (JSString . toJSString) nicks)]
  showJSON (Prepare startData) = makeJSONPacket "prepare"
                                 [("people", JSArray $ map toStartInfo startData)]
    where toStartInfo (nick, x, y, direction) = JSObject $ toJSObject [("nick", JSString $ toJSString nick),
                                                                       ("x", JSRational False $ toRational x),
                                                                       ("y", JSRational False $ toRational y),
                                                                       ("direction", JSRational False $ toRational direction)]
  readJSON = undefined -- we won't need this

data Client = Client {
  nick :: String,
  chan :: TChan ServerMessage
}

data Room = Room {
  capacity :: Int,
  roomClients :: [Client]
  }
data ServerData = ServerData {
  roomCount :: Int,
  rooms :: Map.Map Int Room,
  clients :: Map.Map String Client,
  clientRooms :: Map.Map String Int
}

getRoomsInfo :: ServerState [(Int, Int, [String])]
getRoomsInfo = (map prepareInfo . Map.toList . rooms) `fmap` get
  where prepareInfo (id, Room { capacity = capacity,
                                roomClients = clients}) =(id, capacity, map nick clients)

getRoomData :: Int -> ServerState [String]
getRoomData id = (map nick . roomClients . (Map.! id) . rooms) `fmap` get
getClients :: ServerState (Map.Map String Client)
getClients = clients `fmap` get

hasClient nick = Map.member nick `fmap` getClients
getClient nick = (Map.! nick) `fmap` getClients
getClientChan nick = chan `fmap` getClient nick
getClientRoom nick = ((Map.! nick) . clientRooms) `fmap` get

createRoom nick capacity = do
  e <- get
  let ServerData { roomCount = n,
                   rooms = rs,
                   clients = cs,
                   clientRooms = crs } = e
  put $ e { rooms = Map.insert n (Room capacity [cs Map.! nick]) rs,
            roomCount = n+1, 
            clientRooms = Map.insert nick n crs}
  return n

getRoom id = (Map.lookup id . rooms) `fmap` get
joinRoom nick id = do
  e <- get
  client <- getClient nick
  let r = rooms e
      room = r Map.! id
      clients = roomClients room
      crs = clientRooms e
  put $ e { rooms = Map.insert id (room { roomClients = client : clients}) r,
            clientRooms = Map.insert nick id crs }

updateCapacity id capacity = do
  e <- get
  let rs = rooms e
      r = rs Map.! id
  put $ e { rooms = Map.insert id (r { capacity = capacity }) rs }

type ServerState = StateT ServerData IO

atomicallyState :: STM a -> ServerState a
atomicallyState = liftIO . atomically

sendToClient :: TChan ServerMessage -> ServerMessage -> ServerState ()
sendToClient c m = atomicallyState $ writeTChan c m

sendToRoom :: Room -> ServerMessage -> ServerState ()
sendToRoom room msg = mapM_ ((flip sendToClient $ msg) . chan) (roomClients room)

sendToServer :: TChan ClientMessage -> ClientMessage -> IO ()
sendToServer c m = atomically $ writeTChan c m

main :: IO ()
main = withSocketsDo $ do
  servSock <- listenOn $ PortNumber 9160
  chan <- atomically newTChan
  forkIO $ acceptLoop servSock chan
  evalStateT (mainLoop chan) (ServerData 0 Map.empty Map.empty Map.empty)

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

handleClientMessage (ClientConnected nick clientChan) = do
  nickTaken <- hasClient nick
  if not nickTaken
    then addClient nick clientChan
    else sendToClient clientChan (LoginErr "nick already exists")
    where addClient nick clientChan = do
            e <- get
            put $ e { clients = Map.insert nick (Client nick clientChan) (clients e) }
            sendToClient clientChan LoggedIn

handleClientMessage (ClientAsksForRooms nick) = do
  c <- getClientChan nick
  rooms <- getRoomsInfo
  sendToClient c (RoomsInfo rooms)

handleClientMessage (ClientCreatesRoom nick capacity) = do
  n <- createRoom nick capacity
  c <- getClientChan nick
  sendToClient c (Joined n True)
  sendToClient c (RoomData [nick])

handleClientMessage (ClientJoinsRoom nickname room) = do
  r <- getRoom room
  c <- getClientChan nickname
  case r of
    Nothing -> sendToClient c (JoinErr "room does not exist")
    Just (Room { capacity = capacity, roomClients = clients }) ->
      if length clients < capacity
      then do
        joinRoom nickname room
        sendToClient c $ Joined room False
        Just (Room { roomClients = clients'}) <- getRoom room
        mapM_ (flip sendToClient . RoomData . map nick $ clients') (map chan clients')
      else sendToClient c (JoinErr "room full")

handleClientMessage (ClientStarts nickname) = do
  rId <- getClientRoom nickname
  c <- getClientChan nickname
  Just r <- getRoom rId
  let clients = roomClients r
  updateCapacity rId (length clients)
  poss <- lift (replicateM (length clients) randomPosition)
  sendToRoom r (Prepare $ zipWith zipClientPos clients poss)
  where randomPosition = do
          x <- randomRIO (-1.0, 1.0)
          y <- randomRIO (-1.0, 1.0)
          direction <- randomRIO (0, 2*pi)
          return (x, y, direction)
        zipClientPos client (x, y, direction) = (nick client, x, y, direction)

newClient :: TChan ClientMessage -> Request -> WebSockets Hybi10 ()
newClient chan rq = do
  liftIO $ putStrLn "Ktos przylazl"
  acceptRequest rq
  msg <- (receiveData :: WebSockets Hybi10 String)
  case wireMessageFromString msg of
    Ok (Hello nick) -> handleHelloMessage nick
    Error str -> liftIO $ putStrLn ("JSON error: " ++ str)
    _ -> liftIO $ putStrLn ("Unexpected message")
  return ()
  where handleHelloMessage nick = do
          clientChan <- liftIO . atomically $ newTChan
          networkChan <- liftIO . atomically $ newTChan
          sink <- getSink
          liftIO $ putStrLn (nick ++ " connected")
          liftIO . atomically $ writeTChan chan (ClientConnected nick clientChan)
          liftIO $ forkIO $ clientLoop clientChan chan networkChan sink nick
          clientNetworkLoop networkChan

clientLoop :: TChan ServerMessage
              -> TChan ClientMessage
              -> TChan ClientWireMessage
              -> Sink Hybi10 -> String -> IO ()
clientLoop fromServerChan toServerChan fromNetworkChan toNetworkSink nick = do
  action <- atomically getMessage
  case action of
    Left server -> sendSink toNetworkSink . textData . encode . showJSON $ server
    Right network -> putStrLn (show network) >> handleNetworkMessage network
  clientLoop fromServerChan toServerChan fromNetworkChan toNetworkSink nick
  where getMessage = (Left `fmap` readTChan fromServerChan)
                     `orElse` (Right `fmap` readTChan fromNetworkChan)
        handleNetworkMessage GetRooms = sendToServer toServerChan (ClientAsksForRooms nick)
        handleNetworkMessage (CreateRoom capacity) = sendToServer toServerChan (ClientCreatesRoom nick capacity)
        handleNetworkMessage (Join id) = sendToServer toServerChan (ClientJoinsRoom nick id)
        handleNetworkMessage Start = sendToServer toServerChan (ClientStarts nick)
        handleNetworkMessage _ = return ()

clientNetworkLoop networkChan = do
  msg <- (receiveData :: WebSockets Hybi10 String)
  case wireMessageFromString msg of
    Ok wireMessage -> liftIO $ atomically $ writeTChan networkChan wireMessage
    Error str -> liftIO $ putStrLn ("JSON error: " ++ str)
  clientNetworkLoop networkChan
