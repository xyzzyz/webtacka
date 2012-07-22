{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}
module Main where

import Text.JSON
import Data.String
import Data.Maybe
import qualified Data.Map as Map
import Network (listenOn, PortID(PortNumber))
import Network.Socket (accept, withSocketsDo, Socket)
import Network.WebSockets
import System.IO
import System.Environment
import System.Random
import Control.Monad
import Control.Applicative
import Control.Monad.State
import Control.Monad.Trans
import Control.Exception
import Control.Concurrent
import Control.Concurrent.STM

import qualified Data.ByteString.Lazy.Char8 as B

import CollisionDetection

instance WebSocketsData String where
  fromLazyByteString = B.unpack
  toLazyByteString = B.pack

data ClientMessage = ClientConnected String (TChan ServerMessage)
                   | ClientAsksForRooms String
                   | ClientCreatesRoom String Int
                   | ClientJoinsRoom String Int
                   | ClientStarts String
                   | ClientContinues String
                   | ClientSetsDirection String DirectionChange
                   | DeferredStart Int
                   | Tick

data ClientWireMessage = Hello String
                       | GetRooms
                       | CreateRoom Int
                       | Join Int
                       | Start
                       | Continue
                       | ClientMove DirectionChange
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
      "continue" -> Ok Start
      "client_move" -> ClientMove `fmap` getDirectionChangeFromData dat
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

getDirectionChangeFromData dat = do
  c <- getStringFromData "direction" dat
  case c of
    "left" -> return ChangeLeft
    "right" -> return ChangeRight
    "none" -> return None
    _ -> fail "wrong direction"

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
                   | StartGame
                   | GameTick [(String, Float, Float, Float)]
                   | PlayerDead String [(String, Int)]
                   | EndGame

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
  showJSON (JoinErr str) = makeJSONPacket "join_err" [("error", JSString $ toJSString str)]
  showJSON (RoomData nicks) = makeJSONPacket "room_data"
                              [("people", JSArray $ map (JSString . toJSString) nicks)]
  showJSON (Prepare startData) = makeJSONPacket "prepare"
                                 [("people", JSArray $ map positionToMove startData)]
  showJSON StartGame = makeEmptyJSONPacket "game_started"
  showJSON (GameTick moves) = makeJSONPacket "game_tick"
                              [("moves", JSArray $ map positionToMove moves)]
  showJSON (PlayerDead nick scoreBoard) = makeJSONPacket "player_dead"
                               [("nick", JSString $ toJSString nick),
                                ("scoreboard", JSArray $ map makeScore scoreBoard)]
    where makeScore (nick, s) = JSObject $ toJSObject [("nick", JSString $ toJSString nick),
                                                       ("score", JSRational False (fromIntegral s))]
  showJSON EndGame = makeEmptyJSONPacket "game_ended"
  readJSON = undefined -- we won't need this

type Position = (Float, Float)

positionToMove (nick, x, y, direction) =
  JSObject $ toJSObject [("nick", JSString $ toJSString nick),
                         ("x", JSRational False $ toRational x),
                         ("y", JSRational False $ toRational y),
                         ("direction", JSRational False $ toRational direction)]


data DirectionChange = None | ChangeLeft | ChangeRight
                     deriving (Show)
changeToSign :: DirectionChange -> Float
changeToSign None = 0.0
changeToSign ChangeLeft = -1.0
changeToSign ChangeRight = 1.0

data Client = Client {
  nick :: String,
  chan :: TChan ServerMessage,
  positions :: [Position],
  direction :: Float,
  directionChange :: DirectionChange,
  alive :: Bool,
  score :: Int
}

data Room = Room {
  capacity :: Int,
  roomClients :: [String],
  isActive :: Bool,
  collisionTree :: Tree,
  oldCollisionTree :: Tree
  }
data ServerData = ServerData {
  roomCount :: Int,
  rooms :: Map.Map Int Room,
  clients :: Map.Map String Client,
  clientRooms :: Map.Map String Int,
  serverChan :: TChan ClientMessage
}

type ServerState = StateT ServerData IO

initSegments :: [Segment]
initSegments = [Segment (Point (-1.0) (-1.0)) (Point (-1.0) 1.0),
                Segment (Point (-1.0) (-1.0)) (Point 1.0 (-1.0)),
                Segment (Point (-1.0) 1.0) (Point 1.0 1.0),
                Segment (Point 1.0 (-1.0)) (Point 1.0 1.0)]

initTree :: Tree
initTree = foldl addSegment (buildTree (Rectangle (-1.0) 1.0 1.0 (-1.0))) initSegments

createRoom nick capacity = do
  e <- get
  let ServerData { roomCount = n,
                   rooms = rs,
                   clients = cs,
                   clientRooms = crs } = e
  put $ e { rooms = Map.insert n (Room capacity [nick] False initTree initTree)
                    rs,
            roomCount = n+1,
            clientRooms = Map.insert nick n crs}
  return n

getRoom id = (Map.lookup id . rooms) `fmap` get
joinRoom nick id = do
  e <- get
  let r = rooms e
      room = r Map.! id
      nicks = roomClients room
      crs = clientRooms e
  put $ e { rooms = Map.insert id (room { roomClients = nick : nicks}) r,
            clientRooms = Map.insert nick id crs }

updateRoom id room = do
  e <- get
  put $ e { rooms = Map.insert id room (rooms e) }

updateCapacity id capacity = do
  e <- get
  let rs = rooms e
      r = rs Map.! id
  put $ e { rooms = Map.insert id (r { capacity = capacity }) rs }

setGameStatus id act  = do
  e <- get
  let rs = rooms e
      r = rs Map.! id
  put $ e { rooms = Map.insert id (r { isActive = act }) rs }


atomicallyState :: STM a -> ServerState a
atomicallyState = liftIO . atomically

sendToClient :: TChan ServerMessage -> ServerMessage -> ServerState ()
sendToClient c m = atomicallyState $ writeTChan c m

sendToRoom :: Room -> ServerMessage -> ServerState ()
sendToRoom room msg = do
  clients <- mapM getClient (roomClients room)
  mapM_ ((flip sendToClient $ msg) . chan) clients

sendToServer :: TChan ClientMessage -> ClientMessage -> IO ()
sendToServer c m = atomically $ writeTChan c m

getRoomsInfo :: ServerState [(Int, Int, [String])]
getRoomsInfo = (map prepareInfo . Map.toList . rooms) `fmap` get
  where prepareInfo (id, Room { capacity = capacity,
                                roomClients = nicks}) = (id, capacity, nicks)

getScoreBoard :: Room -> ServerState [(String, Int)]
getScoreBoard room = do
  clients <- mapM getClient (roomClients room)
  return $ map ((,) <$> nick <*> score) clients

getRoomData :: Int -> ServerState [String]
getRoomData id = (roomClients . (Map.! id) . rooms) `fmap` get
getClients :: ServerState (Map.Map String Client)
getClients = clients `fmap` get

hasClient nick = Map.member nick `fmap` getClients
getClient nick = (Map.! nick) `fmap` getClients
getClientChan nick = chan `fmap` getClient nick
getClientRoom nick = ((Map.! nick) . clientRooms) `fmap` get
updateClient nick client = do
  e <- get
  put $ e { clients = Map.insert nick client (clients e) }

tickTime :: Int
tickTime = 30

main :: IO ()
main = withSocketsDo $ do
  servSock <- listenOn $ PortNumber 9160
  chan <- atomically newTChan
  forkIO $ acceptLoop servSock chan
  forkIO $ tickLoop tickTime chan
  evalStateT (mainLoop chan) (ServerData 0 Map.empty Map.empty Map.empty chan)

tickLoop :: Int -> TChan ClientMessage -> IO ()
tickLoop tick chan = do
  threadDelay (1000*tick)
  atomically $ writeTChan chan Tick
  tickLoop tick chan

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
            put $ e { clients = Map.insert nick (Client nick clientChan [] 0.0 None True 0) (clients e) }
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
        Just (Room { roomClients = nicks}) <- getRoom room
        clients' <- mapM getClient nicks
        mapM_ (flip sendToClient . RoomData $ nicks) (map chan clients')
      else sendToClient c (JoinErr "room full")

handleClientMessage (ClientStarts nickname) = do
  rId <- getClientRoom nickname
  c <- getClientChan nickname
  Just r <- getRoom rId
  let nicks = roomClients r
  updateCapacity rId (length nicks)
  newGame rId

handleClientMessage (ClientContinues nickname) = do
  rId <- getClientRoom nickname
  Just r <- getRoom rId
  mapM_ resurrectClient (roomClients r)
  newGame rId
  where resurrectClient nick = do
          client <- getClient nick
          updateClient nick (client { alive = True })

handleClientMessage (ClientSetsDirection nick change) = do
  client <- getClient nick
  updateClient nick (client { directionChange = change })


handleClientMessage (DeferredStart id) = do
  Just r <- getRoom id
  sendToRoom r StartGame
  setGameStatus id True

handleClientMessage Tick = do
  rooms <- rooms `fmap` get
  let activeRooms = Map.toList (Map.filter isActive rooms)
  mapM_ handleRoom activeRooms
  return ()

newGame rId = do
  Just r <- getRoom rId
  let nicks = roomClients r
  poss <- lift (replicateM (length nicks) randomPosition)
  positions <- zipWithM zipClientPos nicks poss
  sendToRoom r (Prepare positions)
  sc <- serverChan `fmap` get
  lift (forkIO $ startGame rId sc)
  return ()
  where randomPosition = do
          x <- randomRIO (-0.7, 0.7)
          y <- randomRIO (-0.7, 0.7)
          direction <- randomRIO (0, 2*pi)
          return (x, y, direction)
        zipClientPos nick (x, y, direction) = do
          client <- getClient nick
          let ps = positions client
          updateClient nick (client { positions = (x, y):ps,
                                      direction = direction })
          return (nick, x, y, direction)


handleRoom :: (Int, Room) -> ServerState ()
handleRoom (rId, r@(Room { roomClients = cs,
                           collisionTree = tree,
                           oldCollisionTree = oldTree })) = do
  clients <- mapM getClient cs
  positions <- mapM adjustPosition (filter alive clients)
  sendToRoom r (GameTick positions)
  Just r' <- getRoom rId
  updateRoom rId (r' { oldCollisionTree = tree })
  newClients <- mapM getClient cs
  when (length (filter alive clients) < 2) $ do
    setGameStatus rId False
    sendToRoom r' EndGame
  where dphi = 0.9
        dp = 0.3
        dt = (fromRational (fromIntegral tickTime / 1000) :: Float)
        adjustPosition (client @(Client { nick = n,
                                          positions = pss@((x, y):ps),
                                          direction = phi,
                                          directionChange = change })) = do
          let phi' = mod2pi (phi + (changeToSign change)*dphi*pi*dt)
              x' = x + (sin phi) * dt * dp
              y' = y + (cos phi) * dt * dp
              segment = Segment (Point x y) (Point x' y')
          case segTreeIntersection segment oldTree of
            Just segment' -> do
              updateClient n (client { alive = False })
              clients' <- mapM getClient cs
              mapM_ givePoint (filter alive clients')
              Just r' <- getRoom rId
              scoreBoard <- getScoreBoard r'
              sendToRoom r (PlayerDead n scoreBoard)
            Nothing -> do
              Just r' <- getRoom rId
              updateRoom rId (r' { collisionTree = addSegment (collisionTree r') segment})
              updateClient n (client { positions = (x', y'):pss,
                                       direction = phi'})
          return (n, x', y', phi')
          where mod2pi phi | phi < 0 = 2*pi - phi
                           | phi >= 2*pi = phi - 2*pi
                           | otherwise = phi
                givePoint c@(Client { nick = n, score = s }) = do
                  updateClient n (c { score = s+1})


startGame rId serverChan = do
  threadDelay (3*1000*1000)
  atomically $ writeTChan serverChan (DeferredStart rId)
  return ()

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
    Right network -> (putStrLn (show network)) >> handleNetworkMessage network
  clientLoop fromServerChan toServerChan fromNetworkChan toNetworkSink nick
  where getMessage = (Left `fmap` readTChan fromServerChan)
                     `orElse` (Right `fmap` readTChan fromNetworkChan)
        handleNetworkMessage GetRooms = sendToServer toServerChan (ClientAsksForRooms nick)
        handleNetworkMessage (CreateRoom capacity) = sendToServer toServerChan (ClientCreatesRoom nick capacity)
        handleNetworkMessage (Join id) = sendToServer toServerChan (ClientJoinsRoom nick id)
        handleNetworkMessage Start = sendToServer toServerChan (ClientStarts nick)
        handleNetworkMessage Continue = sendToServer toServerChan (ClientContinues nick)
        handleNetworkMessage (ClientMove change) = sendToServer toServerChan (ClientSetsDirection nick change)
        handleNetworkMessage _ = return ()

clientNetworkLoop networkChan = do
  msg <- (receiveData :: WebSockets Hybi10 String)
  case wireMessageFromString msg of
    Ok wireMessage -> liftIO $ atomically $ writeTChan networkChan wireMessage
    Error str -> liftIO $ putStrLn ("JSON error: " ++ str)
  clientNetworkLoop networkChan
