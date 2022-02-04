{-# LANGUAGE OverloadedStrings #-}

module Main where

-- -- Imports  -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
import Data.Time
import Data.Text (Text)
import Data.Monoid (mappend)
import Data.Char (isPunctuation, isSpace)

import Text.Printf

import Control.Exception (finally)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (forM_, when, unless)
import Control.Monad.Loops (untilM_, iterateUntil)
import Control.Concurrent (MVar, newMVar, modifyMVar_, modifyMVar, readMVar)

import qualified Database.MySQL.Base as Sql
import qualified System.IO.Streams as Streams
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Network.WebSockets as WS


-- -- General Data and Methods   -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
data Logger = SaveMsg | LoadMsg | GotMsg

data User = User {uName :: Text
                 ,uRoom :: Text
                 ,uConnection :: WS.Connection
                 }

type AppState = [User]

newAppState :: AppState
newAppState = []

addUser :: User -> AppState -> AppState
addUser user users = user : users

newUser :: Text -> Text -> WS.Connection -> User
newUser name room connection =
  User {uName = name
       ,uRoom = room
       ,uConnection = connection}

removeUser :: User -> AppState -> AppState
--removeUser user appState = filter (\u -> uName u /= uName user) appState
removeUser user appState = filter ((/= uName user) . uName) appState

userExists :: Text -> AppState -> Bool
userExists name appState = any (\u -> uName u == name) appState


-- -- Broadcast's methods  -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
broadcast :: Text -> AppState -> IO ()
broadcast message users = do
  forM_ users $ \(User _ _ c) -> WS.sendTextData c message

broadcastInRoom :: Text -> Text -> AppState -> IO ()
broadcastInRoom message room users = do
  forM_ users $ \user -> case user of
    (User _ r c) | (r == room) -> WS.sendTextData c message
                 | otherwise   -> return ()

sendPrivateMsg :: Text -> Text -> AppState -> IO ()
sendPrivateMsg message name users = do
  forM_ users $ \user -> case user of
    (User n _ c) | (n == name) -> WS.sendTextData c message
                 | otherwise   -> return ()


-- -- Rooms's methods   -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
getAllUsersInRoom :: AppState -> Text -> Text
getAllUsersInRoom appState room = case appState of
  [] -> "" :: Text
  (x:xs) | ((uRoom x) == room) -> (uName x) `mappend` ";" `mappend` getAllUsersInRoom xs room
  (x:xs) | ((uRoom x) /= room) -> getAllUsersInRoom xs room


-- -- For debug   -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
debug1 :: AppState -> IO ()
debug1 as = do
  print (length as)
  forM_ as $ \user -> case user of
    (User n r _) -> print (n `mappend` " in " `mappend` r)


-- -- Run app and main loop   -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
main :: IO ()
main = do
  appState <- newMVar newAppState
  WS.runServer "127.0.0.1" 8080 $ runApp appState

runApp :: MVar AppState -> WS.ServerApp
runApp appState pending = do
  connection <- WS.acceptRequest pending
  WS.withPingThread connection 30 (return ()) $ do
    WS.sendTextData connection ("@introducing:Hey! First of all you need to choose a name:" :: Text)
    message <- WS.receiveData connection
    users   <- readMVar appState
    case message of
      _ | ("@name " `T.isPrefixOf` message) && not (userExists (T.drop 6 message) users) -> flip finally disconnect $ do
            liftIO $ modifyMVar_ appState $ \as -> do
              WS.sendTextData connection ("@introducing-succ:true" :: Text)
              let as' = addUser user as
              loadMessages "General" connection
              WS.sendTextData connection ("@system:Welcome! " `mappend` (uName $ head as'):: Text)
              broadcastInRoom ("@system:" `mappend` (uName $ head as') `mappend` ";@message:joined to 'General' room") "General" as'
              broadcastInRoom ("@listOfUsersInRoom:" `mappend` (getAllUsersInRoom as' "General")) "General" as'
              updateListOfRooms as'
              return as'
            mainLoop appState user
        | otherwise -> WS.sendTextData connection ("@introducing:The specified name already exists, try it later." :: Text)
        where
          user = User (T.drop 6 message) "General" connection
          disconnect = do
            as <- modifyMVar appState $ \as ->
              let as' = removeUser user as in return (as', as')
            broadcastInRoom ("@system:" `mappend` (uName user) `mappend` ";@message:disconnected") (uRoom user) as
            broadcastInRoom ("@listOfUsersInRoom:" `mappend` (getAllUsersInRoom as (uRoom user))) (uRoom user) as

mainLoop :: MVar AppState -> User -> IO ()
mainLoop appState u = do
  rMessage <- WS.receiveData (uConnection u)
  currentTime <- getCurrentTime
  let d = T.dropEnd 11 $ T.pack (show currentTime)
  printf "Got message: %s; From: %s; Time: %s" rMessage (uName u) d
  case rMessage of
    _ | ("@private " `T.isPrefixOf` rMessage) -> do
            let msg = T.unwords $ drop 2 $ T.words rMessage
            let sMessage = buildMessage "@private:" msg (uName u) (uRoom u) d
            liftIO $ readMVar appState >>= sendPrivateMsg sMessage (T.words rMessage !! 1)
            liftIO $ readMVar appState >>= sendPrivateMsg sMessage (uName u)
            mainLoop appState u
    _ | ("@room " `T.isPrefixOf` rMessage) -> do
            let newRoom = T.drop 6 rMessage
            liftIO $ modifyMVar_ appState $ \as -> do
              let as1 = removeUser u as
              let as2 = addUser (User (uName u) newRoom (uConnection u)) as1
              loadMessages newRoom (uConnection u)
              broadcastInRoom ("@listOfUsersInRoom:" `mappend` (getAllUsersInRoom as2 $ uRoom u)) (uRoom u) as2
              broadcastInRoom ("@listOfUsersInRoom:" `mappend` (getAllUsersInRoom as2 newRoom)) newRoom as2
              broadcastInRoom ("@system:" `mappend` (uName u) `mappend` ";@message:left the room") (uRoom u) as2
              broadcastInRoom ("@system:" `mappend` (uName u) `mappend` ";@message:joined the room") newRoom as2
              saveMessage (User "@SYSTEM" newRoom (uConnection u)) "todo later"
              updateListOfRooms as2
              return as2
            mainLoop appState (User (uName u) newRoom (uConnection u))
    otherwise -> do
            saveMessage u rMessage
            let sMessage = buildMessage "@message:" rMessage (uName u) (uRoom u) d
            liftIO $ readMVar appState >>= broadcastInRoom sMessage (uRoom u)
            mainLoop appState u


-- -- All for SQL -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
getSQLconnect :: IO Sql.MySQLConn
getSQLconnect = do
  sqlConnection <- Sql.connect Sql.defaultConnectInfo
    {Sql.ciUser     = "root"
    ,Sql.ciPassword = "root"
    ,Sql.ciDatabase = "db_p_n"
    }
  return sqlConnection

loadMessages :: Text -> WS.Connection -> IO ()
loadMessages room connection = do
    sqlConnection <- getSQLconnect
    stmt <- Sql.prepareStmt sqlConnection "SELECT * FROM messages WHERE room=? AND uName != '@SYSTEM';"
    (defs, is) <- Sql.queryStmt sqlConnection stmt [Sql.MySQLText room]
    queryAsList <- Streams.toList is
    forM_ queryAsList $ \e -> do
      -- from [MySQLText "..."] to Text
      let n = T.dropEnd 1 $ T.drop 11 $ T.pack (show $ e !! 1)
      let m = T.dropEnd 1 $ T.drop 11 $ T.pack (show $ e !! 3)
      let d = T.dropEnd 0 $ T.drop 14 $ T.pack (show $ e !! 4)
      let msg = buildMessage "@message:" m n room d
      WS.sendTextData connection msg
    printf "-- History of room %s was loaded\n" room

saveMessage :: User -> Text -> IO ()
saveMessage (User n r _) m = do
  sqlConnection <- getSQLconnect
  stmt <- Sql.prepareStmt sqlConnection "INSERT INTO `db_p_n`.`messages` (`uName`, `room`, `message`) VALUES (?,?,?)"
  let name = Sql.MySQLText n
  let room = Sql.MySQLText r
  let mssg = Sql.MySQLText m
  Sql.executeStmt sqlConnection stmt [name, room, mssg]
  printf "-- Message from %s was saved\n" n

updateListOfRooms :: AppState -> IO ()
updateListOfRooms users = do
  sqlConnection <- getSQLconnect
  (defs, is) <- Sql.query_ sqlConnection "SELECT DISTINCT room FROM messages"
  queryAsList <- Streams.toList is
  let r = lorAsText queryAsList
  forM_ users $ \(User _ _ c) -> WS.sendTextData c ("@listOfRooms:" `mappend` r)
  print "List of rooms was updated"

lorAsText :: (Show a) => [a] -> Text
lorAsText list = case list of
  []     -> ""
  (x:xs) -> (T.drop 12 $ T.dropEnd 2 $ T.pack (show $ x))
            `mappend` ";" `mappend` lorAsText xs


-- -- Help methods   -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
buildMessage :: Text -> Text -> Text -> Text -> Text -> Text
buildMessage t m n r d = t         `mappend` m `mappend`
                         ";@name:" `mappend` n `mappend`
                         ";@room:" `mappend` r `mappend`
                         ";@date:" `mappend` d






--
