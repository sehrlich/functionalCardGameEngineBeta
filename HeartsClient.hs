module HeartsClient
    ( constructGUIPlayer
    , constructPlayer
    , client
    , aiclient
    , Player 
    )
    where
import HeartsCommon
import PlayingCards
import qualified Data.Set as Z
import qualified Data.Foldable as F

import Control.Concurrent
import Control.Concurrent.STM
-- import Control.Concurrent.STM.TMVar

import Control.Monad (forever)

import Graphics.Gloss

type Player = (TMVar ServerToClient, TMVar ClientToServer, ThreadId) -- ??

constructPlayer :: (ServerToClient -> IO ClientToServer) -> IO Player
constructPlayer respondTo
    = do
    inbox  <- newEmptyTMVarIO -- :: (TMVar ServerToClient)
    outbox <- newEmptyTMVarIO -- :: (TMVar ClientToServer)
    thread <- forkIO $ playerThread inbox outbox
    return (inbox, outbox, thread)

    where playerThread inbox outbox = forever $ do
            message <- atomically $ takeTMVar inbox
            response <- respondTo message
            atomically $ putTMVar outbox response

constructGUIPlayer :: IO Player
constructGUIPlayer
    = do
    inbox  <- newEmptyTMVarIO -- :: (TMVar ServerToClient)
    outbox <- newEmptyTMVarIO -- :: (TMVar ClientToServer)
    thread <- forkIO $ guiThread inbox outbox
    return (inbox, outbox, thread)

guiThread :: TMVar ServerToClient -> TMVar ClientToServer -> IO ()
guiThread _inbox _outbox
    = do play
            (InWindow
            "Hearts" 	 -- window title
            (400, 150) 	 -- window size
            (10, 10)) 	 -- window position
            white			 -- background color
            100              -- steps per second
            ""               -- world 
            displayText      -- picture to display
            eventHandle      -- event handler
            (\_ world -> world) -- time update
    where displayText outpt = Translate (-170) (-20)
                  $ Scale 0.5 0.5
                  $ Text outpt
          eventHandle event _world = show event


{-- Client Side code
 -- actual mechanism of splitting it as thread to be determined
 --
 -- Should split some validation stuff out so that
 -- it is accessible to both server and client --
 -- my client should always send valid input
 -- if server receives bad messages, it should check them
 --
 -- Also, rendering should go here
 --}

client :: ServerToClient -> IO ClientToServer

client StcGameStart = do
    return CtsAcknowledge

client (StcGetMove hand info) = do
    card <- getMove hand info
    return $ CtsMove card

client (StcGetPassSelection hand _passDir) = do
   -- render $ Passing hand passDir
   cardSet <- getMultiCards 3 hand
   -- do client validation here
   return $ CtsPassSelection cardSet

client StcGameOver = return CtsDisconnect

getMove :: Hand -> Info -> IO Card
getMove hand info = do
    card <- getCardFromHand hand
    if isValidPlay hand info card
    then return card
    else do
        putStrLn "Illegal move: must follow suit"
        getMove hand info

getMultiCards :: Int -> Hand -> IO (Z.Set Card)
getMultiCards 0 _ = return Z.empty
--getMultiCards _ empty = return Z.empty
getMultiCards i hand = do
    card <- getCardFromHand hand
    others <- getMultiCards (i-1) (Z.delete card hand)
    return $ card `Z.insert` others


getCardFromHand :: Hand -> IO Card
getCardFromHand hand = do
    -- renderHand hand
    card <- getInput
    if card `Z.member` hand
    then return card
    else do
        putStrLn "Error: Card not in hand"
        getCardFromHand hand

getInput :: IO Card
getInput = do
    putStrLn "Choose Card: "
    -- for hearts players only choices in the play are which card to play
    -- We'll check that it's a legal play before constructing the effect
    mv <- getLine
    -- TODO: try parsing meta options and so forth too
    case readCard mv of
        Nothing -> do
                    putStrLn "Could not interpret move!"
                    getInput
        Just c -> return c

{- The trivial ai -}
{- should replace with random choice -}
aiclient :: ServerToClient -> IO ClientToServer
aiclient (StcGetMove hand info) =
    case F.find (isValidPlay hand info) $ Z.toList hand of
        Nothing   -> error "apparently cannot play card"
        Just card -> return $ CtsMove card

aiclient (StcGetPassSelection hand _passDir) = do
    let cardSet = Z.fromList $ take 3 $ Z.toList hand
    return $ CtsPassSelection cardSet

aiclient StcGameStart = return CtsAcknowledge
aiclient StcGameOver = return CtsDisconnect
