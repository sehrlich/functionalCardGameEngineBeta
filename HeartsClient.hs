module HeartsClient
    ( constructGUIPlayer
    , constructPlayer
    , clientTextBased
    , aiclient
    , Player
    -- , renderText
    )
    where
import HeartsCommon
import HeartsGui (guiThread)
import HeartsTui
import qualified Data.Set as Z
import qualified Data.Foldable as F

import Control.Concurrent
-- import Control.Concurrent.Async
import Control.Concurrent.STM
-- import Control.Concurrent.STM.TMVar

import Control.Monad (forever)

type Player = (TMVar ServerToClient, TMVar ClientToServer, ThreadId, Int) -- ??

constructPlayer :: (ServerToClient -> IO ClientToServer) -> Int -> IO Player
constructPlayer respondTo pos
    = do
    inbox  <- newEmptyTMVarIO -- :: (TMVar ServerToClient)
    outbox <- newEmptyTMVarIO -- :: (TMVar ClientToServer)
    thread <- forkIO $ playerThread inbox outbox
    return (inbox, outbox, thread, pos)

    where playerThread inbox outbox = forever $ do
            message <- atomically $ takeTMVar inbox
            response <- respondTo message
            atomically $ putTMVar outbox response

constructGUIPlayer :: Int -> IO Player
constructGUIPlayer pos
    = do
    inbox  <- newEmptyTMVarIO -- :: (TMVar ServerToClient)
    outbox <- newEmptyTMVarIO -- :: (TMVar ClientToServer)
    thread <- forkIO $ guiThread inbox outbox pos
    return (inbox, outbox, thread, pos)

{-- Client Side code
 -- actual mechanism of splitting it as thread to be determined
 --
 -- Should split some validation stuff out so that
 -- it is accessible to both server and client --
 -- my client should always send valid input
 -- if server receives bad messages, it should check them
 --}

{- The trivial ai -}
{- should replace with random choice -}
aiclient :: ServerToClient -> IO ClientToServer
aiclient (StcGetMove hand info@(TrickInfo player trick scores heartsbroken)) = do
    threadDelay 1000000 -- sleep 1 second
    case F.find (isValidPlay hand info) $ Z.toList hand of
        Nothing   -> error $ unlines
                    ["apparently cannot play card"
                    , show hand
                    , show trick
                    , show heartsbroken
                    , show player
                    , show scores
                    ]
        Just card -> return $ CtsMove card

aiclient (StcGetPassSelection hand _passDir) = do
    threadDelay 500000 -- sleep 0.5 second
    let cardSet = Z.fromList $ take 3 $ Z.toList hand
    return $ CtsPassSelection cardSet

aiclient (StcGameStart _ ) = return CtsAcknowledge
aiclient (StcRender _rinfo) = return CtsAcknowledge
aiclient StcGameOver = return CtsDisconnect
