module HeartsClient
    ( constructGUIPlayer
    , constructPlayer
    , clientTextBased
    , aiclient
    , Player
    , RenderInfo(..)
    -- , renderText
    -- Communication Related
    -- , Message(..)
    , ClientToServer(..)
    , ServerToClient(..)
    )
    where
import HeartsCommon
import PlayingCards
import Data.Set (Set)
import qualified Data.Set as Z
import qualified Data.Sequence as S
import qualified Data.Foldable as F

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
-- import Control.Concurrent.STM.TMVar

import Control.Monad (forever)

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game (playIO)
-- text rendering
import Data.List (intercalate) -- colorize

type Player = (TMVar ServerToClient, TMVar ClientToServer, ThreadId, Int) -- ??

-- data Message = ClientToServer | ServerToClient
data ClientToServer = CtsMove Card
                    | CtsPassSelection (Set Card)
                    | CtsDisconnect
                    | CtsAcknowledge

data ServerToClient = StcGetMove Hand Info
                    | StcGetPassSelection Hand PassDir
                    | StcGameStart
                    | StcGameOver
                    | StcRender RenderInfo

data RenderInfo = RenderServerState Board Info
                | Passing Hand PassDir
                | BetweenRounds Scores
                | RenderInRound Hand Trick Scores
                | RenderEmpty

data RenderMode = RenderGame RenderInfo GuiState DebugInfo-- (Picture,pos) what player is currently moving

type DebugInfo = [String]

data GuiState   = DisplayOnly
                {-| SelectCardsToPass-}
                {-| SelectCardToPlay-}

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
    thread <- forkIO $ guiThread inbox outbox
    return (inbox, outbox, thread, pos)

guiThread :: TMVar ServerToClient -> TMVar ClientToServer -> IO ()
guiThread inbox outbox
    = do playIO
            window
            white			 -- background color
            100              -- steps per second
            (RenderGame RenderEmpty DisplayOnly [])     -- world
            drawWorld        -- picture to display
            eventHandle      -- event handler
            commHandle       -- time update
    where eventHandle event (RenderGame rinfo _gs dbgInfo)
            = return $ (RenderGame rinfo _gs ((show event):dbgInfo) )
          commHandle _t world
            = do
            -- check inbox
            message <- atomically $ tryTakeTMVar inbox
            -- return $ maybe world handleMessage message
            maybe (return world) (handleMessage_ outbox world) message
          window = (InWindow
                   "Gloss" 	    -- window title
                                -- title fixed for xmonad
                   (800, 600)   -- window size
                   (10, 10)) 	-- window position

{-handleMessage :: ServerToClient -> RenderMode
handleMessage m = undefined-}

handleMessage_ :: TMVar ClientToServer -> RenderMode -> ServerToClient -> IO RenderMode
handleMessage_ outbox world m
    = do
    {-response <- clientTextBased m-}
    {-atomically $ putTMVar outbox response-}
    _ <- async $ clientTextBased m >>= atomically . putTMVar outbox
    return $ case m of 
        StcRender rinfo -> RenderGame (rinfo) DisplayOnly []
        _ -> world
        

drawWorld :: RenderMode -> IO Picture
drawWorld (RenderGame mri _gs debugInfo)
    = do
    -- render debugInfo
    let dbg = Color rose $ Translate (0) (50) $ scale (0.125) (0.125) $ text $ unlines $ take 4 debugInfo
    -- will want to use viewports for pictures
    return $ Pictures [dbg, render mri]
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

clientTextBased :: ServerToClient -> IO ClientToServer

clientTextBased (StcRender rinfo) = do
    renderText rinfo
    return CtsAcknowledge

clientTextBased StcGameStart = do
    return CtsAcknowledge

clientTextBased (StcGetMove hand info) = do
    card <- getMove hand info
    return $ CtsMove card

clientTextBased (StcGetPassSelection hand _passDir) = do
   -- render $ Passing hand passDir
   cardSet <- getMultiCards 3 hand
   -- do client validation here
   return $ CtsPassSelection cardSet

clientTextBased StcGameOver = return CtsDisconnect

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

aiclient StcGameStart = return CtsAcknowledge
aiclient (StcRender _rinfo) = return CtsAcknowledge
aiclient StcGameOver = return CtsDisconnect
---------------------------------------------
-- Gui stuff
-- Render type
-- and rendering functions that get wrapped up
-- for gloss
render :: RenderInfo -> Picture
render RenderEmpty = Blank
render (RenderInRound hand played _scores)
    = Pictures
        [ Translate (0) (-200) $ renderHand hand
        , Translate (0) (-50) $ renderPlay played
        ]
        -- use viewports for pictures
render (RenderServerState _ _) = Circle 2
render (Passing hand dir)
    = Pictures
        [ Text $ show dir
        , Translate (0) (-200) $ renderHand hand
        ]
render (BetweenRounds _) = ThickCircle 50 8
    {-let playArea  = renderPlayArea
        handArea  = renderHand pos dir hand
        debugArea = renderDebugArea
        leftOpp   = renderHand pos dir hand
        rightOpp  = renderHand pos dir hand
        acrossOpp = renderHand pos dir hand
    in
    Pictures [playArea, handArea, leftOpp, rightOpp, acrossOpp, debugArea]-}

renderCard :: Card -> Picture
renderCard card
    = Pictures
        [ Color magenta $ rectangleSolid (60) (80)
        , Color black $ Scale (0.125) (0.125) $ Text $ show card
        ]

renderHand :: Hand -> Picture
renderHand hand = Translate (-400) (0) $
                    Pictures $ zipWith (\i -> translate (65 *i) (0) ) [1..]
                        $ map renderCard $ Z.toList hand

renderPlay :: Trick -> Picture
renderPlay played = -- "Currently:" ++ F.concat (fmap ((' ':).show ) played)
    Pictures $ F.toList $ S.mapWithIndex (\i -> (translate (80*(fromIntegral i)) (0)). renderCard) played
--------
-- figure out display
--
-----------------------------------------------
renderText :: RenderInfo -> IO ()
renderText (RenderInRound hand played scores) = do
    -- if we should only be rendering the current players hand then do some checking
    -- the following clears the screen
    putStrLn "\ESC[H\ESC[2J"

    putStrLn $ renderTextPlay played
    putStrLn $ renderTextHand hand
    renderTextScores scores

renderText (RenderServerState board (TrickInfo curPlayer played scores _)) = do
    -- if we should only be rendering the current players hand then do some checking
    -- the following clears the screen
    putStrLn "\ESC[H\ESC[2J"

    putStrLn $ renderTextPlay played
    renderTextBoard board curPlayer
    renderTextScores scores

renderText (Passing hand _passDir) = putStrLn $ renderTextHand hand
renderText RenderEmpty = return ()

renderText (BetweenRounds scores) = renderTextScores scores

renderTextScores :: Scores -> IO ()
renderTextScores scores = mapM_ showScore [0..3]
    where showScore  i = putStrLn $ showPlayer i ++ " Score:" ++ show (scores `S.index` i)
          showPlayer i = {- colorize  [44 | i==curPlayer] $ -} "Player " ++ show i

renderTextBoard :: Board -> Int -> IO ()
renderTextBoard board activePlayer = mapM_ printHand [0..3]
    where printHand i = do
                        putStr $ colorize  [44 | i==activePlayer] $ concat ["Player ", show i, " Hand:"]
                        putStr " "
                        putStrLn $ renderTextHand $ board `S.index` i

renderTextHand :: Hand -> String
renderTextHand hand = unwords $ map pretty $ Z.toList hand

renderTextPlay :: Trick -> String
renderTextPlay played = "Currently:" ++ F.concat (fmap ((' ':).pretty ) played)


colorize :: [Int] -> String -> String
colorize options str = "\ESC["
                        ++ intercalate ";" [show i | i <-options]
                        ++ "m" ++ str ++ "\ESC[0m"
