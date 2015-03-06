module HeartsGui
    ( guiThread
    )
    where

-- import PlayingCards
import HeartsCommon
import HeartsTui (clientTextBased)
import qualified Data.Set as Z
import qualified Data.Sequence as S
import qualified Data.Foldable as F
import Control.Concurrent.Async
import Control.Concurrent.STM
-- import Control.Concurrent.STM.TMVar

-- may want to consider
-- idSupply Data.Unique.ID or monadSupply or
-- Control.Eff.Fresh or some such
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game (playIO)

data RenderMode = RenderGame RenderInfo GuiState DebugInfo MarkIIRender -- (Picture,pos) what player is currently moving

type Depth = Int
type Bbox  = (Int,Int)
type Pos   = (Int,Int)
data Sprite   = Sprite Bbox Pos RenderProcess
data Zone     = Zone Bbox Depth ClickProcess
type RenderProcess = IO Picture
type ClickProcess = IO World

data MarkIIRender = MarkIIRender
    { zones     :: IntMap Zone
    , sprites   :: IntMap Sprite
    }
emptyWorld :: MarkIIRender
emptyWorld = MarkIIRender IntMap.empty IntMap.empty

type DebugInfo = [String]

data GuiState   = DisplayOnly
                {-| SelectCardsToPass-}
                {-| SelectCardToPlay-}
guiThread :: TMVar ServerToClient -> TMVar ClientToServer -> IO ()
guiThread inbox outbox
    = do playIO
            window
            white			 -- background color
            100              -- steps per second
            (RenderGame RenderEmpty DisplayOnly [] emptyWorld)     -- world
            drawWorld        -- picture to display
            eventHandle      -- event handler
            commHandle       -- time update
    where eventHandle event (RenderGame rinfo _gs dbgInfo _mIIworld)
            = return $ (RenderGame rinfo _gs ((show event):dbgInfo) emptyWorld)
            -- this will need to check zones and see if a click just made needs to
            -- do one of their things
          commHandle _t world
            = do
            -- check inbox
            message <- atomically $ tryTakeTMVar inbox
            -- register sprites and zones
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
        StcRender rinfo -> RenderGame (rinfo) DisplayOnly [] emptyWorld
        _ -> world

drawWorld :: RenderMode -> IO Picture
drawWorld (RenderGame mri _gs debugInfo _mIIrender)
    = do
    -- render debugInfo
    let dbg = Color rose $ Translate (0) (50) $ scale (0.125) (0.125) $ text $ unlines $ take 4 debugInfo
    -- will want to use viewports for pictures
    return $ Pictures [dbg, render mri]

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
        , Color (greyN 0.875) $ circleSolid 20
        , Color black $ Translate (-10) (-5) $ Scale (0.125) (0.125) $ Text $ show card
        ]

renderHand :: Hand -> Picture
renderHand hand
    = Translate (-400) (0)
    $ Pictures
        $ zipWith (\i -> translate (65 *i) (0)) [1..]
        $ map renderCard $ Z.toList hand

renderPlay :: Trick -> Picture
renderPlay played = -- "Currently:" ++ F.concat (fmap ((' ':).show ) played)
    Pictures $ F.toList $ S.mapWithIndex (\i -> (translate (80*(fromIntegral i)) (0)). renderCard) played

convertCardID :: Card -> Int
convertCardID (Card s r) =
    let s' =
            case s of
                Clubs    -> 0
                Diamonds -> 0
                Hearts   -> 0
                Spades   -> 0
    in
    50*s'+r
