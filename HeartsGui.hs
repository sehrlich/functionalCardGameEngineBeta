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
--import Graphics.Gloss.Data.Color (makeColor)
import Graphics.Gloss.Interface.IO.Game --(playIO, Event(..) )

data RenderWorld = RenderGame
                { _receivedInfo :: RenderInfo
                , _guiState     :: GuiState
                , _dbgInfo      :: DebugInfo
                , _markIIworld  :: MarkIIRender
                -- (Picture,pos) what player is currently moving
                }

type Depth         = Int -- really more like height in that lower numbers are beneath higher numbers
type Bbox          = (Float, Float)
type Pos           = (Float, Float)
data Sprite        = Sprite Location Picture -- RenderProcess if we need io to render?
data Location      = Location Pos Bbox
data Zone          = Zone
                    { loc          :: Location
                    , depth        :: Depth
                    , clickProcess :: ClickProcess
                    }
-- type RenderProcess = IO Picture
type ClickProcess  = RenderWorld -> IO RenderWorld

data MarkIIRender = MarkIIRender
    { _zones     :: IntMap Zone
    , _sprites   :: IntMap Sprite
    }
emptyWorld :: MarkIIRender
emptyWorld = MarkIIRender
            (IntMap.singleton 1
                (Zone baseLoc 0 (\world -> return $ world{_dbgInfo = ["Clicked in window"]})
                )
            )
            (IntMap.singleton 1
                (Sprite baseLoc (Color (makeColor 0.2 0.2 0.2 0.5) $ rectangleSolid (400) (300))
                )
            )
baseLoc :: Location
baseLoc = Location (0,0) (400,300) 

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
    where commHandle _t world
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
                   (0, 0)) 	-- window position


eventHandle :: Event -> RenderWorld -> IO RenderWorld
eventHandle event curGame@(RenderGame _rinfo _gs _dbgInfo _mIIworld)
    = case event of
    EventResize _ws -> return $ curGame
    EventMotion pos -> return $ curGame{_dbgInfo = (show pos):_dbgInfo}
    EventKey k ks _mod _mpos
        -> case (k, ks) of
            (MouseButton _, Down)
                ->
                -- register the click with an object (zone)
                let (_d, action) = IntMap.foldr cmpDepth (-1, return) $ IntMap.filter mouseIn $ _zones _mIIworld
                    cmpDepth z (d, a) = let d' = depth z in if d' > d then (d', clickProcess z) else (d, a)
                -- select the zone with highest depth, and run its on click
                in action curGame
            _   -> return $ curGame{_dbgInfo = (show k):_dbgInfo}
        where mouseIn z = isInRegion _mpos (loc z) -- test if mousepos is in zone

-- this will need to check zones and see if a click just made needs to
-- do one of their things

isInRegion :: (Float, Float) -> Location -> Bool
isInRegion (mx,my) (Location (cx,cy) (bx,by)) = 
    cx - bx/2 <= mx 
    && mx <= cx + bx/2 
    && cy - by/2 <= my 
    && my <= cy + by/2
{-handleMessage :: ServerToClient -> RenderMode
handleMessage m = undefined-}

handleMessage_ :: TMVar ClientToServer -> RenderWorld -> ServerToClient -> IO RenderWorld
handleMessage_ outbox world@(RenderGame _ _mode debug mIIworld) m
    = do
    {-response <- clientTextBased m-}
    {-atomically $ putTMVar outbox response-}
    _ <- async $ clientTextBased m >>= atomically . putTMVar outbox
    return $ case m of
        StcRender rinfo -> do
            RenderGame (rinfo) DisplayOnly debug (register rinfo mIIworld)
        _ -> world

register :: RenderInfo -> MarkIIRender -> MarkIIRender
register _rinfo mIIworld = mIIworld

drawWorld :: RenderWorld -> IO Picture
drawWorld (RenderGame mri _gs debugInfo mIIrender)
    = do
    -- render debugInfo
    let dbg = Color rose $ Translate (-200) (50) $ scale (0.125) (0.125) $ text $ unlines $ take 4 debugInfo
    -- will want to use viewports for pictures
    -- will also want to render in depth order
    return $ Pictures [dbg, render mri, renderII mIIrender]

renderII :: MarkIIRender -> Picture
renderII (MarkIIRender _zones sprites)
    = Pictures $ map renderSprite $ IntMap.elems sprites


render :: RenderInfo -> Picture
render RenderEmpty = Blank
render (Canonical _mode _objlist _strlist) = Blank
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

renderSprite :: Sprite -> Picture
renderSprite (Sprite (Location (px,py) _bbox) pic)
    = Translate px py $ pic

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

_convertCardID :: Card -> Int
_convertCardID (Card s r) =
    let s' = case s of
                Clubs    -> 1
                Diamonds -> 2
                Hearts   -> 3
                Spades   -> 4
    in
    50*s'+r
