module HeartsGui
    ( guiThread
    )
    where

-- import PlayingCards
import HeartsCommon
import HeartsTui (clientTextBased)
--import qualified Data.Set as Z
import qualified Data.Sequence as S
-- import qualified Data.Foldable as F
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
data Sprite        = Sprite Picture -- RenderProcess if we need io to render?
data Location      = Location Pos Bbox -- will want vector stuff to handle/change locations
data Clickable     = Clickable
                    { depth        :: Depth
                    , clickProcess :: ClickProcess
                    }
data Target        = Target
                    { releaseProcess :: ClickProcess
                    }
-- type RenderProcess = IO Picture
type ClickProcess  = RenderWorld -> IO RenderWorld

data MarkIIRender = MarkIIRender
    -- component entity like system
    { clickables :: IntMap Clickable
    , targets    :: IntMap Target
    , sprites    :: IntMap Sprite
    , locations  :: IntMap Location
    -- consider using viewports rather than locations
    , dragged    :: Maybe (Int, Float, Float) -- ID of card currently being draged
    -- movement paths handed to us
    }
    -- possibly name or debug info or logging deserves a place here
    -- will also want a set of logical zones that arrange things inside of them e,g, hand play
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
            (RenderGame RenderEmpty DisplayOnly [] baseWorld)     -- world
            drawWorld        -- picture to display
            eventHandle      -- event handler
            commHandle       -- time update
    where commHandle _t world
            = do
            -- check inbox
            message <- atomically $ tryTakeTMVar inbox
            -- register things that need it
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
    EventMotion (mx,my)
        -> case dragged _mIIworld of
            Nothing      -> return curGame
            Just (i,_,_) -> return $ curGame{ _dbgInfo = (show (mx,my) ):_dbgInfo
                                            , _markIIworld = _mIIworld{dragged = Just (i,mx,my)}
                                            }
    EventKey k ks _mod mpos
        -> case (k, ks) of
            (MouseButton _, Down)
                ->
                -- register the click with an object (zone)
                let (_d, action) = IntMap.foldr cmpDepth (-1, return)
                                    $ IntMap.intersection (clickables _mIIworld)
                                    $ IntMap.filter (isInRegion mpos)
                                    $ locations _mIIworld
                    cmpDepth z (d, a) = let d' = depth z in if d' > d then (d', clickProcess z) else (d, a)
                -- select the zone with highest depth, and run its on click
                in action curGame
            (MouseButton _, Up)
                -> do
                -- will clean this up eventually
                let shouldGoOff = reverse $ map releaseProcess $ IntMap.elems $ IntMap.intersection (targets _mIIworld)
                                            $ IntMap.filter (isInRegion mpos) $ locations _mIIworld
                -- TODO run through should go off, move dragging effects to targets, i.e. can be released here
                -- if only in generic background target, have sensible move back animation
                curGame' <- blah (map (=<<) shouldGoOff) $ return curGame
                return curGame'
                where blah l a = case l of
                                    (x:xs) -> blah xs (x a)
                                    [] -> a
            _   -> return $ curGame

isInRegion :: (Float, Float) -> Location -> Bool
isInRegion (mx,my) (Location (cx,cy) (bx,by)) =
    cx - bx/2 <= mx
    && mx <= cx + bx/2
    && cy - by/2 <= my
    && my <= cy + by/2

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
register (Passing hand _passdir) mIIworld =
    S.foldrWithIndex rgstr mIIworld (orderPile hand)
    where rgstr i = registerCard (-350+ 55*(fromIntegral i), -200 )
register _rinfo mIIworld = mIIworld

registerGeneric :: Int -> Maybe Location -> Maybe Sprite -> Maybe Clickable -> Maybe Target -> MarkIIRender -> MarkIIRender
registerGeneric idNo mLoc mSpr mZon mTar world
    = world
        { clickables = IntMap.alter (const mZon) idNo (clickables world)
        , targets    = IntMap.alter (const mTar) idNo (targets    world)
        , sprites    = IntMap.alter (const mSpr) idNo (sprites    world)
        , locations  = IntMap.alter (const mLoc) idNo (locations  world)
        }

registerCard :: Pos -> Card -> MarkIIRender -> MarkIIRender
registerCard pos card world
    = world
        { clickables = IntMap.insert cid z (clickables world)
        , sprites    = IntMap.insert cid s (sprites    world)
        , locations  = IntMap.insert cid l (locations  world)
        }
    where cid = convertCardID card
          clickCard c w =
            return $ w{ _dbgInfo = ((show c):(_dbgInfo w))
                      , _markIIworld =
                        (_markIIworld w){ dragged = Just (cid, 0,0) }
                      }
          z = Clickable cid $ clickCard card
          s = Sprite   $ renderCard card
          l = Location pos (80,60)

drawWorld :: RenderWorld -> IO Picture
drawWorld (RenderGame _mri _gs debugInfo mIIrender)
    = do
    -- render debugInfo
    let dbg = Color rose $ Translate (-200) (50) $ scale (0.125) (0.125) $ text $ unlines $ take 4 debugInfo
    -- will also want to render in depth order
    return $ Pictures [ dbg
                      {-, render mri-}
                      , renderII mIIrender
                      ]

renderII :: MarkIIRender -> Picture
renderII mIIw
    = Pictures [renderable, dragging]
    where renderable =
            Pictures $ map renderSprite
            $ IntMap.elems
            $ IntMap.intersectionWith (,) (sprites mIIw) (locations mIIw)
          dragging =
            case dragged mIIw of
                Just (i,px,py) -> renderSprite ((IntMap.!) (sprites mIIw) i, (Location (px,py) (80,60)))
                Nothing -> Blank

renderSprite :: (Sprite, Location) -> Picture
renderSprite ((Sprite pic), (Location (px,py) _bbox))
    = Translate px py $ pic

renderCard :: Card -> Picture
renderCard card
    = Pictures
        [ Color magenta $ rectangleSolid (60) (80)
        , Color (greyN 0.875) $ circleSolid 20
        , Color black $ Translate (-10) (-5) $ Scale (0.125) (0.125) $ Text $ show card
        , Color black $ rectangleWire (60) (80)
        ]

convertCardID :: Card -> Int
convertCardID (Card s r) =
    let s' = case s of
                Clubs    -> 1
                Diamonds -> 2
                Hearts   -> 3
                Spades   -> 4
    in
    50*s'+r

-- render :: RenderInfo -> Picture
-- render RenderEmpty = Blank
-- render (Canonical _mode _objlist _strlist) = Blank
-- render (RenderInRound hand played _scores)
--     = Pictures
--         [ Translate (0) (-200) $ renderHand hand
--         , Translate (0) (-50) $ renderPlay played
--         ]
--         -- use viewports for pictures
-- render (RenderServerState _ _) = Circle 2
-- render (Passing hand dir)
--     = Pictures
--         [ Text $ show dir
--         , Translate (0) (-200) $ renderHand hand
--         ]
-- render (BetweenRounds _) = ThickCircle 50 8
    {-let playArea  = renderPlayArea
        handArea  = renderHand pos dir hand
        debugArea = renderDebugArea
        leftOpp   = renderHand pos dir hand
        rightOpp  = renderHand pos dir hand
        acrossOpp = renderHand pos dir hand
    in
    Pictures [playArea, handArea, leftOpp, rightOpp, acrossOpp, debugArea]-}

emptyWorld :: MarkIIRender
emptyWorld = MarkIIRender IntMap.empty IntMap.empty IntMap.empty IntMap.empty Nothing

baseWorld :: MarkIIRender
baseWorld =
    registerGeneric 1
        (Just $ Location (0,0) (400,300))
        (Just $ Sprite (Color (makeColor 0.2 0.2 0.2 0.5) $ rectangleSolid (400) (300)))
        (Just $ Clickable 0 (\world -> return $ world{_dbgInfo = ["Clicked in play area"]}) )
        (Just $
            Target (\world ->
--                    return $ world{_dbgInfo = "Released in play area":(_dbgInfo world)}
                    return $ case dragged (_markIIworld world) of
                        Nothing       -> world{_dbgInfo = "Released in play area":(_dbgInfo world)}
                        Just (i,mx,my)  ->
                            let mIIw = _markIIworld world
                            in
                            -- try to interpret the card as a move based on latest response
                            -- if can, wrap it and send it back
                            world   { _dbgInfo = "Dropping in play area":(_dbgInfo world)
                                    , _markIIworld
                                    = mIIw  { dragged = Nothing
                                            , locations = IntMap.insert i (Location (mx,my) (80,60)) (locations mIIw)
                                            }
                                    }
                   )
        )
    $ registerGeneric 0
        (Just $ Location (0,0) (800,600))
        (Just $ Sprite (Color (makeColor 0.2 0.2 0.2 0.5) $ rectangleSolid (400) (300)))
        (Just $ Clickable 0 (\world -> return $ world{_dbgInfo = ["Clicked in window"]}) )
        (Just $
            Target (\world ->
                    return $ case dragged (_markIIworld world) of
                        Nothing       -> world{_dbgInfo = "Released in window":(_dbgInfo world)}
                        -- Just (i,mx,my)  ->
                        Just _  ->
                            let mIIw = _markIIworld world
                            in
                            world   { _dbgInfo = "Released in window, dropping back to init pos":(_dbgInfo world)
                                    , _markIIworld
                                    = mIIw  { dragged = Nothing
                                            -- , locations = IntMap.insert i (Location (mx,my) (80,60)) (locations mIIw)
                                            }
                                    }
                    )
        )
    $ emptyWorld

{-renderHand :: Hand -> Picture
renderHand hand
    = Translate (-400) (0)
    $ Pictures
        $ zipWith (\i -> translate (65 *i) (0)) [1..]
        $ map renderCard $ Z.toList hand-}

{-renderPlay :: Trick -> Picture
renderPlay played = -- "Currently:" ++ F.concat (fmap ((' ':).show ) played)
    Pictures $ F.toList $ S.mapWithIndex (\i -> (translate (80*(fromIntegral i)) (0)). renderCard) played-}
