module HeartsGui
    ( guiThread
    )
    where

-- import PlayingCards
import HeartsCommon
-- import HeartsTui (clientTextBased)
import qualified Data.Set as Z
import qualified Data.Sequence as S
-- import qualified Data.Foldable as F
-- import Control.Concurrent.Async
import Control.Concurrent.STM
-- import Control.Concurrent.STM.TMVar

-- may want to consider
-- idSupply Data.Unique.ID or monadSupply or
-- Control.Eff.Fresh or some such
import Data.Maybe (fromJust)
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
                -- _animation  --- collect drag and server generated animations
                -- consider moving inbox and outbox here
                -- may also need a place to register current effect seeking target
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
type ClickProcess  = Pos -> RenderWorld -> IO RenderWorld

data MarkIIRender = MarkIIRender
    -- component entity like system
    { clickables  :: IntMap Clickable
    , targets     :: IntMap Target
    , sprites     :: IntMap Sprite
    , locations   :: IntMap Location
    , gameObjects :: IntMap Card
    -- consider using viewports rather than locations
    , dragged    :: Maybe (Int, Float, Float) -- ID of card currently being draged
    -- movement paths handed to us
    }
    -- possibly name or debug info or logging deserves a place here
    -- will also want a set of logical zones that arrange things inside of them e,g, hand play
type DebugInfo = [String]

data GuiState   = DisplayOnly
                | SelectCardsToPass Hand -- should maybe have this as pile
                | SelectCardToPlay Hand Info (Maybe Card)

guiThread :: TMVar ServerToClient -> TMVar ClientToServer -> IO ()
guiThread inbox outbox
    = do playIO
            window
            white			 -- background color
            100              -- steps per second
            (RenderGame RenderEmpty DisplayOnly [] baseWorld)     -- world
            drawWorld        -- picture to display
            eventHandle      -- event handler
            timeHandle       -- time update
            -- It is plausible that we want to replace commHandle here
            -- with a timeHandle that checks commHandle and also checks
            -- animations and so forth
    where timeHandle = (commHandle inbox outbox)
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
            Just (i,_,_) -> return $ curGame 
                                    { _markIIworld = _mIIworld{dragged = Just (i,mx,my)}
                                    -- , _dbgInfo = (show (mx,my) ):_dbgInfo
                                    }
    EventKey k ks _mod mpos
        -> case (k, ks) of
            (MouseButton _, Down)
                ->
                -- register the click with an object (clickable)
                let (_d, action) = IntMap.foldr cmpDepth (-1, const return)
                                    $ IntMap.intersection (clickables _mIIworld)
                                    $ IntMap.filter (isInRegion mpos)
                                    $ locations _mIIworld
                    cmpDepth z (d, a) = let d' = depth z in if d' > d then (d', clickProcess z) else (d, a)
                -- select the zone with highest depth, and run its on click
                in action mpos curGame
            (MouseButton _, Up)
                -> do
                -- will clean this up eventually
                let shouldGoOff = reverse $ map releaseProcess $ IntMap.elems $ IntMap.intersection (targets _mIIworld)
                                          $ IntMap.filter (isInRegion mpos) $ locations _mIIworld
                -- TODO run through should go off, move dragging effects to targets, i.e. can be released here
                -- if only in generic background target, have sensible move back animation
                blah (map ((=<<) . ($ mpos)) shouldGoOff) $ return curGame
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

commHandle :: TMVar ServerToClient -> TMVar ClientToServer -> Float -> RenderWorld -> IO RenderWorld
commHandle inbox outbox _t world
    = do

    -- check inbox
    message <- atomically $ tryTakeTMVar inbox
    let acknowledge = do atomically $ putTMVar outbox $ CtsAcknowledge
    case message of
        Just StcGameStart  -> acknowledge
        Just StcGameOver   -> acknowledge
        Just (StcRender _) -> acknowledge
        _ -> return ()
    world' <- case (_guiState world) of
        DisplayOnly
            -> return world
        SelectCardToPlay _ _ Nothing
            -> return world
        SelectCardToPlay _ _ (Just c)
            -> do
            atomically $ putTMVar outbox $ CtsMove c
            return $
                world { _guiState = DisplayOnly
                      -- , _dbgInfo = "sent move ":(_dbgInfo world)
                      }
        SelectCardsToPass passSet
            | Z.size passSet == 3
                -> do
                atomically $ putTMVar outbox $ CtsPassSelection passSet
                return $
                    world { _guiState = DisplayOnly
                          -- , _dbgInfo = "passed cards ":(_dbgInfo world)
                          }
            | otherwise -> return world
    return $ maybe world' (handleInMessage_ world') message
    -- post messages if ready
{-handleOutMessage_ :: RenderWorld -> Maybe ServerToClient -> Maybe (ClientToServer, RenderWorld)
handleOutMessage_ world m
    | m == Just StcGameStart  = acknowledge
    | m == Just StcGameOver   = acknowledge
    | m == Just (StcRender _) = acknowledge
    | _guiState world
    | otherwise               = Nothing
    where acknowledge = (Just CtsAcknowledge, world)-}

handleInMessage_ :: RenderWorld -> ServerToClient -> RenderWorld
handleInMessage_ world@(RenderGame _ _mode debug mIIworld) m
    =
    -- need to update mode based on what rinfo we revieve
    case m of
        StcRender rinfo ->
            RenderGame (rinfo) DisplayOnly debug (register rinfo mIIworld)
        StcGetPassSelection _ _ -> world{ _guiState = SelectCardsToPass Z.empty}
        StcGetMove hand info -> world{ _guiState = SelectCardToPlay hand info Nothing}
        _ -> world

register :: RenderInfo -> MarkIIRender -> MarkIIRender
register (Passing hand _passdir) mIIworld =
    S.foldrWithIndex rgstr mIIworld (orderPile hand)
    where rgstr i = registerCard (-350+ 55*(fromIntegral i), -200 )
-- will need to register hand after cards have passed
register (RenderInRound hand trick _scores) _mIIworld =
    flip (S.foldrWithIndex rgstr') trick $
    S.foldrWithIndex rgstr baseWorld (orderPile hand)
    where rgstr  i = registerCard (-350+ 55*(fromIntegral i), -200 )
          rgstr' i = registerCard (-350+ 55*(fromIntegral i), 200  )
register (RenderServerState _ _) w = w
register (BetweenRounds _) _w = baseWorld
register (Canonical _ _ _) w = w
register (RenderEmpty) _w = emptyWorld

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
        { clickables  = IntMap.insert cid c    (clickables  world)
        , targets     = IntMap.insert cid t    (targets     world)
        , sprites     = IntMap.insert cid s    (sprites     world)
        , locations   = IntMap.insert cid l    (locations   world)
        , gameObjects = IntMap.insert cid card (gameObjects world)
        }
    where cid = convertCardID card
          clickCard _crd (mx, my) w =
            return $ w{ _markIIworld =
                        (_markIIworld w){ dragged = Just (cid, mx, my) }
                      -- , _dbgInfo = ((show crd):(_dbgInfo w))
                      }
          dropCard _crd _mpos w =
            return w
            {-return $ w{ _dbgInfo = (("releasing around area of " ++show crd):(_dbgInfo w))
                      }-}
          c = Clickable cid $ clickCard card
          t = Target   $ dropCard card
          s = Sprite   $ renderCard card
          l = Location pos (80,60)

drawWorld :: RenderWorld -> IO Picture
drawWorld (RenderGame _mri _gs debugInfo mIIrender)
    = do
    -- render debugInfo
    let dbg = {-Color rose $-} Translate (-200) (150) $ scale (0.225) (0.225) $ text $ unlines $ take 4 debugInfo
    -- will also want to render in depth order
    return $ Pictures [ dbg
                      {-, render mri-}
                      , renderII mIIrender
                      ]

-- If I moved dragged into an animtions record then I'll either
-- need to make this take the whole world, or, more plausibly,
-- have a renderAnim separate from renderStatic or something
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

emptyWorld :: MarkIIRender
emptyWorld = MarkIIRender IntMap.empty IntMap.empty IntMap.empty IntMap.empty IntMap.empty Nothing

baseWorld :: MarkIIRender
baseWorld =
    registerGeneric 1
    -- The PlayArea
        (Just $ Location (0,0) (400,300))
        (Just $ Sprite (Color (makeColor 0.2 0.2 0.2 0.5) $ rectangleSolid (400) (300)))
        (Just $ Clickable 0 (\_mpos world -> return $ world {-{_dbgInfo = ["Clicked in play area"]}-} ) )
        (Just $
            Target (\_mpos world ->
--                  -- we also need this to take into consideration the gui mode
--                  -- very likely that this is easier to pull out into another function
                    let mIIw = _markIIworld world
                    in
                    return $
                    case (dragged mIIw, _guiState world) of
                        (Nothing, _)       -> world -- {_dbgInfo = "Released in play area":(_dbgInfo world)}
                        (_, DisplayOnly)   -> world -- {_dbgInfo = "Released in play area":(_dbgInfo world)}
                        (Just (i,mx,my), SelectCardsToPass soFar) ->
                            let card = fromJust (IntMap.lookup i (gameObjects mIIw) )
                            in
                            if Z.size soFar < 3 && Z.notMember card soFar
                                -- Card not in set and size of set less than 3
                            then world  { _dbgInfo = "passing this card ":(_dbgInfo world)
                                        , _markIIworld
                                        = mIIw  { dragged = Nothing
                                                , locations = IntMap.insert i (Location (mx,my) (80,60)) (locations mIIw)
                                                }
                                        , _guiState = SelectCardsToPass (Z.insert card soFar)
                                        }
                            else world  { _dbgInfo = "cannot add card to passing set":(_dbgInfo world)}
                        (Just (i,mx,my), SelectCardToPlay hand info Nothing)  ->
                            -- let card = (IntMap.! (gameObjects mIIw) i)
                            let card = fromJust (IntMap.lookup i (gameObjects mIIw) )
                            in
                            if isValidPlay hand info card
                            then
                                world   
                                    { _markIIworld
                                    = mIIw  { dragged = Nothing
                                            , locations = IntMap.insert i (Location (mx,my) (80,60)) (locations mIIw)
                                            }
                                    , _guiState
                                    = SelectCardToPlay hand info $
                                        if isValidPlay hand info card
                                        then Just card
                                        else Nothing
                                    -- , _dbgInfo = "Dropping in play area":(_dbgInfo world)
                                    }
                            else world{_dbgInfo = "Not valid play ":(_dbgInfo world)}
                        (Just _, SelectCardToPlay _ _ (Just _))  ->
                                world{_dbgInfo = "Already selected card to play":(_dbgInfo world)}
                    )
            )
        $ registerGeneric 0
        -- The game window
            (Just $ Location (0,0) (800,600))
            (Just $ Sprite (Color (makeColor 0.2 0.2 0.2 0.5) $ rectangleSolid (400) (300)))
            (Just $ Clickable 0 (\_mpos world -> return $ world {-{_dbgInfo = ["Clicked in window"]}-}) )
            (Just $
                Target (\_mpos world ->
                        return $ case dragged (_markIIworld world) of
                            Nothing       -> world -- {_dbgInfo = "Released in window":(_dbgInfo world)}
                            -- Just (i,mx,my)  ->
                            Just _  ->
                                let mIIw = _markIIworld world
                                in
                                world   
                                    { _markIIworld
                                    = mIIw  { dragged = Nothing
                                            -- , locations = IntMap.insert i (Location (mx,my) (80,60)) (locations mIIw)
                                            }
                                    -- , _dbgInfo = "Released in window, dropping back to init pos":(_dbgInfo world)
                                    }
                    )
            )
    $ emptyWorld

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
{-renderHand :: Hand -> Picture
renderHand hand
    = Translate (-400) (0)
    $ Pictures
        $ zipWith (\i -> translate (65 *i) (0)) [1..]
        $ map renderCard $ Z.toList hand-}

{-renderPlay :: Trick -> Picture
renderPlay played = -- "Currently:" ++ F.concat (fmap ((' ':).show ) played)
    Pictures $ F.toList $ S.mapWithIndex (\i -> (translate (80*(fromIntegral i)) (0)). renderCard) played-}
