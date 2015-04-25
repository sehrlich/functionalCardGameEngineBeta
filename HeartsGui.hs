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
-- decided on Control.Concurrent.Supply for two reasons: first I forgot where this
-- comment was so just found searched for what I wanted again, second this library
-- was written by ekmett and is in LTS stackage
import Control.Concurrent.Supply

import Data.Maybe (fromJust)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap

import Graphics.Gloss
--import Graphics.Gloss.Data.Color (makeColor)
import Graphics.Gloss.Interface.IO.Game --(playIO, Event(..) )

data GuiWorld = GuiWorld
                { _renderWorld  :: RenderWorld
                , _markIIworld  :: MarkIIRender
                , _idSupply     :: Supply
                }
                -- somewhere needs access to mouse position

data RenderWorld = RenderGame
                { _receivedInfo :: RenderInfo
                , _guiState     :: GuiState
                , _dbgInfo      :: DebugInfo
                , _position     :: Int          -- Player position
                -- , _mouseCoords :: Pos
                -- _animation  --- collect drag and server generated animations
                -- consider moving inbox and outbox here
                -- may also need a place to register current effect seeking target
                }

{- Zones are data structures to handle and organize objects on the screen
 - they should support the following operations
 - query if objectid is handled by zone
 - return location/position of valid objectids for rendering
 - accept objectid to be handled
 - delete objectid (i.e. stop representing them)
 -
 - Alternatively zones might want to be a typeclass
 -}
data Zone          = PlayArea Pos | ExactPos Pos
                    -- HandArea Pos | 
                    ---  Zone ManagementStyle Intmap Pos
                    --- type ManagementStyle = GuiWorld -> Pos
extractPos :: Zone -> Int -> Pos
extractPos zone oid = undefined
manageObject :: GuiWorld -> Zone -> Int -> Zone
manageObject world zone oid = undefined
deleteObject :: Zone -> Int -> Zone
deleteObject zone oid = undefined
-- with the above in mind, handarea playarea and exactpos (maybe should be window) should be variables maybe?
-- zones may map ids to positions
-- may want to let zone hold zones
-- and right now zone + location are tangled

-- depth should maybe be a list of ints so that all cards have same first index, and differ in next index
type Depth         = Int -- really more like height in that lower numbers are beneath higher numbers
type Bbox          = (Float, Float)
type Pos           = (Float, Float)
data Sprite        = Sprite Picture -- RenderProcess if we need io to render?
data Location      = Location Zone Bbox -- will want vector stuff to handle/change locations
data Clickable     = Clickable
                    { depth        :: Depth
                    , clickProcess :: ClickProcess
                    -- id of thing
                    }
data Target        = Target
                    { releaseProcess :: ClickProcess
                    -- id of thing
                    }
-- type RenderProcess = IO Picture
type ClickProcess  = Pos -> GuiWorld -> IO GuiWorld

data MarkIIRender = MarkIIRender
    -- component entity like system
    { clickables  :: IntMap Clickable
    , targets     :: IntMap Target
    , sprites     :: IntMap Sprite
    , locations   :: IntMap Location -- should go through zones
    , gameObjects :: IntMap Card
    -- consider using viewports rather than locations
    , dragged    :: Maybe (Int, Float, Float) -- ID of card currently being draged
    -- movement paths handed to us
    -- need Zones
    }
    -- possibly name or debug info or logging deserves a place here
    -- will also want a set of logical zones that arrange things inside of them e,g, hand play
type DebugInfo = [String]

data GuiState   = DisplayOnly
                | SelectCardsToPass Hand -- should maybe have this as pile
                | SelectCardToPlay Hand Info (Maybe Card)

guiThread :: TMVar ServerToClient -> TMVar ClientToServer -> Int -> IO ()
guiThread inbox outbox pos
    = do 
        idSupply <- newSupply
        playIO
            window
            white			 -- background color
            100              -- steps per second
            (initWorld $ emptyWorld pos idSupply) -- world
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


eventHandle :: Event -> GuiWorld -> IO GuiWorld
eventHandle event world
    = let mIIw = _markIIworld world
    in case event of
    EventResize _ws -> return $ world
    EventMotion (mx,my)
        -> case dragged (_markIIworld world) of
            Nothing      -> return world
            Just (i,_,_) -> return $ world
                                    { _markIIworld = mIIw{dragged = Just (i,mx,my)}
                                    -- , _dbgInfo = (show (mx,my) ):_dbgInfo
                                    }
    EventKey k ks _mod mpos
        -> case (k, ks) of
            (MouseButton _, Down)
                ->
                -- register the click with an object (clickable)
                let (_d, action) = IntMap.foldr cmpDepth (-1, const return)
                                    $ IntMap.intersection (clickables mIIw)
                                    $ IntMap.filter (isInRegion mpos)
                                    $ locations mIIw
                    cmpDepth z (d, a) = let d' = depth z in if d' > d then (d', clickProcess z) else (d, a)
                -- select the zone with highest depth, and run its on click
                in action mpos world
            (MouseButton _, Up)
                -> do
                -- will clean this up eventually
                let shouldGoOff = reverse $ map releaseProcess $ IntMap.elems $ IntMap.intersection (targets mIIw)
                                          $ IntMap.filter (isInRegion mpos) $ locations mIIw
                -- TODO run through should go off, move dragging effects to targets, i.e. can be released here
                -- if only in generic background target, have sensible move back animation
                -- give this the proper name rather than blah
                blah (map ((=<<) . ($ mpos)) shouldGoOff) $ return world
                where blah l a = case l of
                                    (x:xs) -> blah xs (x a)
                                    [] -> a
            _   -> return $ world

isInRegion :: (Float, Float) -> Location -> Bool
isInRegion (mx,my) (Location (ExactPos (cx,cy)) (bx,by)) =
    cx - bx/2 <= mx
    && mx <= cx + bx/2
    && cy - by/2 <= my
    && my <= cy + by/2
isInRegion _ _ = False -- hand area play area etc. not checking properly

commHandle :: TMVar ServerToClient -> TMVar ClientToServer -> Float -> GuiWorld -> IO GuiWorld
commHandle inbox outbox _t world
    = do
    -- check inbox
    message <- atomically $ tryTakeTMVar inbox
    let (toSend, renderWorld) = handleOutMessage_ (_renderWorld world) message
        world' = world{_renderWorld = renderWorld}
    case toSend of
        Nothing -> return ()
        Just outMessage -> do atomically $ putTMVar outbox $ outMessage
    return $ maybe world' (handleInMessage_ world') message

handleOutMessage_ :: RenderWorld -> Maybe ServerToClient -> (Maybe ClientToServer, RenderWorld)
handleOutMessage_ world m =
    case m of
    Just (StcGameStart i) ->
        (Just CtsAcknowledge, world{_position = i})
    Just StcGameOver      -> acknowledge
    Just (StcRender _)    -> acknowledge
    _ ->
        case (_guiState world) of
        DisplayOnly -> (Nothing, world)
        SelectCardToPlay _ _ Nothing -> (Nothing, world)
        SelectCardToPlay _ _ (Just c) ->
            ( Just $ CtsMove c
            , world { _guiState = DisplayOnly
                    -- , _dbgInfo = "sent move ":(_dbgInfo world)
                    }
            )
        SelectCardsToPass passSet
            | Z.size passSet == 3 ->
                ( Just $ CtsPassSelection passSet
                , world { _guiState = DisplayOnly
                        -- , _dbgInfo = "passed cards ":(_dbgInfo world)
                        }
                )
            | otherwise -> (Nothing, world)
    where acknowledge = (Just CtsAcknowledge, world)

-- TODO fix this, maybe with lenses, maybe just with a principled reorganization of guiworld and constituents
handleInMessage_ :: GuiWorld -> ServerToClient -> GuiWorld
handleInMessage_ world m
    =
    case m of
        StcRender rinfo -> register rinfo world
        StcGetPassSelection _ _ -> world{ _renderWorld = (_renderWorld world){_guiState = SelectCardsToPass Z.empty} }
        StcGetMove hand info -> world{ _renderWorld = (_renderWorld world){_guiState = SelectCardToPlay hand info Nothing} }
        _ -> world

{- Register nonsense takes guiworld to guiworld
 -}
register :: RenderInfo -> GuiWorld -> GuiWorld
register rinfo@(Passing hand _passdir) world =
    S.foldrWithIndex rgstr (world{ _renderWorld = (_renderWorld world){_receivedInfo = rinfo}}) (orderPile hand)
    where rgstr i = registerCard $ ExactPos (-350+ 55*(fromIntegral i), -200 ) -- Switch to HandArea
-- will need to register hand after cards have passed
register rinfo@(RenderInRound hand trick _scores) w =
    flip (S.foldrWithIndex rgstr') trick $ S.foldrWithIndex rgstr world (orderPile hand)
    where rgstr  i = registerCard $ ExactPos (-350+ 55*(fromIntegral i), -200 ) -- Switch to HandArea
          rgstr' i = registerCard $ ExactPos (-350+ 55*(fromIntegral i), 200  ) -- Switch to PlayArea
          world = w { _renderWorld = (_renderWorld w){_receivedInfo = rinfo}
                    -- , _markIIworld = baseWorld
                    }
register (RenderServerState _ _) w = w
register (BetweenRounds _) w = w{ _markIIworld = emptyRender}
register (Canonical _ _ _) w = w
register (RenderEmpty) w = w{ _markIIworld = emptyRender}

registerGeneric :: Maybe Location -> Maybe Sprite -> Maybe Clickable -> Maybe Target -> GuiWorld -> GuiWorld
registerGeneric mLoc mSpr mZon mTar world
    = 
    let mIIw = _markIIworld world
        (idNo, newSup) = freshId $ _idSupply world
    in     
    world
    { _markIIworld = mIIw
        { clickables = IntMap.alter (const mZon) idNo (clickables mIIw)
        , targets    = IntMap.alter (const mTar) idNo (targets    mIIw)
        , sprites    = IntMap.alter (const mSpr) idNo (sprites    mIIw)
        , locations  = IntMap.alter (const mLoc) idNo (locations  mIIw)
        }
    , _idSupply = newSup
    }

registerCard :: Zone -> Card -> GuiWorld -> GuiWorld
registerCard pos card world
    = 
    let mIIw = _markIIworld world
        (cid, newSup) = freshId $ _idSupply world
        c = Clickable cid $ clickCard cid card
        t = Target   $ dropCard card
        s = Sprite   $ renderCard card
        l = Location pos (80,60)
    in
    world
        { _markIIworld = mIIw
            { clickables  = IntMap.insert cid c    (clickables  mIIw)
            , targets     = IntMap.insert cid t    (targets     mIIw)
            , sprites     = IntMap.insert cid s    (sprites     mIIw)
            , locations   = IntMap.insert cid l    (locations   mIIw)
            , gameObjects = IntMap.insert cid card (gameObjects mIIw)
            }
        , _idSupply = newSup
        }
    where clickCard cid _crd (mx, my) w =
            return $ w{ _markIIworld =
                        (_markIIworld w){ dragged = Just (cid, mx, my) }
                      -- , _dbgInfo = ((show crd):(_dbgInfo w))
                      }
          dropCard _crd _mpos w =
            return w
            {-return $ w{ _dbgInfo = (("releasing around area of " ++show crd):(_dbgInfo w))
                      }-}

{-registerButton :: Location -> Sprite -> ClickProcess -> GuiWorld -> GuiWorld-}
{--- registerButton = undefined -- needs to actually register button-}
{-registerButton loc img action world-}
    {-= world-}
        {-{ targets   = IntMap.insert bid (Target action) (targets   world)-}
        {-, sprites   = IntMap.insert bid img             (sprites   world)-}
        {-, locations = IntMap.insert bid loc             (locations world)-}
        {-}-}
    {-where bid = 37-- generate button id-}

drawWorld :: GuiWorld -> IO Picture
drawWorld world
    = do
    -- render debugInfo
    let debugInfo = _dbgInfo $ _renderWorld world
        mIIrender = _markIIworld world
        dbg = {-Color rose $-} Translate (-200) (150) $ scale (0.225) (0.225) $ text $ unlines $ take 4 debugInfo
    -- will also want to render in depth order
    return $ Pictures [ dbg
                      {-, render mri-}
                      , render mIIrender
                      ]

-- If I moved dragged into an animtions record then I'll either
-- need to make this take the whole world, or, more plausibly,
-- have a renderAnim separate from renderStatic or something
-- also consider where textual things are
render :: MarkIIRender -> Picture
render mIIw
    = Pictures [renderable, dragging]
    -- the proper alternative here is to iterate over zones rendering everything inside them
    where renderable =
            Pictures $ map renderSprite
            $ IntMap.elems
            $ IntMap.intersectionWith (,) (sprites mIIw) (locations mIIw)
          dragging =
            case dragged mIIw of
                Just (i,px,py) -> renderSprite ((IntMap.!) (sprites mIIw) i, (Location (ExactPos (px,py)) (80,60)))
                Nothing -> Blank

-- Will need a way to turn a location into coordinates
-- will be made obsolete when zones come online
renderSprite :: (Sprite, Location) -> Picture
renderSprite ((Sprite pic), (Location (ExactPos (px,py)) _bbox))
    = Translate px py $ pic
renderSprite ((Sprite pic), (Location (PlayArea (px,py)) _bbox))
    = Translate px py $ pic
{-renderSprite ((Sprite pic), (Location (HandArea (px,py)) _bbox))
    = Translate px py $ pic-}
-- not correct way to render something in a zone

{-renderZone :: Zone -> Picture-}
{-renderZone = undefined-}

renderCard :: Card -> Picture
renderCard card
    = Pictures
        [ Color magenta $ rectangleSolid (60) (80)
        , Color (greyN 0.575) $ circleSolid 20
        , Color black $ Translate (-10) (-5) $ Scale (0.125) (0.125) $ Text $ show card
        , Color black $ rectangleWire (60) (80)
        ]

emptyRender :: MarkIIRender
emptyRender = MarkIIRender IntMap.empty IntMap.empty IntMap.empty IntMap.empty IntMap.empty Nothing

initWorld :: GuiWorld -> GuiWorld
initWorld =
    registerGeneric
    -- The PlayArea
        (Just $ Location (ExactPos (0,0)) (400,300))
        (Just $ Sprite (Color (makeColor 0.2 0.2 0.2 0.5) $ rectangleSolid (400) (300)))
        (Just $ Clickable 0 (\_mpos world -> return $ world {-{_dbgInfo = ["Clicked in play area"]}-} ) )
        (Just $
            Target (\_mpos world ->
--                  -- we also need this to take into consideration the gui mode
--                  -- very likely that this is easier to pull out into another function
                let mIIw = _markIIworld world
                    renW = _renderWorld world
                in
                return $
                case (dragged mIIw, _guiState renW) of
                    (Nothing, _)       -> world -- {_dbgInfo = "Released in play area":(_dbgInfo world)}
                    (_, DisplayOnly)   -> world -- {_dbgInfo = "Released in play area":(_dbgInfo world)}
                    (Just (i,mx,my), SelectCardsToPass soFar) ->
                        let card = fromJust (IntMap.lookup i (gameObjects mIIw) )
                        in
                        if Z.size soFar < 3 && Z.notMember card soFar
                            -- Card not in set and size of set less than 3
                        then world  { _renderWorld 
                                    = renW  { _dbgInfo = "passing this card ":(_dbgInfo renW)
                                            , _guiState = SelectCardsToPass (Z.insert card soFar)
                                            }
                                    , _markIIworld
                                    = mIIw  { dragged = Nothing
                                            , locations = IntMap.insert i (Location (PlayArea (mx,my)) (80,60)) (locations mIIw)
                                            }
                                    }
                        else world  { _renderWorld = renW{_dbgInfo = "cannot add card to passing set":(_dbgInfo renW)}}
                    (Just (i,mx,my), SelectCardToPlay hand info Nothing)  ->
                        -- let card = (IntMap.! (gameObjects mIIw) i)
                        let card = fromJust (IntMap.lookup i (gameObjects mIIw) )
                        in
                        if isValidPlay hand info card
                        then world
                                { _markIIworld
                                = mIIw  { dragged = Nothing
                                        , locations = IntMap.insert i (Location (ExactPos (mx,my)) (80,60)) (locations mIIw)
                                        -- may need to be altering old zone
                                        }
                                , _renderWorld
                                = renW  { _guiState = SelectCardToPlay hand info $
                                                if isValidPlay hand info card
                                                then Just card
                                                else Nothing
                                        }
                                -- , _dbgInfo = "Dropping in play area":(_dbgInfo world)
                                }
                        else world{ _renderWorld = renW{_dbgInfo = "Not valid play ":(_dbgInfo renW)}}
                    (Just _, SelectCardToPlay _ _ (Just _))  ->
                            world{ _renderWorld = renW{_dbgInfo = "Already selected card to play":(_dbgInfo renW)}}
                )
            )
        . registerGeneric
        -- The game window
            (Just $ Location (ExactPos (0,0)) (800,600))
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
    

emptyWorld :: Int -> Supply -> GuiWorld
emptyWorld pos sup = (GuiWorld (RenderGame RenderEmpty DisplayOnly [] pos) emptyRender sup)  -- world
