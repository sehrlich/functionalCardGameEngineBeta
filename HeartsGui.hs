module HeartsGui
    ( guiThread
    )
    where

import HeartsCommon
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

-- Overview:
-- Gui Stuff
-- -- Thread
-- Play thread is responsible for turning a list of game objects into a coherent
-- picture and letting you interact with it
-- Spawns child thread for communication with server
--
-- -- Communication
-- Protocol is defined where?
-- Render Messages: here is list of game objects
-- Requests: Give back particular input
--
-- -- Graphics + Components
-- Want zones to organize rendering of objects
--   Multiple types of zones for varying displays:
--   Are zones inherently ordered?
--   Zone interaction defaults?  -- Buttons?
-- Text Boxes?
--
-- -- World Type
-- We have this component-entity system
-- List of objects/zones
-- Need some Gui Info such as cursor position
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
                , _mouseCoords  :: Pos
                -- _animation  --- collect drag and server generated animations
                -- consider moving inbox and outbox here
                -- may also need a place to register current effect seeking target
                }

_addDebug :: String -> GuiWorld -> GuiWorld
_addDebug s w = w{ _renderWorld = (_renderWorld w){_dbgInfo = s:(_dbgInfo $ _renderWorld $ w)}}

{- Zones are data structures to handle and organize objects on the screen
 - they should support the following operations
 - query if objectid is handled by zone
 - return location/position of valid objectids for rendering
 - accept objectid to be handled
 - delete objectid (i.e. stop representing them)
 -
 - Alternatively zones might want to be a typeclass
 -}
class HZone z where
    extractPos :: z -> i -> Pos
    insert :: a -> z -> z -- maybe a should also have i in signature?
    remove :: i -> z -> z

data Zone          = PlayArea Pos | ExactPos Pos
                    -- HandArea Pos |
                    ---  Zone ManagementStyle Intmap Pos
instance HZone Zone where
    extractPos z _i = case z of
                      (PlayArea p) -> p
                      (ExactPos p) -> p
    insert _a z = z
    remove _i z = z

{-type ManagementStyle = GuiWorld -> Pos-}
{-insertAtMousePos :: ManagementStyle
insertAtMousePos gw = _mouseCoords $ _renderWorld gw
insertNextPos :: ManagementStyle
insertNextPos = undefined-}

-- depth should maybe be a list of ints so that all cards have same first index, and differ in next index
-- or oranized by Zone or something
type Depth         = Int -- really more like height in that lower numbers are beneath higher numbers
type Bbox          = (Float, Float)
type Pos           = (Float, Float)
data Sprite        = Sprite Picture -- RenderProcess if we need io to render?
data Location      = Location Zone Bbox -- will want vector stuff to handle/change locations
data Clickable     = Clickable
                    { depth        :: Depth
                    , clickProcess :: ClickProcess
                    -- , objId        :: Int
                    -- id of thing
                    }
data Target        = Target
                    { releaseProcess :: ClickProcess
                    -- id of thing
                    }
-- type RenderProcess = IO Picture
type ClickProcess  = GuiWorld -> IO GuiWorld

data MarkIIRender = MarkIIRender
    -- component entity like system
    { clickables  :: IntMap Clickable
    , targets     :: IntMap Target
    , sprites     :: IntMap Sprite
    , locations   :: IntMap Location -- should go through zones
    , gameObjects :: IntMap HeartsCommon.Card
    -- consider using viewports rather than locations
    , dragged    :: Maybe Int -- ID of card currently being draged
    -- movement paths handed to us
    -- need Zones
    }
    -- possibly name or debug info or logging deserves a place here
    -- will also want a set of logical zones that arrange things inside of them e,g, hand play
type DebugInfo = [String]

-- This should get changed
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
    where timeHandle = (commHandle inbox outbox)
          window = (InWindow
                   "Gloss" 	    -- window title
                                -- title fixed for xmonad
                   (800, 600)   -- window size
                   (0, 0)) 	-- window position

-- Gui Event loop
eventHandle :: Event -> GuiWorld -> IO GuiWorld
eventHandle event world
    = let mIIw = _markIIworld world
          renW = _renderWorld world
    in case event of
    EventResize _ws -> return $ world
    EventMotion (mx,my)
        -> let world' = world { _renderWorld = renW{_mouseCoords = (mx, my)} }
            in
            case dragged (_markIIworld world) of
            Nothing -> return world'
            Just i  -> return $ world'
                                    { _markIIworld = mIIw{dragged = Just i}
                                    -- , _dbgInfo = (show (mx,my) ):_dbgInfo
                                    }
    EventKey k ks _mod mpos
        -> case (k, ks) of
            (MouseButton _, Down)
                ->
                -- register the click with an object (clickable)
                let (_d, action) = IntMap.foldr cmpDepth (-1, return)
                                    $ IntMap.intersection (clickables mIIw)
                                    $ IntMap.filter (isInRegion mpos)
                                    $ locations mIIw
                    cmpDepth z (d, a) = let d' = depth z in if d' > d then (d', clickProcess z) else (d, a)
                -- select the object with highest depth, and run its on click
                in action world 
            (MouseButton _, Up)
                -> do
                -- will clean this up eventually
                let shouldGoOff = reverse
                        $ map releaseProcess
                        $ IntMap.elems
                        $ IntMap.intersection (targets mIIw)
                        $ IntMap.filter (isInRegion mpos)
                        $ locations mIIw
                -- TODO run through should go off, move dragging effects to targets, i.e. can be released here
                -- if only in generic background target, have sensible move back animation
                -- give this the proper name rather than blah
                blah (map ((=<<) ) shouldGoOff) $ return world
                where blah l a = case l of
                                    (x:xs) -> blah xs (x a)
                                    [] -> a
            _   -> return $ world

-- Generic Gui -- Gui Elements -- collision detection
isInRegion :: (Float, Float) -> Location -> Bool
isInRegion (mx,my) (Location (ExactPos (cx,cy)) (bx,by)) =
    cx - bx/2 <= mx
    && mx <= cx + bx/2
    && cy - by/2 <= my
    && my <= cy + by/2
isInRegion _ _ = False -- for pattern matching purposes

commHandle :: TMVar ServerToClient -> TMVar ClientToServer -> Float -> GuiWorld -> IO GuiWorld
commHandle inbox outbox _t world
    = do
    -- check inbox
    message <- atomically $ tryTakeTMVar inbox
    let (toSend, world') = handleOutMessage_ world message
    case toSend of
        Nothing -> return ()
        Just outMessage -> do atomically $ putTMVar outbox $ outMessage
    return $ maybe world' (handleInMessage_ world') message

handleOutMessage_ :: GuiWorld -> Maybe ServerToClient -> (Maybe ClientToServer, GuiWorld)
handleOutMessage_ world m =
    case m of
    Just (StcGameStart i) -> (Just CtsAcknowledge, world{_renderWorld = (_renderWorld world){_position = i}})
    Just StcGameOver      -> acknowledge
    Just (StcRender _)    -> acknowledge
    _ ->
        case (_guiState $ _renderWorld world) of
        DisplayOnly -> (Nothing, world)
        SelectCardToPlay _ _ Nothing -> (Nothing, world)
        SelectCardToPlay _ _ (Just c) ->
            ( Just $ CtsMove c
            , unregisterId (_id c) $
                world 
                    { _renderWorld = (_renderWorld world){_guiState = DisplayOnly}
                    -- , _dbgInfo = "sent move ":(_dbgInfo world)
                    }
            )
        SelectCardsToPass passSet
            -- AS A TEMPORARY HACK we are unregistering upon sending this information, as it is the last place we know we know it.
            | Z.size passSet == 3 ->
                let --- idlist = map _id $ Z.elems passSet -- ids of cards we pass
                    newWorld = Z.foldr (unregisterId . _id) world passSet
                in
                ( Just $ CtsPassSelection passSet
                , newWorld 
                        { _renderWorld = (_renderWorld world){_guiState = DisplayOnly}
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
    S.foldrWithIndex rgstr
        (world{ _renderWorld = (_renderWorld world){_receivedInfo = rinfo}})
        (orderPile hand)
    where rgstr i = registerCard $ ExactPos (-350+ 55*(fromIntegral i), -200 ) -- Switch to HandArea
-- will need to register hand after cards have passed
register rinfo@(RenderInRound hand trick _scores) w =
    flip (S.foldrWithIndex rgstr') trick $
        S.foldrWithIndex rgstr world (orderPile hand)
    where rgstr  i = registerCard $ ExactPos (-350+ 55*(fromIntegral i), -200 ) -- Switch to HandArea
          rgstr' i = registerCard $ ExactPos (-350+ 55*(fromIntegral i), 200  ) -- Switch to PlayArea
          world = w { _renderWorld = (_renderWorld w){_receivedInfo = rinfo}
                    -- , _markIIworld = baseWorld
                    }
register (RenderServerState _ _) w = w
register (BetweenRounds _) w = w{ _markIIworld = emptyRender}
register (Canonical _ _ _) w = w
register (RenderEmpty) w = w{ _markIIworld = emptyRender}

{- Generic Gui elements -}
registerGenericSetID :: Maybe Location -> Maybe Sprite -> Maybe Clickable -> Maybe Target -> GuiWorld -> GuiWorld
registerGenericSetID mLoc mSpr mZon mTar world
    =
    let 
        (idNo, newSup) = freshId $ _idSupply world
    in registerGeneric idNo mLoc mSpr mZon mTar world{_idSupply = newSup}

registerGeneric :: Int -> Maybe Location -> Maybe Sprite -> Maybe Clickable -> Maybe Target -> GuiWorld -> GuiWorld
registerGeneric idNo mLoc mSpr mZon mTar world
    =
    let mIIw = _markIIworld world
    in
    world
    { _markIIworld = mIIw
        { clickables = IntMap.alter (const mZon) idNo (clickables mIIw)
        , targets    = IntMap.alter (const mTar) idNo (targets    mIIw)
        , sprites    = IntMap.alter (const mSpr) idNo (sprites    mIIw)
        , locations  = IntMap.alter (const mLoc) idNo (locations  mIIw)
        -- will probably want id->names for debug log
        }
    }

unregisterId :: Int -> GuiWorld -> GuiWorld
unregisterId i world
    =
    let mIIw = _markIIworld world
    in
    world
        { _markIIworld = mIIw
            { clickables  = IntMap.delete i (clickables  mIIw)
            , sprites     = IntMap.delete i (sprites     mIIw)
            , targets     = IntMap.delete i (targets     mIIw)
            , locations   = IntMap.delete i (locations   mIIw)
            , gameObjects = IntMap.delete i (gameObjects mIIw)
            }
        }

{- Generic Gui elements -}
registerCard :: Zone -> Card -> GuiWorld -> GuiWorld
registerCard pos card world
    =
    let mIIw = _markIIworld world
        cid = _id card
        c = Clickable cid $ clickCard cid card
        s = Sprite   $ renderCard card
        l = Location pos (80,60)
    in
    world
        { _markIIworld = mIIw
            { clickables  = IntMap.insert cid c    (clickables  mIIw)
            , sprites     = IntMap.insert cid s    (sprites     mIIw)
            , locations   = IntMap.insert cid l    (locations   mIIw)
            , gameObjects = IntMap.insert cid card (gameObjects mIIw)
            }
        }
    where clickCard cid _crd w =
            return $ w{ _markIIworld =
                        (_markIIworld w){ dragged = Just cid }
                      -- , _dbgInfo = ((show crd):(_dbgInfo w))
                      }

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
    -- render debugInfo can now move this into render
    let debugInfo = _dbgInfo $ _renderWorld world
        dbg = {-Color rose $-} Translate (-200) (150) $ scale (0.225) (0.225) $ text $ unlines $ take 4 debugInfo
    -- will also want to render in depth order
    return $ Pictures [ dbg
                      {-, render mri-}
                      , render world
                      ]

-- If I moved dragged into an animtions record then I'll either
-- need to make this take the whole world, or, more plausibly,
-- have a renderAnim separate from renderStatic or something
-- also consider where textual things are
render :: GuiWorld -> Picture
render gw
    = Pictures [renderable, dragging]
    -- the proper alternative here is to iterate over zones rendering everything inside them
    -- can render debug info here as well
    where
       renderable =
            Pictures $ map renderSprite
            $ IntMap.elems
            $ IntMap.intersectionWith (,) (sprites mIIw) (locations mIIw)
       mIIw = _markIIworld gw
       mouseCoords = ExactPos $ _mouseCoords ( _renderWorld gw )
       dragging =
            case dragged mIIw of
                Just i -> renderSprite ((IntMap.!) (sprites mIIw) i, (Location (mouseCoords) (80,60)))
                Nothing -> Blank

-- Will need a way to turn a location into coordinates
-- will be made obsolete when zones come online
renderSprite :: (Sprite, Location) -> Picture
renderSprite ((Sprite pic), (Location zone _bbox))
    = let
        correctIdForSprite = error $ "tried to render sprite for: " ++ show pic
        (px,py) = extractPos zone correctIdForSprite
    in
    Translate px py $ pic
{-renderSprite ((Sprite pic), (Location (HandArea (px,py)) _bbox))
    = Translate px py $ pic-}
-- not correct way to render something in a zone

-- this will iterate over objects contained by zone and
-- display each displayable one at their location
-- (in order of depth)
-- then clip the resulting picture to its bounding box
-- translate to zone position
-- and return the picture
{-renderZone :: Zone -> Picture-}
{-renderZone = undefined-}

renderCard :: Card -> Picture
renderCard card
    = Pictures
        [ Color magenta $ rectangleSolid (60) (80)
        , Color (greyN 0.575) $ circleSolid 20
        , Color black $ Translate (-10) (-5) $ Scale (0.125) (0.125) $ Text $ pretty card
        , Color black $ rectangleWire (60) (80)
        ]

emptyRender :: MarkIIRender
emptyRender = MarkIIRender IntMap.empty IntMap.empty IntMap.empty IntMap.empty IntMap.empty Nothing

-- Hearts specific
initWorld :: GuiWorld -> GuiWorld
initWorld =
    (registerGenericSetID
    -- The PlayArea
        (Just $ Location (ExactPos (0,0)) (400,300))
        (Just $ Sprite (Color (makeColor 0.2 0.2 0.2 0.5) $ rectangleSolid (400) (300)))
        {--| No particular action happens upon clicking in play area --}
        (Just $ Clickable 0 return)
        {--| If a card is being dragged, we try to play it as appropriate --}
        (Just $ Target playAreaHandleRelease)
    )
    . 
    (registerGenericSetID
     --    The game window
        (Just $ Location (ExactPos (0,0)) (800,600))
        (Just $ Sprite (Color (makeColor 0.2 0.6 0.2 0.2) $ rectangleSolid (600) (300)))
        {--| No particular action happens upon clicking in window --}
        (Just $ Clickable 0 return)
        {--| If a card is being dragged, it snaps back to zone --}
        (Just $ Target gameWindowHandleRelease)
    )
    .
    (registerGenericSetID
    -- The Hand
        (Just $ Location (ExactPos (0,-200)) (800,100))
        (Just $ Sprite (Color (makeColor 0.4 0.7 0.2 0.5) $ rectangleSolid (800) (100)))
        {--| No particular action happens upon clicking in window --}
        (Just $ Clickable 0 return)
        {--| If a card is being dragged, we try to play it as appropriate --}
        (Just $ Target return)
    )

playAreaHandleRelease :: ClickProcess
playAreaHandleRelease world = 
    let mIIw = _markIIworld world
        renW = _renderWorld world
    in
    return $
        case (dragged mIIw, _guiState renW) of
            (Nothing, _)       -> world
            (_, DisplayOnly)   -> world
            (Just i, SelectCardsToPass soFar) ->
                let card = fromJust (IntMap.lookup i (gameObjects mIIw) )
                    (mx,my) = _mouseCoords renW
                in
                if Z.size soFar < 3 && Z.notMember card soFar
                    -- Card not in set and size of set less than 3
                then world  { _renderWorld
                            = renW  { _dbgInfo = "passing this card ":(_dbgInfo renW)
                                    , _guiState = SelectCardsToPass (Z.insert card soFar)
                                    }
                            , _markIIworld
                            = mIIw  { dragged = Nothing
                                    , locations = IntMap.insert i (Location (ExactPos (mx,my)) (80,60)) (locations mIIw)
                                    }
                            }
                else _addDebug  "cannot add card to passing set " world
            (Just i, SelectCardToPlay hand info Nothing)  ->
                -- let card = (IntMap.! (gameObjects mIIw) i)
                let card = fromJust (IntMap.lookup i (gameObjects mIIw) )
                    (mx,my) = _mouseCoords renW
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

gameWindowHandleRelease :: ClickProcess
gameWindowHandleRelease world
    = 
    return $ case dragged (_markIIworld world) of
        Nothing       -> world
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
 
emptyWorld :: Int -> Supply -> GuiWorld
emptyWorld pos sup = (GuiWorld (RenderGame RenderEmpty DisplayOnly ["Initializing"] pos (0,0)) emptyRender sup)  -- world
