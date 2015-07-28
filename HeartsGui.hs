{-# LANGUAGE TemplateHaskell #-}
module HeartsGui
    ( guiThread
    )
    where

import HeartsCommon
import qualified Data.Set as Z
import qualified Data.Sequence as S
import qualified Data.Foldable as F
-- import Control.Concurrent.Async
import Control.Concurrent.STM
-- import Control.Concurrent.STM.TMVar

import Control.Lens

-- may want to consider
-- idSupply Data.Unique.ID or monadSupply or
-- Control.Eff.Fresh or some such
-- decided on Control.Concurrent.Supply for two reasons: first I forgot where this
-- comment was so just found searched for what I wanted again, second this library
-- was written by ekmett and is in LTS stackage
import Control.Concurrent.Supply

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
                { _miscState  :: MiscState
                , _markIIworld  :: MarkIIRender
                , _messages     :: MessageHandler
                -- , _miscState    :: MiscState
                , _idSupply     :: Supply
                }
                -- somewhere needs access to mouse position
-- TODO fix this, maybe with lenses, maybe just with a principled reorganization of guiworld and constituents

data MiscState = MiscState
                { _receivedInfo :: RenderInfo
                , _dbgInfo      :: DebugInfo
                , _position     :: Int          -- Player position
                , _mouseCoords  :: Pos
                -- _animation  --- collect drag and server generated animations
                -- may also need a place to register current effect seeking target
                }

{-data MiscState = MiscState 
                -- move message handler into here?
                {
                , _mouseCoords  :: Pos
                }
-}
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
    clean :: z -> z -- empties the zone
    allMems :: z -> [i] -- returns allMems

data Zone          = PlayArea Pos | ExactPos Pos
                    -- HandArea Pos |
                    ---  Zone ManagementStyle Intmap Pos
instance HZone Zone where
    extractPos z _i = case z of
                      (PlayArea p) -> p
                      (ExactPos p) -> p
    insert _a z = z
    remove _i z = z
    clean z = z
    allMems _ = []

{-type ManagementStyle = GuiWorld -> Pos-}
{-
insertAtMousePos :: ManagementStyle
insertAtMousePos gw = _mouseCoords $ _miscState gw
insertNextPos :: ManagementStyle
insertNextPos = undefined
-}

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
                    }
data Target        = Target
                    { releaseProcess :: ClickProcess
                    -- id of thing
                    }
-- type RenderProcess = IO Picture
type ClickProcess  = GuiWorld -> IO GuiWorld

data MarkIIRender = MarkIIRender
    -- component entity like system
    { _clickables  :: IntMap Clickable
    , _targets     :: IntMap Target
    , _sprites     :: IntMap Sprite
    -- consider using viewports rather than locations
    , _locations   :: IntMap Location -- should go through zones
    , _gameObjects :: IntMap HeartsCommon.Card
    , _dragged     :: Maybe Int -- ID of card currently being dragged
    -- movement paths handed to us
    -- need Zones
    }
    -- possibly name or debug info or logging deserves a place here
    -- will also want a set of logical zones that arrange things inside of them e,g, hand play
type DebugInfo = [String]

data MessageHandler = MessageHandler
                    { _toSend  :: Maybe ClientToServer
                    , _current :: Directive 
                    }

data Directive  = Exiting
                | Initializing
                | CollectPass Hand
                | PassNow Hand
                | SelectCard Hand Info
                | SendCard Card
                | Waiting
                deriving (Show)

makeLenses ''GuiWorld
makeLenses ''MiscState
makeLenses ''MarkIIRender
makeLenses ''MessageHandler

_addDebug :: String -> GuiWorld -> GuiWorld
_addDebug s w = w & miscState . dbgInfo %~ (s:)


guiThread :: TMVar ServerToClient -> TMVar ClientToServer -> Int -> IO ()
guiThread inbox outbox pos
    = do
        idSup <- newSupply
        playIO
            window
            white			 -- background color
            100              -- steps per second
            (initWorld $ emptyWorld pos (MessageHandler Nothing Initializing) idSup) -- world
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
    = case event of
    EventResize _ws -> return $ world
    EventMotion mpos --(mx,my)
        -> return $ world & miscState . mouseCoords .~ mpos
    EventKey k ks _mod mpos
        -> case (k, ks) of
            (MouseButton _, Down)
                ->
                -- register the click with an object (clickable)
                let (_d, action) = IntMap.foldr cmpDepth (-1, return)
                                    $ IntMap.intersection (world ^. markIIworld . clickables)
                                    $ IntMap.filter (isInRegion mpos)
                                    $ world ^. markIIworld . locations
                    cmpDepth z (d, a) = let d' = depth z in if d' > d then (d', clickProcess z) else (d, a)
                -- select the object with highest depth, and run its on click
                in action world 
            (MouseButton _, Up)
                -> do
                -- will clean this up eventually
                let shouldGoOff = reverse
                        $ map releaseProcess
                        $ IntMap.elems
                        $ IntMap.intersection (world ^. markIIworld . targets)
                        $ IntMap.filter (isInRegion mpos)
                        $ world ^. markIIworld . locations
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
    inNote <- atomically $ tryTakeTMVar inbox
    let world' = processMessage inNote world
    let outMessage = world' ^. messages . toSend
    case outMessage of
        Nothing -> return ()
        Just reply -> do
            atomically $ putTMVar outbox $ reply
    return $ world' & messages . toSend .~ Nothing

processMessage :: Maybe ServerToClient -> GuiWorld -> GuiWorld
processMessage messageReceived world =
    let 
        acknowledged = world & messages . toSend .~ Just CtsAcknowledge & messages . current .~ Waiting
    in
    case messageReceived of
    Just (StcGameStart i) -> 
        acknowledged & miscState . position .~ i
    Just StcGameOver -> 
        acknowledged & messages . current .~ Exiting
    Just (StcGetPassSelection _ _) -> 
        world & messages . current .~ (CollectPass Z.empty)
    Just (StcWasPassed _) -> acknowledged -- soon these will be registered to hand zone
    Just (StcGetMove hand info) -> 
        world & messages . current .~ (SelectCard hand info)
    Just (StcRender rinfo ) -> registerWorld rinfo acknowledged  -- soon we'll just register the trick
    Just StcCleanTrick -> unregisterTrick acknowledged -- also unregistering shit?
    Nothing ->
        case world ^. messages . current of
        SendCard c   -> unregisterId (_id c) $
            world & messages . toSend .~ Just (CtsMove c) & messages . current .~ Waiting
        PassNow cs   -> 
            flip (Z.foldr (unregisterId . _id)) cs $
            -- AS A TEMPORARY HACK we are unregistering upon sending this information, as it is the last place we know we know it.
            world & messages . toSend .~ Just (CtsPassSelection cs) & messages . current .~ Waiting
        Exiting      -> undefined-- send CTS terminate unless we just recieved it?
        Initializing -> world

        -- don't need to do anything for waiting, selectcard, collectpass
        Waiting         -> world
        SelectCard _ _  -> world
        CollectPass _cs -> world -- temporary check to see if 3 cards then switch


{- Register nonsense takes guiworld to guiworld -}
registerWorld :: RenderInfo -> GuiWorld -> GuiWorld
registerWorld rinfo@(Passing hand _passdir) world =
    registerHand hand (world & miscState . receivedInfo .~ rinfo)

-- will need to register hand after cards have passed
registerWorld rinfo@(RenderInRound hand trick _scores) world =
    registerTrick trick $ registerHand hand $ world & miscState . receivedInfo .~ rinfo

registerWorld (RenderServerState _ _) w = w
registerWorld (BetweenRounds _) w = w & markIIworld .~ emptyRender 
registerWorld (Canonical _ _ _) w = w
registerWorld (RenderEmpty) w = w & markIIworld .~ emptyRender 

registerHand :: Hand -> GuiWorld -> GuiWorld
registerHand hand world =
    S.foldrWithIndex rgstr world (orderPile hand)
    where rgstr i = registerCard $ ExactPos (-351+ 55*(fromIntegral i), -200 ) -- Switch to HandArea

registerTrick :: Trick -> GuiWorld -> GuiWorld
registerTrick trick world =
    S.foldrWithIndex rgstr world trick
    where rgstr i = registerCard $ ExactPos (-351+ 55*(fromIntegral i), 200 ) -- Switch to HandArea

{- Generic Gui elements -}
registerGenericSetID :: Maybe Location -> Maybe Sprite -> Maybe Clickable -> Maybe Target -> GuiWorld -> GuiWorld
registerGenericSetID mLoc mSpr mZon mTar world
    =
    let 
        (idNo, newSup) = freshId $ world ^. idSupply
    in registerGeneric idNo mLoc mSpr mZon mTar $ world & idSupply .~ newSup

registerGeneric :: Int -> Maybe Location -> Maybe Sprite -> Maybe Clickable -> Maybe Target -> GuiWorld -> GuiWorld
registerGeneric idNo mLoc mSpr mZon mTar world
    =
    world
    & markIIworld . clickables %~ IntMap.alter (const mZon) idNo
    & markIIworld . targets %~ IntMap.alter (const mTar) idNo
    & markIIworld . sprites %~ IntMap.alter (const mSpr) idNo
    & markIIworld . locations %~ IntMap.alter (const mLoc) idNo

unregisterTrick :: GuiWorld -> GuiWorld
unregisterTrick world =
    case world ^. miscState . receivedInfo of 
        RenderInRound _hand trick _scores -> F.foldr (unregisterId . _id) world trick         
        _ -> error $ "Called unregisterTrick while not inRound"
    

unregisterId :: Int -> GuiWorld -> GuiWorld
unregisterId i world
    =
    -- should use a fold or traverse or some other clever lens thing
    world
    & markIIworld . clickables %~ IntMap.delete i
    & markIIworld . targets %~ IntMap.delete i
    & markIIworld . sprites %~ IntMap.delete i
    & markIIworld . locations %~ IntMap.delete i
    & markIIworld . gameObjects %~ IntMap.delete i

{- Generic Gui elements -}
registerCard :: Zone -> Card -> GuiWorld -> GuiWorld
registerCard pos card world
    =
    let 
        cid = _id card
        c = Clickable cid $ return . (set (markIIworld . dragged) (Just cid))
        s = Sprite   $ renderCard card
        l = Location pos (80,60)
    in
    -- can I use better lens magic for this?
    world
    & markIIworld . clickables %~ IntMap.insert cid c
    & markIIworld . sprites %~ IntMap.insert cid s
    & markIIworld . locations %~ IntMap.insert cid l
    & markIIworld . gameObjects %~ IntMap.insert cid card

{-registerButton :: Location -> Sprite -> ClickProcess -> GuiWorld -> GuiWorld-}
{--- registerButton = undefined -- needs to actually register button-}

drawWorld :: GuiWorld -> IO Picture
drawWorld world
    = do
    -- render debugInfo can now move this into render
    let debugInfo = world ^. miscState . dbgInfo
        dbg = {-Color rose $-} Translate (-200) (150) $ scale (0.225) (0.225) $ text $ unlines $ take 4 debugInfo
    -- will also want to render in depth order
    return $ Pictures [ dbg
                      , render world
                      ]

-- If I moved dragged into an animtions record then I'll either
-- need to make this take the whole world, or, more plausibly,
-- have a renderAnim separate from renderStatic or something
-- also consider where textual things are
render :: GuiWorld -> Picture
render world
    = Pictures [renderable, dragging]
    -- the proper alternative here is to iterate over zones rendering everything inside them
    -- can render debug info here as well
    where
       renderable =
            Pictures $ map renderSprite
            $ IntMap.elems
            $ IntMap.intersectionWith (,) 
                (world ^. markIIworld . sprites) 
                (world ^. markIIworld . locations) 
       mpos = ExactPos $ world ^. miscState . mouseCoords
       dragging =
            case _dragged (world ^. markIIworld) of
                Just i -> renderSprite  ( views (markIIworld . sprites) (IntMap.! i) world
                                        , Location (mpos) (80,60) 
                                        )
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
playAreaHandleRelease world = return $ maybe id processCardRelease (world ^. markIIworld . dragged) $ world


processCardRelease :: Int -> GuiWorld -> GuiWorld
processCardRelease i world =
        case (world ^. messages . current) of
            CollectPass soFar ->
                let card = views (markIIworld . gameObjects) (IntMap.! i) world
                    (mx,my) = world ^. miscState . mouseCoords
                in
                if Z.size soFar < 3 && Z.notMember card soFar
                    -- Card not in set and size of set less than 3
                then world  
                    & messages . current .~ 
                            (
                                let newSet = (Z.insert card soFar) 
                                in 
                                if (Z.size newSet == 3)
                                then PassNow newSet
                                else CollectPass newSet
                            )
                    -- TODO FIXME implement button for the switch
                    & markIIworld . locations %~ IntMap.insert i (Location (ExactPos (mx,my)) (80,60))
                    & markIIworld . dragged .~ Nothing
                    & miscState . dbgInfo %~ ("Passing this card. ":)
                else world & miscState . dbgInfo %~ ("cannot add card to passing set ":)
            SelectCard hand info  ->
                let card = views (markIIworld . gameObjects) (IntMap.! i) world
                    (mx,my) = world ^. miscState . mouseCoords
                in
                if isValidPlay hand info card
                then world
                        & messages    . current .~ SendCard card
                        & markIIworld . dragged .~ Nothing
                        & markIIworld . locations %~ IntMap.insert i (Location (ExactPos (mx,my)) (80,60))
                else world & miscState . dbgInfo %~ ("Not valid play. ":)
            Waiting -> world
            s -> error $ "dragging id " ++ (show i) ++ " while in state " ++ show s

gameWindowHandleRelease :: ClickProcess
gameWindowHandleRelease world = return $ world & markIIworld . dragged .~ Nothing
 
emptyWorld :: Int -> MessageHandler -> Supply -> GuiWorld
emptyWorld pos mess sup = (GuiWorld (MiscState RenderEmpty ["Initializing"] pos (0,0)) emptyRender mess sup)  -- world
