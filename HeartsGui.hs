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
                , _objectStore  :: ObjectStore
                , _idSupply     :: Supply
                , _thingWarehouse :: IntMap Thing
                }
                -- somewhere needs access to mouse position

data MiscState = MiscState
                { _receivedInfo :: RenderInfo
                , _dbgInfo      :: DebugInfo
                , _position     :: Int       -- Player position
                , _mouseCoords  :: Pos
                , _toSend       :: Maybe ClientToServer
                , _current      :: Directive
                -- _animation  --- collect drag and server generated animations
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
class HZone z where -- Switch to multiparameter type classes
    extractPos :: z -> Int -> Maybe Pos
    insert     :: Int -> GuiWorld -> z -> z -- should be w in general case
    remove     :: Int -> z -> z
    clean      :: z -> z -- empties the zone
    allMems    :: z -> [Int] -- returns allMems
    checkPos   :: z -> Pos -> Maybe Int
    render     :: z -> [(Pos, Sprite)]

data ExactZone = ExactZone (IntMap Pos)
data SingletonZone          = ExactPos Pos Thing
                    -- | HandArea Pos
                    ---  Zone ManagementStyle Intmap Pos
instance HZone SingletonZone where
    extractPos z _i = case z of
                      (ExactPos p _ ) -> Just p
    insert _i _w z = z
    remove _i z = z
    clean z = z
    allMems _ = []
    checkPos _z _p = Nothing

{-type ManagementStyle = GuiWorld -> Pos-}
{-
insertAtMousePos :: ManagementStyle
insertAtMousePos gw = _mouseCoords $ _miscState gw
insertNextPos :: ManagementStyle
insertNextPos = undefined
-}

-- depth should maybe be a list of ints so that all cards have same first index, and differ in next index
-- or oranized by Zone or something
type DebugInfo = [String]
type Depth         = Int -- really more like height in that lower numbers are beneath higher numbers
type Bbox          = (Float, Float)
type Pos           = (Float, Float)
data Sprite        = Sprite Picture -- RenderProcess if we need io to render?
data Location      = Location Pos Bbox -- will want vector stuff to handle/change locations
data Clickable     = Clickable
                    { depth        :: Depth
                    , clickProcess :: Trigger
                    }
data Target        = Target
                    { releaseProcess :: Trigger
                    -- id of thing
                    }
type Trigger  = GuiWorld -> IO GuiWorld

data Directive  = Exiting
                | Initializing
                | CollectPass Hand
                | PassNow Hand
                | SelectCard Hand Info
                | SendCard Card
                | Waiting
                deriving (Show)

data ObjectStore = ObjectStore
    -- component entity like system
    { _clickables  :: IntMap Clickable
    , _targets     :: IntMap Target
    , _sprites     :: IntMap Sprite
    -- consider using viewports rather than locations
    , _locations   :: IntMap Location -- [ExactZone] should go through zones
    , _gameObjects :: IntMap HeartsCommon.Card
    , _dragged     :: Maybe Int -- ID of card currently being dragged
    -- dragged can be changed into a particular zone once the new regime works
    -- movement paths handed to us
    -- need Zones
    }
-- alternatively 

data Thing = Thing
    { _action :: Maybe Clickable
    , _reaction :: Maybe Target
    , _object :: Maybe Card
    , _sprite :: Maybe Sprite
    }

makeLenses ''GuiWorld
makeLenses ''MiscState
makeLenses ''ObjectStore

instance HZone ExactZone where
    extractPos (ExactZone z) i = IntMap.lookup i z
    insert i w (ExactZone z)   = ExactZone $ IntMap.insert i (w ^. miscState . mouseCoords) z
    remove i (ExactZone z)     = ExactZone $ IntMap.delete i z
    clean _z                   = ExactZone $ IntMap.empty
    allMems (ExactZone z)      = IntMap.keys z
    checkPos _z _p             = error "checkPos not implemented"

_addDebug :: String -> GuiWorld -> GuiWorld
_addDebug s w = w & miscState . dbgInfo %~ (s:)


guiThread :: TMVar ServerToClient -> TMVar ClientToServer -> Int -> IO ()
guiThread inbox outbox pos
    = do
        -- server should be passing supply, probably in intiatialization method
        idSup <- newSupply
        playIO
            window
            white			 -- background color
            100              -- steps per second
            (initWorld $ emptyWorld pos idSup) -- world
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
                                    $ IntMap.intersection (world ^. objectStore . clickables)
                                    $ IntMap.filter (isInRegion mpos)
                                    $ world ^. objectStore . locations
                    cmpDepth z (d, a) = let d' = depth z in if d' > d then (d', clickProcess z) else (d, a)
                -- select the object with highest depth, and run its on click
                in action world 
            (MouseButton _, Up)
                -> do
                let shouldGoOff = reverse
                        $ map releaseProcess
                        $ IntMap.elems
                        $ IntMap.intersection (world ^. objectStore . targets)
                        $ IntMap.filter (isInRegion mpos)
                        $ world ^. objectStore . locations
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
isInRegion (mx,my) (Location ((cx,cy)) (bx,by)) =
       cx -  bx /2 <= mx
    && mx <= cx +     bx /2
    && cy -  by /2 <= my
    && my <= cy    +  by /2

commHandle :: TMVar ServerToClient -> TMVar ClientToServer -> Float -> GuiWorld -> IO GuiWorld
commHandle inbox outbox _t world
    = do
    -- check inbox
    messageReceived <- atomically $ tryTakeTMVar inbox
    let world' = processMessage messageReceived world
    let outMessage = world' ^. miscState . toSend
    maybe (return ()) (atomically . (putTMVar outbox)) outMessage
    return $ world' & miscState . toSend .~ Nothing

processMessage :: Maybe ServerToClient -> GuiWorld -> GuiWorld
processMessage messageReceived world =
    let acknowledged = world & miscState . toSend  .~ Just CtsAcknowledge
                             & miscState . current .~ Waiting
    in
    case messageReceived of
    Just (StcGameStart i) -> 
        acknowledged & miscState . position .~ i
    Just StcGameOver -> 
        acknowledged & miscState . current .~ Exiting
    Just (StcGetPassSelection _ _) -> 
        world & miscState . current .~ (CollectPass Z.empty)
    Just (StcWasPassed _) -> acknowledged -- soon these will be registered to hand zone
    Just (StcGetMove hand info) -> 
        world & miscState . current .~ (SelectCard hand info)
    Just (StcRender rinfo ) -> registerWorld rinfo acknowledged  -- soon we'll just register the trick
    Just StcCleanTrick -> unregisterTrick acknowledged -- also unregistering shit?
    Nothing ->
        case world ^. miscState . current of
        SendCard c   -> unregisterId (_id c) $
            world & miscState . toSend .~ Just (CtsMove c) & miscState . current .~ Waiting
        PassNow cs   -> 
            -- TODO switch flip with &
            flip (Z.foldr (unregisterId . _id)) cs $
            world & miscState . toSend .~ Just (CtsPassSelection cs) & miscState . current .~ Waiting
        
        Exiting         -> undefined-- send CTS terminate unless we just recieved it?
        Initializing    -> world

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
registerWorld (BetweenRounds _) w = w & objectStore .~ emptyRender 
registerWorld (Canonical _ _ _) w = w
registerWorld (RenderEmpty) w = w & objectStore .~ emptyRender 

registerHand :: Hand -> GuiWorld -> GuiWorld
registerHand hand world =
    S.foldrWithIndex rgstr world (orderPile hand)
    where rgstr i = registerCard $ (-351+ 55*(fromIntegral i), -200 ) -- Switch to HandArea

registerTrick :: Trick -> GuiWorld -> GuiWorld
registerTrick trick world =
    S.foldrWithIndex rgstr world trick
    where rgstr i = registerCard $ (-351+ 55*(fromIntegral i), 200 ) -- Switch to HandArea

{- Generic Gui elements -}
registerGenericSetID :: Maybe Location -> Maybe Sprite -> Maybe Clickable -> Maybe Target -> GuiWorld -> GuiWorld
registerGenericSetID mLoc mSpr mZon mTar world
    =
    let (idNo, newSup) = freshId $ world ^. idSupply
    in registerGeneric idNo mLoc mSpr mZon mTar $ world & idSupply .~ newSup

registerThing :: Int -> Thing -> GuiWorld -> GuiWorld -- do we want an initial zone?
registerThing i t world = world & thingWarehouse %~ IntMap.insert i t

registerGeneric :: Int -> Maybe Location -> Maybe Sprite -> Maybe Clickable -> Maybe Target -> GuiWorld -> GuiWorld
registerGeneric idNo mLoc mSpr mZon mTar world
    =
    let t = Thing mZon mTar Nothing mSpr
    in 
    world
    & objectStore . clickables %~ IntMap.alter (const mZon) idNo
    & objectStore . targets    %~ IntMap.alter (const mTar) idNo
    & objectStore . sprites    %~ IntMap.alter (const mSpr) idNo
    & objectStore . locations  %~ IntMap.alter (const mLoc) idNo
    & registerThing idNo t

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
    & objectStore . clickables  %~ IntMap.delete i
    & objectStore . targets     %~ IntMap.delete i
    & objectStore . sprites     %~ IntMap.delete i
    & objectStore . locations   %~ IntMap.delete i
    & objectStore . gameObjects %~ IntMap.delete i
    & thingWarehouse %~ IntMap.delete i
    -- probably need to broadcast delete over zones

{- Generic Gui elements -}
registerCard :: Pos -> Card -> GuiWorld -> GuiWorld
registerCard pos card world
    =
    let 
        cid = _id card
        c   = Clickable cid $ return . (set (objectStore . dragged) (Just cid))
        s   = Sprite        $ renderCard card
        l   = Location  pos (80,60)
        thing = Thing (Just c) Nothing (Just card) (Just s)
        zone = ExactPos pos thing
    in
    -- can I use better lens magic for this?
    world
    & objectStore . clickables  %~ IntMap.insert cid c
    & objectStore . sprites     %~ IntMap.insert cid s
    & objectStore . locations   %~ IntMap.insert cid l
    & objectStore . gameObjects %~ IntMap.insert cid card
    & thingWarehouse %~ IntMap.insert cid thing

{-registerButton :: Location -> Sprite -> Trigger -> GuiWorld -> GuiWorld-}
{--- registerButton = undefined -- needs to actually register button-}

drawWorld :: GuiWorld -> IO Picture
drawWorld world
    = do
    -- render debugInfo can now move this into render
    let debugInfo = world ^. miscState . dbgInfo
        dbg = {-Color rose $-} Translate (-200) (150) $ scale (0.225) (0.225) $ text $ unlines $ take 4 debugInfo
    -- will also want to render in depth order
    return $ Pictures [ dbg
                      , renderW world
                      ]

-- If I moved dragged into an animtions record then I'll either
-- need to make this take the whole world, or, more plausibly,
-- have a renderAnim separate from renderStatic or something
-- also consider where textual things are
renderW :: GuiWorld -> Picture
renderW world
    = Pictures [renderable, dragging]
    -- the proper alternative here is to iterate over zones rendering everything inside them
    -- can render debug info here as well
    where
       renderable =
            Pictures $ map renderSprite
            $ IntMap.elems
            $ IntMap.intersectionWith (,) 
                (world ^. objectStore . sprites) 
                (world ^. objectStore . locations) 
       mpos = world ^. miscState . mouseCoords
       dragging =
            case _dragged (world ^. objectStore) of
                Just i -> renderSprite  ( views (objectStore . sprites) (IntMap.! i) world
                                        , Location (mpos) (80,60) 
                                        )
                Nothing -> Blank

-- Will need a way to turn a location into coordinates
-- will be made obsolete when zones come online
renderSprite :: (Sprite, Location) -> Picture
renderSprite ((Sprite pic), (Location pos _bbox))
    = 
    let (px,py) = pos
        {-correctIdForSprite = error $ "tried to render sprite for: " ++ show pic
        (px,py) = maybe (error "no position") id $ extractPos zone correctIdForSprite-}
    in
    Translate px py $ pic

-- not correct way to render something in a zone

-- this will iterate over objects contained by zone and
-- display each displayable one at their location
-- (in order of depth)
-- then clip the resulting picture to its (Zone's) bounding box?
-- translate to zone position
-- and return the picture
{-renderZone :: GuiWorld -> Zone -> Picture
renderZone = undefined-}

renderCard :: Card -> Picture
renderCard card
    = Pictures
        [ Color magenta $ rectangleSolid ( 60  ) ( 80 )
        , Color         ( greyN 0.575 )  $ circleSolid 20
        , Color black   $ Translate      ( -10 ) ( -5 ) $ Scale ( 0.125) ( 0.125) $ Text $ pretty card
        , Color black   $ rectangleWire  ( 60  ) ( 80 )
        ]

emptyRender :: ObjectStore
emptyRender = ObjectStore IntMap.empty IntMap.empty IntMap.empty IntMap.empty IntMap.empty Nothing

-- Hearts specific
initWorld :: GuiWorld -> GuiWorld
initWorld =
    (registerGenericSetID
    -- The PlayArea
        (Just $ Location ((0,0)) (400,300))
        (Just $ Sprite (Color (makeColor 0.2 0.2 0.2 0.5) $ rectangleSolid (400) (300)))
        {--| No particular action happens upon clicking in play area --}
        (Just $ Clickable 0 return)
        {--| If a card is being dragged, we try to play it as appropriate --}
        (Just $ Target playAreaHandleRelease)
    )
    . 
    (registerGenericSetID
     --    The game window
        (Just $ Location ((0,0)) (800,600))
        (Just $ Sprite (Color (makeColor 0.2 0.6 0.2 0.2) $ rectangleSolid (600) (300)))
        {--| No particular action happens upon clicking in window --}
        (Just $ Clickable 0 return)
        {--| If a card is being dragged, it snaps back to zone --}
        (Just $ Target gameWindowHandleRelease)
    )
    .
    (registerGenericSetID
    -- The Hand
        (Just $ Location ((0,-200)) (800,100))
        (Just $ Sprite (Color (makeColor 0.4 0.7 0.2 0.5) $ rectangleSolid (800) (100)))
        {--| No particular action happens upon clicking in window --}
        (Just $ Clickable 0 return)
        {--| If a card is being dragged, we try to play it as appropriate --}
        (Just $ Target return)
    )

playAreaHandleRelease :: Trigger
playAreaHandleRelease world = return $ maybe id processCardRelease (world ^. objectStore . dragged) $ world

processCardRelease :: Int -> GuiWorld -> GuiWorld
processCardRelease i world =
        case (world ^. miscState . current) of
            CollectPass soFar ->
                let card = views (objectStore . gameObjects) (IntMap.! i) world
                    (mx,my) = world ^. miscState . mouseCoords
                in
                if Z.size soFar < 3 && Z.notMember card soFar
                    -- Card not in set and size of set less than 3
                then world  
                    & miscState . current .~ 
                            (
                                let newSet = (Z.insert card soFar) 
                                in 
                                if (Z.size newSet == 3)
                                then PassNow newSet
                                else CollectPass newSet
                            )
                    -- TODO FIXME implement button for the switch
                    & objectStore . locations %~ IntMap.insert i (Location ((mx,my)) (80,60))
                    & objectStore . dragged   .~ Nothing
                    & miscState   . dbgInfo   %~ ("Passing this card. ":)
                else world 
                    & miscState   . dbgInfo   %~ ("cannot add card to passing set ":)

            SelectCard hand info  ->
                let card = views (objectStore . gameObjects) (IntMap.! i) world
                    (mx,my) = world ^. miscState . mouseCoords
                in
                if isValidPlay hand info card
                then world
                        & miscState   . current   .~ SendCard card
                        & objectStore . dragged   .~ Nothing
                        & objectStore . locations %~ IntMap.insert i (Location ((mx,my)) (80,60))
                else world 
                        & miscState   . dbgInfo   %~ ("Not valid play. ":)

            Waiting -> world

            s       -> error $ "dragging id " ++ (show i) ++ " while in state " ++ show s

gameWindowHandleRelease :: Trigger
gameWindowHandleRelease world = return $ world & objectStore . dragged .~ Nothing
 
emptyWorld :: Int -> Supply -> GuiWorld
emptyWorld pos sup = GuiWorld (MiscState RenderEmpty ["Initializing"] pos (0,0) Nothing Initializing) emptyRender sup (IntMap.empty)
