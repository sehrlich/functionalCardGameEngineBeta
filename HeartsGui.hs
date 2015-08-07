{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module HeartsGui
    ( guiThread
    )
    where

import HeartsCommon
import GuiZones
import qualified Data.Set as Z
import qualified Data.Sequence as S
-- import Data.Foldable (Foldable)
import qualified Data.Foldable as F
-- import Control.Concurrent.Async
import Control.Concurrent.STM
-- import Control.Concurrent.STM.TMVar

import Control.Lens
-- lens operators can be referred to from 
-- https://github.com/ekmett/lens/wiki/Operators
-- https://www.fpcomplete.com/school/to-infinity-and-beyond/pick-of-the-week/a-little-lens-starter-tutorial#actually-there-are-a-whole-lot-of-operators-in-lens---over-100

-- may want to consider
-- idSupply Data.Unique.ID or monadSupply or
-- Control.Eff.Fresh or some such
-- decided on Control.Concurrent.Supply for two reasons: first I forgot where this
-- comment was so just found searched for what I wanted again, second this library
-- was written by ekmett and is in LTS stackage
import Control.Concurrent.Supply

import Graphics.Gloss
-- import qualified Graphics.Gloss.Data.Color as C
import Graphics.Gloss.Interface.IO.Game --(playIO, Event(..) )

import Data.Maybe (catMaybes, listToMaybe, fromMaybe)
-- Overview:
-- Gui Stuff
-- -- Thread
-- Play thread is responsible for turning a list of game objects into a coherent
-- picture and letting you interact with it
-- Spawns child thread for communication with server
--
-- -- Communication
-- Initialization method should pass supply, possibly seed if seed is needed?
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
    , _zones :: ZoneList
    }

data ZoneList = ZoneList
    { _handZone :: ExactZone Thing -- managedzone
    , _playZone :: ExactZone Thing -- managedzone
    , _draggingZone :: SingletonZone Thing
    , _guiObjects :: ExactZone Thing 
    -- , _extraZones :: [AllZones]
    }
                

-- possibly rename current to currently
data MiscState = MiscState
    { _receivedInfo :: RenderInfo
    , _dbgInfo      :: DebugInfo
    , _position     :: Int       -- Player position
    , _mouseCoords  :: Pos
    , _toSend       :: Maybe ClientToServer
    , _current      :: Directive
    , _idSupply     :: Supply
    }
    -- may need to register current effect seeking target
    
    -- _animation  --- collect drag and server generated animations
    -- these should be appropriate zones

type DebugInfo = [String]
type Bbox          = (Float, Float)
type Pos           = (Float, Float)
data Sprite        = Sprite Picture -- RenderProcess if we need io to render?
data Location      = Location { _pos::Pos, _bbox::Bbox} -- will want vector stuff to handle/change locations

-- TODO: should probably just remove depth outright
-- depth should maybe be a list of ints so that all cards have same first index, and differ in next index
-- or oranized by Zone or something
type Depth         = Int -- really more like height in that lower numbers are beneath higher numbers
data Clickable     = Clickable
                    { _depth       :: Depth
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

data Thing = Thing
    { _action :: Maybe Clickable
    , _reaction :: Maybe Target
    , _object :: Maybe Card
    , _sprite :: Maybe Sprite
    , _location :: Maybe Location
    , _objId :: Int
    }

instance IDable Thing where
    getID = _objId

-- these are at the bottom for template haskell nonsense reasons
makeLenses ''Location
makeLenses ''GuiWorld
makeLenses ''MiscState
makeLenses ''ZoneList
makeLenses ''Thing


_allMems :: Zone z => z Thing -> [Int] -- returns allMems
_allMems = (map getID) . F.toList 

-- since Picture is a Monoid this might be even simpler
render     :: Zone z => z Thing -> Picture
render z = Pictures $! map renderThing (F.toList z)

-- similarly, there might be a nifty way to do this as [] is a monoid as well
checkAllT  :: Zone z => z Thing -> Pos -> [Thing]
checkAllT z p = catMaybes $! fmap (addIfInRegion) $! F.toList z 
    where addIfInRegion t
            = do
            loc <- (t ^. location)
            if isInRegion p loc 
            then Just t 
            else Nothing
-- an instance where it would be nice to have a lens into zones
updatePos :: Pos -> SingletonZone Thing -> SingletonZone Thing
updatePos p (SingletonZone z) = SingletonZone $ do
        t <- z
        loc <- t ^. location
        return $! t & location .~ (Just $ loc{_pos = p} )

_addDebug :: String -> GuiWorld -> GuiWorld
_addDebug s w = w & miscState . dbgInfo %~ (s:)


guiThread :: TMVar ServerToClient -> TMVar ClientToServer -> Int -> IO ()
guiThread inbox outbox playerPos
    = do
        -- server should be passing supply, probably in intiatialization message
        -- it makes sense to supply playerPos there as well
        idSup <- newSupply
        playIO
            window
            white			 -- background color
            100              -- steps per second
            (emptyWorld playerPos idSup) -- world
            drawWorld        -- picture to display
            eventHandle      -- event handler
            (commHandle inbox outbox) -- time update
    where window = (InWindow
                   "Gloss" 	    -- window title
                                -- title fixed for xmonad
                   (800, 600)   -- window size
                   (0, 0)) 	-- window position

-- Gui Event loop
eventHandle :: Event -> GuiWorld -> IO GuiWorld
eventHandle event world
    = case event of
    EventResize _ws -> return $! world
    EventMotion mpos --(mx,my)
        -> return $! world 
                    & miscState . mouseCoords  .~ mpos
                    & zones     . draggingZone %~ updatePos mpos
    EventKey k ks _mod mpos
        -> case (k, ks) of
            (MouseButton _, Down)
                -> do
                let h =  mpos & (checkAllT $ world ^. zones . handZone)
                let p =  mpos & (checkAllT $ world ^. zones . playZone)
                let go = mpos & (checkAllT $ world ^. zones . guiObjects)
                -- let ez = fmap (flip checkAllT mpos) $ world ^. zones . extraZones
                
                -- should make zones foldable
                let trigger = listToMaybe $ map clickProcess $ catMaybes $ 
                                map (^. action) (concat  [h,p,go]) -- ,ez
                fromMaybe return trigger world
                -- F.foldrM (releaseProcess) world shouldGoOff
            (MouseButton _, Up)
                -> do
                let h =  mpos & (checkAllT $ world ^. zones . handZone)
                let p =  mpos & (checkAllT $ world ^. zones . playZone)
                let go = mpos & (checkAllT $ world ^. zones . guiObjects)
                -- let ez = concatMap (flip checkAll mpos) $ world ^. zones . extraZones
                
                -- should make zones foldable
                let shouldGoOff = catMaybes $ map 
                                    ( ^. reaction) 
                                    (concat [h,p,go])
                                    -- (concat [h,p,go,ez])
                F.foldrM (releaseProcess) world shouldGoOff

            (SpecialKey KeyEsc, Up) 
                -> return $! world & miscState . toSend .~ Just CtsDisconnect
            _   -> return $! world

-- Generic Gui -- Gui Elements -- collision detection
isInRegion :: (Float, Float) -> Location -> Bool
isInRegion (mx,my) (Location ((cx,cy)) (bx,by)) =
       cx -  bx /2 <= mx
    && mx <= cx +     bx /2
    && cy -  by /2 <= my
    && my <= cy    +  by /2

commHandle :: TMVar ServerToClient -> TMVar ClientToServer -> Float -> GuiWorld -> IO GuiWorld
commHandle inbox outbox t world
    = do
    messageReceived <- atomically $ tryTakeTMVar inbox
    let world'      = processMessage messageReceived world
    let outMessage  = world' ^. miscState . toSend
    maybe           (return ()) (atomically . (putTMVar outbox)) outMessage
    return          $ world' & miscState . toSend .~ Nothing
                    & miscState . dbgInfo .~ [show t]

processMessage :: Maybe ServerToClient -> GuiWorld -> GuiWorld
processMessage messageReceived world =
    -- use prism to remove Just
    let acknowledged = 
            world 
                & miscState . toSend  .~ Just CtsAcknowledge 
                & miscState . current .~ Waiting
        cleaned = 
            acknowledged
                & zones . playZone %~ clean
                & zones . handZone %~ clean
    in
    case messageReceived of
    Just (StcGameStart i)          -> acknowledged & miscState . position .~ i
    Just StcGameOver               -> acknowledged & miscState . current .~ Exiting
    Just (StcGetPassSelection _ _) -> world        & miscState . current .~ (CollectPass Z.empty)
    Just (StcWasPassed _)          -> cleaned ---- maybe keep track of info intelligently
    Just (StcGetMove hand info)    -> world        & miscState . current .~ (SelectCard hand info)
    Just (StcRender rinfo )        -> registerWorld rinfo acknowledged  -- soon we'll just register the trick
    Just StcCleanTrick             -> cleaned
    Nothing ->
        case world ^. miscState . current of
            -- consider using prism to remove Just
        SendCard c      -> world & miscState . toSend .~ Just (CtsMove c) 
                                 & miscState . current .~ Waiting
        PassNow cs      -> world & miscState . toSend .~ Just (CtsPassSelection cs) 
                                 & miscState . current .~ Waiting
                                 & zones . playZone  %~ clean
        
        Exiting         -> undefined-- send CTS terminate unless we just recieved it?
        Initializing    -> world

        Waiting         -> world
        SelectCard _ _  -> world
        CollectPass _cs -> world


{- Register nonsense takes guiworld to guiworld -}
registerWorld :: RenderInfo -> GuiWorld -> GuiWorld
registerWorld rinfo@(Passing hand _passdir) world =
    registerHand hand (world & miscState . receivedInfo .~ rinfo)

-- will need to register hand after cards have passed
registerWorld rinfo@(RenderInRound hand trick _scores) world =
    registerTrick trick $ registerHand hand $ world & miscState . receivedInfo .~ rinfo

registerWorld (RenderServerState _ _) w = w
registerWorld (BetweenRounds _) w = w 
registerWorld (Canonical _ _ _) w = w
registerWorld (RenderEmpty) w = w 

registerHand :: Hand -> GuiWorld -> GuiWorld
registerHand hand world =
    S.foldrWithIndex rgstr world (orderPile hand)
    where rgstr i c w = w & zones . handZone %~ manage (crd i c)
          loc i = Location (-351+ 55*(fromIntegral i), -200 ) (80,60)
          crd i c = cardThing c & location .~ (Just $ loc i)

registerTrick :: Trick -> GuiWorld -> GuiWorld
registerTrick trick world =
    S.foldrWithIndex rgstr world trick 
    where rgstr i c w = w & zones . playZone %~ manage (crd i c)
          loc i = Location (-351+ 55*(fromIntegral i), 200 ) (80,60)
          crd i c = cardThing c & location .~ (Just $ loc i)

cardThing :: Card -> Thing
cardThing card 
    = 
    let cid = _id card
        click w = case extract (w ^. zones . handZone) cid of
                    Just cd -> w & zones . draggingZone %~ manage cd
                    Nothing -> w
        c   = Clickable cid $ return . click
        s   = Sprite (renderCard card)
    in Thing (Just c) Nothing (Just card) (Just s) Nothing cid


{-registerButton :: Location -> Sprite -> Trigger -> GuiWorld -> GuiWorld-}
{--- registerButton = undefined -- needs to actually register button-}

{- Need to display scores, show cards in trick in appropriate place
 - indicate opponents hands, tricks taken -}
drawWorld :: GuiWorld -> IO Picture
drawWorld world
    = do
    let debugInfo = world ^. miscState . dbgInfo
        dbg = {-Color rose $-} Translate (-200) (150) $ scale (0.225) (0.225) $ text $ unlines $ take 4 debugInfo
    -- will also want to render in depth order
    return $! Pictures [ dbg
                      , renderZones world
                      ]

-- Do I need to:
--     Clip pics to zone bounding box
renderZones :: GuiWorld -> Picture
renderZones world = 
    let -- zs = world ^. zones . extraZones -- change to traversal? 
        {-combine ((x,y), (Sprite spr )) = Translate x y spr
        process z = map combine $ render z-}
        hz = world ^. zones . handZone
        go = world ^. zones . guiObjects
        pz = world ^. zones . playZone
        dz = world ^. zones . draggingZone
    in
    -- can make this nicer by making zonelist traversable or foldable
    Pictures $ (render dz):(map render [go,hz,pz]) --   -- ++ (concatMap process zs)

renderCard :: Card -> Picture
renderCard card
    = Pictures
        [ suitColor $ rectangleSolid ( 60  ) ( 80 )
        , Color       ( greyN 0.575 )  $ circleSolid 20
        , Color black $ Translate      ( -10 ) ( -5 ) $ Scale ( 0.125) ( 0.125) $ Text $ pretty card
        , Color white $ rectangleWire  ( 60  ) ( 80 )
        ]
        where suitColor = 
                case _suit card of
                    Clubs -> Color black
                    Spades -> Color black
                    Hearts -> Color red
                    Diamonds -> Color red

renderThing :: Thing -> Picture
renderThing t = fromMaybe Blank $ do
        (Sprite spr) <- t ^. sprite
        loc <- t ^. location
        let (tx,ty) = (loc ^. pos)
        return $! Translate tx ty spr
    
playArea :: Thing
playArea = Thing 
            Nothing 
            (Just $ Target playAreaHandleRelease) 
            Nothing 
            (Just $ Sprite (Color (makeColor 0.2 0.2 0.2 0.5) $ rectangleSolid (400) (300)) )
            (Just $ Location (0,0) (400,300))
            (-5)

gameWindow :: Thing
gameWindow = Thing
            Nothing 
            (Just $ Target gameWindowHandleRelease)
            Nothing 
            (Just $ Sprite (Color (makeColor 0.2 0.6 0.2 0.2) $ rectangleSolid (600) (300)) )
            (Just $ Location (0,0) (800,600))
            (-10)

handBackground :: Thing
handBackground = Thing
                Nothing
                Nothing
                Nothing
                (Just $ Sprite (Color (makeColor 0.4 0.7 0.2 0.5) $ rectangleSolid (800) (100)) )
                (Just $ Location ((0,-200)) (800,100))
                (-42)

playAreaHandleRelease :: Trigger
playAreaHandleRelease world 
    = return $! 
        case extract (world ^. zones . draggingZone) (error "shouldn't check id")  of 
            Just t -> processCardRelease t world
            Nothing -> world

processCardRelease :: Thing -> GuiWorld -> GuiWorld
processCardRelease thing world =
        let card = fromMaybe (error "object does not contain card. ") $ thing ^. object
            -- (mx,my) = world ^. miscState . mouseCoords 
        in
        case (world ^. miscState . current) of
            CollectPass soFar ->
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
                    & zones . draggingZone %~ clean
                    & miscState   . dbgInfo   %~ ("Passing this card. ":)
                    -- take card out of handZone
                    & zones . handZone %~ remove (thing ^. objId)
                    & zones . playZone %~ manage thing
                else world 
                    & miscState   . dbgInfo   %~ ("cannot add card to passing set ":)

            SelectCard hand info  ->
                if isValidPlay hand info card
                then world
                        & zones . handZone %~ remove (thing ^. objId)
                        & zones . playZone %~ manage thing
                        & miscState   . current   .~ SendCard card
                else world 
                        & miscState   . dbgInfo   %~ ("Not valid play. ":)

            Waiting -> world

            s       -> error $ "dragging thing " ++ (show card) ++ " while in state " ++ show s

gameWindowHandleRelease :: Trigger
gameWindowHandleRelease world = 
    return $! world & zones . draggingZone %~ clean
 
emptyWorld :: Int -> Supply -> GuiWorld
emptyWorld p sup = GuiWorld (MiscState RenderEmpty ["Initializing"] p (0,0) Nothing Initializing sup) defaultZL

defaultZL :: ZoneList
defaultZL = ZoneList
    { _handZone = new
    , _playZone = new
    , _draggingZone = new
    , _guiObjects = new & manage playArea & manage gameWindow & manage handBackground
    -- , _extraZones = []
    }

