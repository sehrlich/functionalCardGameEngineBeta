{-# LANGUAGE ViewPatterns, PatternSynonyms #-} -- for pattern matching on sequences
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

-- consider trying
-- type ZoneList = [MonadReader (Zone Thing)]
-- or possibly
-- type ZoneList = [MonadState (Zone Thing)]
data ZoneList = ZoneList
    { _handZone :: OrderArrangedZone Thing -- ExactZone Thing -- managedzone
    , _playZone :: OrderArrangedZone Thing -- ExactZone Thing -- managedzone
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
    , _timeSoFar    :: Float
    }
    -- may need to register current effect seeking target
    
    -- _animation  --- collect drag and server generated animations
    -- these should be appropriate zones

type DebugInfo = [String]
type Bbox          = (Float, Float)
type Pos           = (Float, Float)
data Sprite        = Sprite Picture -- RenderProcess if we need io to render?
-- Locations should be understood to be at the center of the image
-- data Location      = Location { _pos::Pos, _bbox::Bbox} -- will want vector stuff to handle/change locations

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

data GuiThing o = Thing
    { _action :: Maybe Clickable
    , _reaction :: Maybe Target
    , _object :: Maybe o
    , _sprite :: Maybe Sprite
    , _location :: Maybe Pos
    , _bbox :: Maybe Bbox
    , _objId :: Int
    }
type Thing = GuiThing Card

instance IDable Thing where
    getID = _objId

instance Eq (GuiThing o) where
    (==) a b = _objId a == _objId b

instance Show Thing where
    show (Thing _ _ _o _ _l _b _i) =
        case _o of
            Nothing -> ""
            Just c -> pretty c ++ " at loc " ++ 
                case _l of 
                    Nothing -> "no location!\n"
                    Just l -> show l


-- these are at the bottom for template haskell nonsense reasons
-- makeLenses ''Location
makeLenses ''GuiWorld
makeLenses ''MiscState
makeLenses ''ZoneList
makeLenses ''GuiThing


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
            box <- (t ^. bbox)
            if isInRegion p loc box
            then Just t 
            else Nothing

isInRegion :: (Float, Float) -> Pos -> Bbox -> Bool
isInRegion (mx,my) ((cx,cy)) (bx,by) =
       cx -  bx /2 <= mx
    && mx <= cx +     bx /2
    && cy -  by /2 <= my
    && my <= cy    +  by /2

updateZones :: Float -> GuiWorld -> GuiWorld
updateZones dt w =
    w & zones . handZone %~ rePlace (w ^. zones . handZone . noElements)
      & zones . playZone %~ rePlace 4
      & miscState . timeSoFar +~ dt


-- also, (as in guiZone) perhaps this should by a Positioner p i, where i is the
-- type of some needed info, occasionally the world, or the card or nothing (as 
-- in this case)
-- under this change (Positioner p i) is an instance of MonadReader ___ (Loc)?
-- TODO make this less than oh-so-ugly
-- possibly by doing an indexed traversal of elems using modlist
rePlace :: Int -> OrderArrangedZone Thing -> OrderArrangedZone Thing
rePlace n z =
    let strat = z ^. strategy
        modList l = [ modThing i t |(i,t)<- zip [0..] l]
        modThing :: Int -> Thing -> Thing
        modThing i t = t & location .~ placeBy strat i n
    in z & elems %~ modList
    -- alternative idea : z & elems . itraverse %%@~ imap modThing

-- This should get folded into update / rePlace
-- an instance where it would be nice to have a lens into zones
updatePos :: Pos -> SingletonZone Thing -> SingletonZone Thing
updatePos p (SingletonZone z) = SingletonZone $ do
        t <- z
        -- loc <- t ^. location
        return $! t & location .~ Just p

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
            timeHandle -- time update
    where window = (InWindow
                   "Gloss" 	    -- window title
                                -- title fixed for xmonad
                   (800, 600)   -- window size
                   (0, 0)) 	-- window position
          timeHandle dt w = do
                commHandle inbox outbox $ updateZones dt w

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

commHandle :: TMVar ServerToClient -> TMVar ClientToServer -> GuiWorld -> IO GuiWorld
commHandle inbox outbox world
    = do
    messageReceived <- atomically $ tryTakeTMVar inbox
    let world'      = processMessage messageReceived world
    let outMessage  = world' ^. miscState . toSend
    maybe           (return ()) (atomically . (putTMVar outbox)) outMessage
    return          $ world' & miscState . toSend .~ Nothing
                    -- & miscState . timeSoFar +~ t

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
    -- ^ Need to put cards into hand ordered
    Just (StcGetMove hand info)    -> world        & miscState . current .~ (SelectCard hand info)
    Just (StcRender rinfo )        -> registerWorld rinfo acknowledged  -- soon we'll just register the trick?
    Just StcCleanTrick             -> cleaned
    Nothing ->
        case world ^. miscState . current of
            -- consider using prism to (re)move Just
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
    F.foldr rgstr world (orderPile hand)
    where rgstr c w = w & zones . handZone %~ manage (cardThing c)

registerTrick :: DecTrick -> GuiWorld -> GuiWorld
registerTrick trick world =
    F.foldr rgstr w'' trick 
    where rgstr c w = w & zones . playZone %~ manage (cardThing $ fst c)
          off = -pi/2 + case trick of
                (h :< _) -> fromIntegral (snd h - 1) * pi /2
                _ -> 0
          w'' = world & zones . playZone . strategy .~ arcBetween off (off-1.5*pi) (0,0) 100

cardThing :: Card -> Thing
cardThing card 
    = 
    let cid = _id card
        click w = case extract (w ^. zones . handZone) cid of
                    Just cd -> w & zones . draggingZone %~ manage cd
                    Nothing -> w
        c   = Clickable cid $ return . click
        s   = Sprite (renderCard card)

    in Thing (Just c) Nothing (Just card) (Just s) Nothing (Just (80,60)) cid


{-registerButton :: Location -> Sprite -> Trigger -> GuiWorld -> GuiWorld-}
{--- registerButton = undefined -- needs to actually register button-}

{- Need to display scores, show cards in trick in appropriate place
 - indicate opponents hands, tricks taken -}
drawWorld :: GuiWorld -> IO Picture
drawWorld world
    = do
    let debugInfo = world ^. miscState . dbgInfo
        dbg = {-Color rose $-} Translate (-200) (150) $ scale (0.225) (0.225) $ text $ unlines $ take 4 debugInfo
        time = Translate (200) 200 $ scale 0.2 0.2 $ text $ take 5 $ show $ world ^. miscState . timeSoFar
    -- will also want to render in depth order
    return $! Pictures 
            [ dbg
            , time            
            , renderZones world
            ]

-- Do I need to:
--     Clip pics to zones bounding box
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
    Pictures $ render dz : render go : render hz : render pz : []--   -- ++ (concatMap process zs)

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
                    Clubs -> Color $ light $ light black
                    Spades -> Color black
                    Hearts -> Color red
                    Diamonds -> Color $ light red

renderThing :: Thing -> Picture
renderThing t = fromMaybe Blank $ do
        (Sprite spr) <- t ^. sprite
        (tx, ty) <- t ^. location
        -- (bx, by) <- t ^. bbox
        -- should be using the bounding box to offset it so that it is displayed at center
        return $! Translate tx ty spr
        -- return $! Translate (tx - bx/2) (ty - by/2) spr
    
playArea :: Thing
playArea = Thing 
            Nothing 
            (Just $ Target playAreaHandleRelease) 
            Nothing 
            (Just $ Sprite (Color (makeColor 0.2 0.2 0.2 0.5) $ rectangleSolid (400) (300)) )
            (Just (0,0))
            (Just (400,300))
            (-5)

gameWindow :: Thing
gameWindow = Thing
            Nothing 
            (Just $ Target gameWindowHandleRelease)
            Nothing 
            (Just $ Sprite (Color (makeColor 0.2 0.6 0.2 0.2) $ rectangleSolid (600) (300)) )
            (Just (0,0))
            (Just (800,600))
            (-10)

handBackground :: Thing
handBackground = Thing
                Nothing
                Nothing
                Nothing
                (Just $ Sprite (Color (makeColor 0.4 0.7 0.2 0.5) $ rectangleSolid (800) (100)) )
                (Just ((0,-200)))
                (Just (800,100))
                (-42)

playAreaHandleRelease :: Trigger
playAreaHandleRelease world 
    = return $! 
        case (world ^. zones . draggingZone) of 
            SingletonZone (Just t) -> processCardRelease t world
            SingletonZone Nothing -> world

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
emptyWorld p sup = GuiWorld (MiscState RenderEmpty ["Initializing"] p (0,0) Nothing Initializing sup 0.0) defaultZL

defaultZL :: ZoneList
defaultZL = ZoneList
    { _handZone = initialize $ linearBetween (400,-200) (-300, -200)
    , _playZone = initialize $ linearBetween (100,0) (-100, 0)-- arcBetween (-0.5 * pi) pi (0,0) 100 
    , _draggingZone = new
    , _guiObjects = new & manage playArea & manage gameWindow & manage handBackground
    -- , _extraZones = []
    }

-- Patterns go at end of file since hlint can't parse them
-- pattern Empty   <- (S.viewl -> S.EmptyL)
pattern x :< xs <- (S.viewl -> x S.:< xs)
-- pattern xs :> x <- (S.viewr -> xs S.:> x)
