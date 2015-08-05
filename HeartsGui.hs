{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module HeartsGui
    ( guiThread
    )
    where

import HeartsCommon
import qualified Data.Set as Z
import qualified Data.Sequence as S
-- import Data.Foldable (Foldable)
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
-- import qualified Graphics.Gloss.Data.Color as C
import Graphics.Gloss.Interface.IO.Game --(playIO, Event(..) )

import Data.Maybe (catMaybes, maybeToList, listToMaybe, fromMaybe)
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
    { _handZone :: ExactZone -- managedzone
    , _playZone :: ExactZone -- managedzone
    , _draggingZone :: SingletonZone
    , _guiobjects :: ExactZone
    , _extraZones :: [AllZones]
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

{- Zones are data structures to handle and organize objects on the screen
 - they should support the following operations
 - query if objectid is handled by zone
 - return location/position of valid objectids for rendering
 - accept objectid to be handled
 - delete objectid (i.e. stop representing them)
 -
 - on the subject of having zones as typeclasses, check out
 - http://www.haskellforall.com/2012/05/scrap-your-type-classes.html
 - for an alternative
 -}
-- class Foldable z => HZone z where -- Switch to multiparameter type classes
class HZone z where -- Switch to multiparameter type classes
    extract    :: z -> Int -> Maybe Thing
    -- extract isn't needed if we're storing all the things in the thing warehouse?
    manage     :: Thing -> z -> z
    remove     :: Int -> z -> z
    clean      :: z -> z -- empties the zone
    allMems    :: z -> [Int] -- returns allMems
    checkPos   :: z -> Pos -> Maybe Int
    checkAll   :: z -> Pos -> [Int]
    checkAllT  :: z -> Pos -> [Thing]
    update     :: z -> z
    update = id
    --filterZ    :: z -> (Thing -> Bool) -> (Thing -> t) -> [t]
    -- checkPos returns the id of whatever is at that position
    -- it might make sense to allow it to return a (possibly empty) list instead
    -- ALSO it might be pretty to make this a lens, maybe with rename
    -- idsAtPos
    render     :: z -> [(Pos, Sprite)]

data ExactZone = ExactZone (IntMap Thing)
-- can I make this a newtype instead?
type SingletonZone          = Maybe Thing
                    -- | HandArea Pos
                    ---  Zone ManagementStyle Intmap Pos
data AllZones = HSingleton SingletonZone
              | HExact ExactZone
{-type ManagementStyle = GuiWorld -> Pos-}
{-
insertAtMousePos :: ManagementStyle
insertAtMousePos gw = _mouseCoords $ _miscState gw
insertNextPos :: ManagementStyle
insertNextPos = undefined
-}

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

makeLenses ''Location
makeLenses ''GuiWorld
makeLenses ''MiscState
makeLenses ''ZoneList
makeLenses ''Thing

instance HZone ExactZone where
    extract  (ExactZone z) i  = IntMap.lookup i z
    manage t (ExactZone z)    = ExactZone $ IntMap.insert (t ^. objId) t z
    remove i (ExactZone z)    = ExactZone $ IntMap.delete i z
    clean _z                  = ExactZone $ IntMap.empty
    allMems  (ExactZone z)    = IntMap.keys z
    checkAllT (ExactZone z) p =
        catMaybes [ checkP t | t <- IntMap.elems z]
        where checkP t = do
                        loc <- t ^. location 
                        if isInRegion p loc
                        then return $ t
                        else Nothing
    checkAll (ExactZone z) p  =
        catMaybes [ checkP t | t <- IntMap.elems z]
        where checkP t = do
                        loc <- t ^. location 
                        if isInRegion p loc
                        then return $ t ^. objId
                        else Nothing
    checkPos z p              = listToMaybe $ checkAll z p
    render   (ExactZone z)    = 
        catMaybes [ info t | t <- IntMap.elems z]
        where  info t = do
                        spr <- t ^. sprite
                        loc <- t ^. location
                        return $ (loc ^. pos, spr) 

instance HZone SingletonZone where
    extract       = const -- should check id matches
    manage t _z   = Just t
    remove _i z   = z
    clean (_)     = Nothing
    allMems _     = []
    checkAll z p  = maybeToList $ checkPos z p
    checkAllT t p = maybeToList $ do
        obj <- t
        loc <- _location obj
        if (isInRegion p loc)
        then Just $ obj
        else Nothing
    checkPos (t) cpos = do
        obj <- t
        loc <- _location obj
        if (isInRegion cpos loc)
        then Just $ obj ^. objId
        else Nothing
    render z = maybeToList $ do
        t <- z
        spr <- t ^. sprite
        loc <- t ^. location
        let p = loc ^. pos
        return (p, spr)
    -- should be able to simplify render by using prisms

updatePos :: Pos -> SingletonZone -> SingletonZone
updatePos p z = do
        t <- z
        loc <- t ^. location
        return $ t & location .~ (Just $ loc{_pos = p} )

-- There has got to be a better way to do this...
instance HZone AllZones where
    extract (HExact z) i = extract z i
    extract (HSingleton z) i = extract z i
    manage t (HExact z) = HExact $ manage t z
    manage t (HSingleton z) = HSingleton $ manage t z
    remove i (HExact z) = HExact $ remove i z 
    remove i (HSingleton z) = HSingleton $ remove i z 
    clean (HExact z) = HExact $ clean z 
    clean (HSingleton z) = HSingleton $ clean z 
    allMems (HExact z) = allMems z 
    allMems (HSingleton z) = allMems z 
    checkAllT (HExact z) p = checkAllT z p 
    checkAllT (HSingleton z) p = checkAllT z p 
    checkAll (HExact z) p = checkAll z p 
    checkAll (HSingleton z) p = checkAll z p 
    checkPos (HExact z) p = checkPos z p 
    checkPos (HSingleton z) p = checkPos z p 
    render (HSingleton z) = render z 
    render (HExact z) = render z 

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
    EventResize _ws -> return $ world
    EventMotion mpos --(mx,my)
        -> return $ world 
                    & miscState . mouseCoords  .~ mpos
                    & zones     . draggingZone %~ updatePos mpos
    EventKey k ks _mod mpos
        -> case (k, ks) of
            (MouseButton _, Down)
                -> do
                let h =  mpos & (checkAllT $ world ^. zones . handZone)
                let p =  mpos & (checkAllT $ world ^. zones . playZone)
                let go = mpos & (checkAllT $ world ^. zones . guiobjects)
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
                let go = mpos & (checkAllT $ world ^. zones . guiobjects)
                -- let ez = concatMap (flip checkAll mpos) $ world ^. zones . extraZones
                
                -- should make zones foldable
                let shouldGoOff = catMaybes $ map 
                                    ( ^. reaction) 
                                    (concat [h,p,go])
                                    -- (concat [h,p,go,ez])
                F.foldrM (releaseProcess) world shouldGoOff

            (SpecialKey KeyEsc, Up) 
                -> return $ world & miscState . toSend .~ Just CtsDisconnect
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
    messageReceived <- atomically $ tryTakeTMVar inbox
    let world'      = processMessage messageReceived world
    let outMessage  = world' ^. miscState . toSend
    maybe           (return ()) (atomically . (putTMVar outbox)) outMessage
    return          $ world' & miscState . toSend .~ Nothing

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
    return $ Pictures [ dbg
                      , renderZones world
                      ]

-- Do I need to:
--     Clip pics to zone bounding box
renderZones :: GuiWorld -> Picture
renderZones world = 
    let -- zs = world ^. zones . extraZones -- change to traversal? 
        combine ((x,y), (Sprite spr )) = Translate x y spr
        process z = map combine $ render z
        hz = world ^. zones . handZone
        go = world ^. zones . guiobjects
        pz = world ^. zones . playZone
        dz = world ^. zones . draggingZone
    in
    Pictures $ (concatMap process [go,hz,pz]) ++ (process dz) -- ++ (concatMap process zs)

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
    = return $ 
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
    return $ world & zones . draggingZone %~ clean
 
emptyWorld :: Int -> Supply -> GuiWorld
emptyWorld p sup = GuiWorld (MiscState RenderEmpty ["Initializing"] p (0,0) Nothing Initializing sup) defaultZL


emptyExactZone :: ExactZone
emptyExactZone = ExactZone IntMap.empty

defaultZL :: ZoneList
defaultZL = ZoneList
    { _handZone = emptyExactZone
    , _playZone = emptyExactZone
    , _draggingZone = Nothing
    , _guiobjects = emptyExactZone & manage playArea & manage gameWindow & manage handBackground
    , _extraZones = []
    }
