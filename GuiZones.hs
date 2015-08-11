{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE TemplateHaskell #-}

module GuiZones 
    ( Zone(..)
    , SimpleZone(..)

    , IDable(..)
    -- Specific types of zones
    , ExactZone(..)
    , SingletonZone(..)
    , OrderArrangedZone(..)
    , initialize
    -- lenses
    , strategy
    , elems
    , noElements
    -- commonPositioners
    , PlacementStrategy(..)
    , linearBetween
    , arcBetween
    -- CirclularP
    --
    -- can do animation with time-varying positioners
    )
    where
import Data.Foldable
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap

import Data.Maybe (listToMaybe)
import Linear.Vector (lerp, (^+^), (*^))
import Linear.V2

import Control.Lens
-- consider making an appropriate lens for zones


newtype ExactZone t = ExactZone {_store :: IntMap t} deriving (Foldable)
newtype SingletonZone t = SingletonZone (Maybe t) deriving (Foldable)

-- this is better represented by MonadReader
class IDable t where
    getID :: t -> Int

class (Foldable z) => Zone z where
    clean   :: z t      -> z t
    manage  :: IDable t => t   -> z t -> z t
    extract :: IDable t => z t -> Int -> Maybe t
    remove  :: IDable t => Int -> z t -> z t
    -- needs to take in world info, maybe?
    -- so should probably be defined as a subclass in whatever gui
    -- there needs to be a middle ground for common context-aware zones
    -- update :: z t -> z t
    -- update = id
    --
    -- Also, should Zone supply a lens/traversal into the elements?

class (Zone z) => SimpleZone z where
    new     :: z t

instance Zone ExactZone where
    clean _z                  = ExactZone $ IntMap.empty
    manage t (ExactZone z)    = ExactZone $ IntMap.insert (getID t) t z
    extract  (ExactZone z) i  = IntMap.lookup i z
    remove i (ExactZone z)    = ExactZone $ IntMap.delete i z
instance SimpleZone ExactZone where
    new = ExactZone IntMap.empty

instance Zone SingletonZone where
    extract (SingletonZone z) i 
        = do
            t <- z
            if getID t == i
            then return t
            else Nothing
    manage t _z = SingletonZone $ Just t
    remove _i z = z
    clean (_)   = SingletonZone $ Nothing

instance SimpleZone SingletonZone where
    new       = SingletonZone $ Nothing

data PlacementStrategy = PlacementStrategy { placeBy :: Int -> Int -> Maybe (Float, Float) }

type V = V2 Float
convert :: V -> (Float, Float)
convert (V2 x y) = (x, y)
wrap :: (Float, Float) -> V
wrap (x, y) = V2 x y

linearBetween :: (Float, Float) -> (Float, Float) -> PlacementStrategy
linearBetween a b = PlacementStrategy $ place (wrap a) (wrap b)
    where place s e i n = 
            if n > i && i >= 0 -- need to deal with cases n==0,1 separately
            then Just $ convert (lerp (fromIntegral i / fromIntegral n) s e)
            else Nothing

arcBetween :: Float -> Float -> (Float, Float) -> Float -> PlacementStrategy
arcBetween a t c r = PlacementStrategy place 
    where place i n =
            if n > i && i >= 0 && n > 1-- need to deal with cases n==0,1 separately
            then Just $ convert $ 
                (wrap c) ^+^ r *^ angle ((fromIntegral i / fromIntegral (n-1)) * (t-a) + a)
            else Nothing

-- translate :: (Float, Float) -> PlacementStrategy -> PlacementStrategy
-- also want centered variants, etc.

-- if we use array, we store #elems implicitly and can access elems
data OrderArrangedZone t = OrderArrangedZone
    { _noElements :: Int -- must be positive
    , _strategy :: PlacementStrategy
    , _elems :: [t]
    } -- deriving Foldable
makeLenses ''OrderArrangedZone 

instance Foldable (OrderArrangedZone) where
    -- foldMap f (OrderArrangedZone _n _s e) = foldMap f e
    foldMap f z = foldMap f $ _elems z


instance Zone (OrderArrangedZone) where
    clean z       = z & elems .~ [] & noElements .~ 0
    manage t z    = if anyOf (elems . traverse) ((getID t ==) . getID ) z
                    then z
                    else z & elems %~  (t :) & noElements +~ 1
    extract  z i  = listToMaybe $ filter ((i ==) . getID) $ z ^. elems
    -- remove i z    = z & elems %~ filter (not . (i ==) . getID) & noElements -~ 1
    remove i z    = if noneOf (elems . traverse) ((i ==) . getID ) z
                    then z
                    else z & elems %~ filter (not . (i ==) . getID) & noElements -~ 1
    
initialize :: PlacementStrategy -> OrderArrangedZone t
initialize p = OrderArrangedZone 0 p []

{-
-- type L a = Lens' a (Maybe (Float,Float))
positionBy :: (Positioner p, Zone z, L l)=> l -> p -> z t -> z t
positionBy l p z = -- z -- undefined
    let strat = z ^. strategy
        -- modThing updates according to strat
        modList l = [ modThing i t |(i,t)<- zip [0..] l]
        modThing :: Int -> Thing -> Thing
        modThing i t = t & location .~ place p i (z ^. noElements) 
        _results = modList $ z^. elems
    --in if length results > 0 then error (show results) else z & elems %~ modList
    in z & elems %~ modList
    -- in z & elems . itraverse %%@~ imap modThing
-}

-- There has got to be a better way to do this...
-- right now it doesn't seem needed though
{-instance HZone AllZones where
    long list of matching on a variant
-}
