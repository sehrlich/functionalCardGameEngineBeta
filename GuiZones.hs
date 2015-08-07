{-# LANGUAGE DeriveFoldable #-}

module GuiZones 
    ( Zone(..)
    , IDable(..)
    -- Specific types of zones
    , ExactZone(..)
    , SingletonZone(..)
    )
    where
import Data.Foldable
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
-- import Data.Maybe

-- consider making an appropriate lens for zones

newtype ExactZone t = ExactZone {_store :: IntMap t} deriving (Foldable)
newtype SingletonZone t = SingletonZone (Maybe t) deriving (Foldable)

class IDable t where
    getID :: t -> Int

class Foldable z => Zone z where
    clean   :: z t      -> z t
    manage  :: IDable t => t   -> z t -> z t
    extract :: IDable t => z t -> Int -> Maybe t
    remove  :: Int      -> z t -> z t
    new     :: z t
    -- needs to take in world info, maybe?
    -- so should probably be defined as a subclass in whatever gui
    -- update :: z t -> z t
    -- update = id
    -- consider
    -- filter :: z t -> (t -> Bool) -> [t]
    -- with default implementation from foldable

instance Zone ExactZone where
    clean _z                  = ExactZone $ IntMap.empty
    manage t (ExactZone z)    = ExactZone $ IntMap.insert (getID t) t z
    extract  (ExactZone z) i  = IntMap.lookup i z
    remove i (ExactZone z)    = ExactZone $ IntMap.delete i z
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
    new         = SingletonZone $ Nothing

-- There has got to be a better way to do this...
-- right now it doesn't seem needed though
{-instance HZone AllZones where
    long list of matching on a variant
-}
