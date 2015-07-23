module PlayingCards
    ( -- Types
      Suit(..)
    , PlayingCard(..)
    -- Utility
    , matches
    -- IO
    , prettySym
    , readCard     -- | Interpret a two character string as a card
    -- deck
    , shuffledDeck -- | Provides a shuffled standard poker deck
    -- , draw
    , drawExactly
    -- , stdDeck
    -- , shuffle
    -- trick taking utilities
    , followsSuit
    , trickWinner
    ) where
import Data.List (intercalate)
import Data.Function (on)
import Control.Monad (forM)
import Data.Array.IO
import System.Random

import qualified Data.Foldable as F
import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
import Data.Set (Set)

{- TODO:
 - we want to make HCards be playing cards with ID
 - 
 - In an OO framework, the natural thing to do would be to make HCard a subtype of Card
 - But we don't do that
 - Instead we'll make Card a typeclass (along with a default instantiation for testing)
 - and update all our functions appropriately
 - 
 - Then HCard is just a new type of this typeclass, and everything will automatically work
 -
 -
 -}

-- one line of thinking is that Card should have Show and Eq with default implementations based off suit and rank. Instead, we're just going to add prettyprint to card (with no colors)
class (Show c) => PlayingCard c where
   suit   :: c -> Suit
   rank   :: c -> Int
   pretty :: c -> String
   pretty c =  ("-A23456789TJQKA"!!(rank c)) : (head . show $ suit c) : ""

data Suit = Clubs | Hearts | Spades | Diamonds deriving (Eq, Show, Ord)
data TestCard = TCard {_suit::Suit, _rank::Int} deriving (Eq, Ord) -- maybe later (Generic, Typeable) --Show
-- type Trick   = Seq Card -- ordered
-- type OrdPile = Seq Card -- ordered
-- type Pile = Set Card -- unordered
-- type Hand = Pile
instance PlayingCard TestCard where
    suit = _suit
    rank = _rank

instance Show TestCard
    where show (TCard s r) = ("-A23456789TJQKA"!!r) : (head $ show s) : ""

prettySym :: PlayingCard c => c -> String
prettySym c =
    let (col,pic) = case (suit c) of
            Clubs       -> ([1,30,47], "♣")
            Spades      -> ([1,30,47], "♠")
            Hearts      -> ([1,31,47], "♥")
            Diamonds    -> ([1,31,47], "♦")
    in colorize col $ ("-A23456789TJQKA"!! (rank c)) : pic

readSuit :: Char -> Suit
readSuit s = case s of
        'c' -> Clubs
        'C' -> Clubs
        'd' -> Diamonds
        'D' -> Diamonds
        'h' -> Hearts
        'H' -> Hearts
        's' -> Spades
        'S' -> Spades
        _ -> error "Unrecognized suit"

readRank :: Char -> Int
readRank r
        | r=='A' = 14
        | r=='a' = 14
        | r=='K' = 13
        | r=='k' = 13
        | r=='Q' = 12
        | r=='q' = 12
        | r=='J' = 11
        | r=='j' = 11
        | r=='T' = 10
        | r=='t' = 10
        | r `elem` "23456789" = read [r] ::Int
        | otherwise = 0
        -- temporary thing should correspond to card not in hand

readCard :: String -> Maybe TestCard
readCard [r,s] = Just (TCard (readSuit s) (readRank r))
readCard _     = Nothing

colorize :: [Int] -> String -> String
colorize options str = "\ESC["
                        ++ intercalate ";" [show i | i <-options]
                        ++ "m" ++ str ++ "\ESC[0m"

_cardback :: String
_cardback = colorize [104] "()"


{-[>-| Returns all cards that are playable in this trick -<]-}
{-playableCards :: Hand -> Trick -> Hand-}
{-playableCards hand trick =-}
    {-if Seq.null trick-}
    {-then hand-}
    {-else-}
       {-let matchesLead c   = _suit c == _suit (Seq.index trick 0)-}
       {-in-}
       {-if F.any matchesLead hand-}
       {-then Set.filter matchesLead hand-}
       {-else hand-}

{--| Checks that the card played follows suit if able --}

followsSuit ::PlayingCard c => Set c -> Seq c -> c -> Bool
followsSuit hand played card =
    let on_lead         = Seq.null played
        matchesLead c   = suit c == suit (Seq.index played 0)
    in
    on_lead || matchesLead card || F.all (not . matchesLead) hand

{--| Computes the index of the card that won the trick (maybe trump)
 - Note that this is relative to the first player in the trick
 - --}
trickWinner :: PlayingCard c => Seq c -> Maybe Suit -> Int
trickWinner played trump =
    let lead_suit = suit $ Seq.index played 0
        (winner, _best_card_val) = F.maximumBy (compare `on` snd) $ flip Seq.mapWithIndex played $ (. (cardVal lead_suit trump)) . (,)
    in
    winner
    where cardVal lead maybeTrump c
             = let s = suit c
                   r = rank c
                in 
                r + (if s == lead then 15 else 0)
                + (if Just s == maybeTrump then 50 else 0)

matches :: PlayingCard c => Int -> Suit -> c -> Bool
matches r s c = (suit c == s) && (rank c == r)

--  {--| Randomly draw n cards from pile (until pile is empty), return the drawn stack and the reduced pile --}
--  draw :: Int -> Pile -> (Pile, Pile)
--  draw _n _deck = undefined
--  --    if n <= length deck
--  --    then Just $ Seq.splitAt n deck
--  --    else Nothing

{--| Draw precisely n cards from pile and return the drawn stack and the reduced pile or fail with Nothing --}
drawExactly :: PlayingCard c => Int -> Seq c -> Maybe (Seq c, Seq c)
drawExactly n deck =
    if n <= Seq.length deck
    then Just $ Seq.splitAt n deck
    else Nothing

-- switch at some point to using RVars
shuffledDeck :: IO [TestCard]
shuffledDeck = shuffle stdDeck

stdDeck :: [TestCard]
---- setting aces at 14
stdDeck = [TCard s r | r <- [2..14], s <- [Clubs, Hearts, Spades, Diamonds]]

shuffle :: [a] -> IO [a]
-- shuffle x = return x
shuffle xs = do
        ar <- newArr n xs
        forM [1..n] $ \i -> do
            j  <- randomRIO (i,n)
            vi <- readArray ar i
            vj <- readArray ar j
            writeArray ar j vi
            return vj
  where
    n = length xs
    newArr :: Int -> [a] -> IO (IOArray Int a)
    newArr n' =  newListArray (1,n')

-- AI code
-- dodging
-- takes a hand, a trick (and maybe trump) and
-- selects the highest card it can play that won't win 
{-dodging :: Hand -> Trick -> Maybe Suit -> Card-}
{-dodging h t ms =-}
    {-if Seq.null t-}
    {-then Set.findMin h-}
    {-else-}
       {-let playables = playableCards h t-}
           {-curwinner = trickWinner t ms-}
           {-dodges = undefined-}
       {-in-}
       {-if F.any dodges playables-}
       {-then Set.findMax $ Set.filter dodges playables-}
       {-else Set.findMin playables-}
