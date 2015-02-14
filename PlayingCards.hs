module PlayingCards 
    ( -- Types
      Suit(..)
    , Card(..)
    -- Utility
    , readCard
    -- deck
    , stdDeck
    , shuffle
    -- trick taking utilities
    -- computeWinner
    -- followsLead
    ) where
import Data.List (intercalate)
import Control.Monad (forM)
import Data.Array.IO
import System.Random

data Suit = Clubs | Hearts | Spades | Diamonds deriving (Eq, Show, Ord)
data Card = Card {_suit::Suit, _rank::Int} deriving (Eq, Ord) -- maybe later (Generic, Typeable) --Show

instance Show Card 
    where show (Card s r) 
            = 
            let (col,pic) = case s of
                 Clubs       -> ([1,30,47], "C")
                 Spades      -> ([1,30,47], "S")
                 Hearts      -> ([1,31,47], "H")
                 Diamonds    -> ([1,31,47], "D")
            in colorize col $ ("-A23456789TJQKA"!!r) : pic

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

readCard :: String -> Maybe Card
readCard [r,s] = Just (Card (readSuit s) (readRank r))
readCard _     = Nothing

colorize :: [Int] -> String -> String
colorize options str = "\ESC[" 
                        ++ intercalate ";" [show i | i <-options] 
                        ++ "m" ++ str ++ "\ESC[0m"

_cardback :: String
_cardback = colorize [104] "()"

stdDeck :: [Card]
---- setting aces at 14
stdDeck = [Card s r | r <- [2..14], s <- [Clubs, Hearts, Spades, Diamonds]]

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
