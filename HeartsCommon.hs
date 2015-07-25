module HeartsCommon
    ( -- module PlayingCards
      PlayingCard(..)
    , Card(..)
    , Trick
    , Hand
    , shuffledDeck
    , P.matches
    , orderPile
    , unorderPile
    , P.drawExactly
    , computeWinner
    , Suit(..)
    ----
    , Info(..)
    , Effect(..)
    , PassDir(..)
    , World(..)
    -- type synonyms
    , Scores
    , PlayerID
    , Board
    -- Game Logic (client can access)
    , isValidPlay
    -- Communication interfaces
    -- , Message(..)
    , ServerToClient(..)
    , ClientToServer(..)
    , RenderInfo(..)
    , Mode(..)
    ) where
import PlayingCards (Suit(..), PlayingCard(..))
import qualified PlayingCards as P
import Data.Sequence (Seq)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Sequence as S
import qualified Data.Foldable as F

data Effect = Effect (World -> World)
                | GetInput
                | NewTrick
                | ComputeWinner

data PassDir = PassLeft
                | PassRight
                | PassAcross
                | NoPass
                deriving (Eq, Show)

data Info =
    TrickInfo
    { curPlayer       :: PlayerID
    , playedSoFar     :: Trick
    , pointsCollected :: Scores
    , heartsBroken    :: Bool
    }

data World = InRound Board Stack Info
            | StartGame
            | StartRound PassDir Scores
            | PassingPhase Board PassDir
            | RoundOver Scores
            | GameOver Scores
type Stack    = [Effect]
type Scores   = Seq Int
type PlayerID = Int
type Board    = Seq Hand

data Card = Card
    { _suit  :: Suit
    , _rank  :: Int
    , _id    :: Int
    } deriving (Show, Eq, Ord)

instance PlayingCard Card where
    suit = _suit
    rank = _rank

type Hand = Set Card
type Trick = Seq Card

orderPile :: Hand -> Trick
orderPile pile = S.fromList $ F.toList pile
unorderPile :: Trick -> Hand
unorderPile pile = Set.fromList $ F.toList pile

computeWinner :: Info -> (PlayerID, Scores, Bool)
computeWinner (TrickInfo started played scores broken) =
    let winner = (P.trickWinner played Nothing + started) `mod` 4
        pts (Card s r _) | s==Hearts = 1
                         | r==12 && s==Spades = 13
                         | otherwise = 0
        trickVal    = F.sum $ fmap pts played
        new_scores  = S.adjust (+ trickVal) winner scores
        isHeart c   = _suit c == Hearts
        broken'     = broken || F.foldr ((||).isHeart) False played
    in
        (winner, new_scores, broken')

shuffledDeck :: IO [Card] -- should take in idsupply
shuffledDeck = do
    cards <- P.shuffledDeck
    ids <- return $ [100,102..] -- FIXME should be drawn from idsupply
    return $ zipWith makeCard ids cards
        where makeCard i c = Card (P.suit c) (P.rank c) i

-- This seems like an ideal thing to practice using quickCheck with
-- namely, no matter what the trick is, should always have at least one valid play
isValidPlay :: Hand -> Info -> Card -> Bool
isValidPlay hand info card =
    let played          = playedSoFar info
        playIf p        = p card || F.all (not . p) hand
        on_lead         = S.null played
        isFirstTrick    = is2c $ S.index played 0
        matchesLead c   = _suit c == _suit (S.index played 0)
        is2c c          = P.matches 2 Clubs c
        isGarbage c     = _suit c == Hearts || P.matches 12 Spades c
    in
    -- Note that at the moment, you can't lead the QS if hearts hasn't been broken
    playIf is2c &&
        if on_lead
        then (not . isGarbage) card || heartsBroken info ||  F.all isGarbage hand
        else playIf matchesLead && not (isGarbage card && isFirstTrick)


----
-- communication related

-- data Message = ClientToServer | ServerToClient
data ClientToServer = CtsMove Card
                    | CtsPassSelection (Set Card)
                    | CtsDisconnect
                    | CtsAcknowledge

data ServerToClient = StcGetMove Hand Info
                    | StcGetPassSelection Hand PassDir
                    | StcGameStart Int
                    | StcGameOver
                    | StcRender RenderInfo

data RenderInfo = RenderServerState Board Info
                | Passing HeartsCommon.Hand PassDir
                | BetweenRounds Scores
                | RenderInRound HeartsCommon.Hand HeartsCommon.Trick Scores
                | RenderEmpty
                | Canonical Mode [Card] [String]

-- rename eventually to rendermode after figuring out name conflicts
-- eventually mode will store passing/betweenrounds/renderinround/etc
data Mode = ObjectList
