module HeartsCommon
    ( -- module PlayingCards
      HeartsCommon.Card
    , Hand
    , orderPile
    , shuffledDeck
    , unorderPile
    , drawExactly
    , trickWinner
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
import PlayingCards (Suit(..), Card(..))
import qualified PlayingCards as P
import Data.Sequence (Seq)
import Data.Set (Set)
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
    , playedSoFar     :: P.Trick
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

type Card = (Int, P.Card)
type Hand = Set HeartsCommon.Card
-- type Trick = P.Trick
type Trick = Seq HeartsCommon.Card

orderPile :: Hand -> Trick
orderPile = undefined
-- P.orderPile . convertHand
-- unorderPile :: P.OrdPile -> P.Pile
unorderPile = P.unorderPile

-- need a liftCard type function
-- not quite sure how to go about it
-- so that it takes a function that operates on cards
-- and instead operates on HCards

trickWinner :: P.Trick -> Maybe Suit -> Int
trickWinner = P.trickWinner

drawExactly :: Int -> P.Trick -> Maybe (P.Trick, P.Trick)
drawExactly = P.drawExactly

shuffledDeck :: IO [P.Card]
shuffledDeck = P.shuffledDeck

-- This seems like an ideal thing to practice using quickCheck with
-- namely, no matter what the trick is, should always have at least one valid play
isValidPlay' :: P.Hand -> Info -> P.Card -> Bool
isValidPlay' hand info card =
    let played          = playedSoFar info
        playIf p        = p card || F.all (not . p) hand
        on_lead         = S.null played
        isFirstTrick    = is2c $ S.index played 0
        matchesLead c   = _suit c == _suit (S.index played 0)
        is2c c          = c       == Card Clubs 2
        isGarbage c     = _suit c == Hearts || c == Card Spades 12
    in
    -- Note that at the moment, you can't lead the QS if hearts hasn't been broken
    playIf is2c &&
        if on_lead
        then (not . isGarbage) card || heartsBroken info ||  F.all isGarbage hand
        else playIf matchesLead && not (isGarbage card && isFirstTrick)


convertHand :: HeartsCommon.Hand -> P.Hand
convertHand = undefined
-- convertTrick :: HeartsCommon.Trick -> P.Trick
-- convertTrick = undefined

isValidPlay :: HeartsCommon.Hand -> Info -> HeartsCommon.Card -> Bool
isValidPlay hand info card =
    isValidPlay' (convertHand hand) info (snd card)
----
-- communication related

-- data Message = ClientToServer | ServerToClient
data ClientToServer = CtsMove HeartsCommon.Card
                    | CtsPassSelection (Set HeartsCommon.Card)
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
                | RenderInRound HeartsCommon.Hand Trick Scores
                | RenderEmpty
                | Canonical Mode [HeartsCommon.Card] [String]

-- rename eventually to rendermode after figuring out name conflicts
-- eventually mode will store passing/betweenrounds/renderinround/etc
data Mode = ObjectList
