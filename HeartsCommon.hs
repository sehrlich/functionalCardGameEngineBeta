module HeartsCommon
    ( -- module PlayingCards
      Card
    , Hand
    , orderPile
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
import PlayingCards (Card(..), Hand, Trick, Suit(..))
import PlayingCards as P
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

--orderPile = P.orderPile
-- This seems like an ideal thing to practice using quickCheck with
-- namely, no matter what the trick is, should always have at least one valid play
isValidPlay :: Hand -> Info -> Card -> Bool
isValidPlay hand info card =
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
                | Passing Hand PassDir
                | BetweenRounds Scores
                | RenderInRound Hand Trick Scores
                | RenderEmpty
                | Canonical Mode [(Int,Card)] [String]

-- rename eventually to rendermode after figuring out name conflicts
-- eventually mode will store passing/betweenrounds/renderinround/etc
data Mode = ObjectList
