module HeartsCommon
    ( Info(..)
    , Effect(..)
    , PassDir(..)
    , World(..)
    -- type synonyms
    , Scores
    , PlayerID
    , Board
    -- Communication Related
    , Message(..)
    , ClientToServer(..)
    , ServerToClient(..)
    -- Game Logic (client can access)
    , isValidPlay
    ) where
import PlayingCards
import Data.Set (Set)
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import qualified Data.Foldable as F

data Effect = Effect (World -> World)
                | GetInput
                | NewTrick
                | ComputeWinner

data PassDir = PassLeft | PassRight | PassAcross | NoPass deriving (Eq)

-- curPlayer, played so far, scores this round, hearts broken
data Info = TrickInfo PlayerID Trick Scores Bool
data World = InRound Board Stack Info
            | StartGame
            | StartRound PassDir Scores
            | PassingPhase Board PassDir
            | RoundOver Scores
            | GameOver Scores
type Stack = [Effect]
type Scores = Seq Int
type PlayerID = Int
type Board = Seq Hand

data Message = ClientToServer | ServerToClient
data ClientToServer = CtsMove Card
                    | CtsPassSelection (Set Card)
                    | CtsDisconnect
                    | CtsAcknowledge

data ServerToClient = StcGetMove Hand Info
                    | StcGetPassSelection Hand PassDir
                    | StcGameStart
                    | StcGameOver

-- This seems like an ideal thing to practice using quickCheck with
-- namely, no matter what the trick is, should always have at least one valid play
isValidPlay :: Hand -> Info -> Card -> Bool
isValidPlay hand _info@(TrickInfo _ played _ heartsBroken) card =
    let playIf p        = p card || F.all (not . p) hand
        on_lead         = S.null played
        isFirstTrick    = is2c $ S.index played 0
        matchesLead c   = _suit c == _suit (S.index played 0)
        is2c c          = c == Card {_suit = Clubs, _rank = 2}
        isGarbage c     = _suit c == Hearts || c == Card {_suit = Spades, _rank = 12}
    in
    -- Note that at the moment, you can't lead the QS if hearts hasn't been broken
    playIf is2c &&
        if on_lead
        then (not . isGarbage) card || heartsBroken ||  F.all isGarbage hand
        else playIf matchesLead && not (isGarbage card && isFirstTrick)
