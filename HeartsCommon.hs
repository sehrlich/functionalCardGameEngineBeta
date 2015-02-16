module HeartsCommon
    ( UZone
    , Info(..)
    , Effect(..)
    , PassDir(..)
    , World(..)
    -- type synonyms
    , Scores
    , PlayerID
    , Board
    -- Game Logic
    , isValidPlay
    -- Communication Related
    , Message(..)
    , ClientToServer(..)
    , ServerToClient(..)
    ) where 
import PlayingCards
import qualified Data.Set as Z
import qualified Data.Sequence as S
import qualified Data.Foldable as F

type UZone = Z.Set Card

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
type Scores = S.Seq Int
type PlayerID = Int
--type Trick = S.Seq (Card, PlayerID)
type Board = S.Seq UZone
data Message = ClientToServer | ServerToClient -- deriving (Generic, Typeable)

data ClientToServer = CtsMove Card 
                    | CtsPassSelection (Z.Set Card)
                    | CtsDisconnect
data ServerToClient = StcGetMove UZone Info 
                    | StcGetPassSelection UZone PassDir
                    | StcGameOver

-- This seems like an ideal thing to practice using quickCheck with
-- namely, no matter what the trick is, should always have at least one valid play
isValidPlay :: UZone -> Info -> Card -> Bool
isValidPlay hand _info@(TrickInfo _ played _ heartsBroken) card =
    let checkHandHasNo p    = not $ Z.foldr ((||).p) False hand
        playIf p            = p card || checkHandHasNo p
        on_lead         = S.null played
        isFirstTrick    = is2c $ S.index played 0
        matchesLead c   = _suit c == _suit (S.index played 0)
        is2c c          = c == Card {_suit = Clubs, _rank = 2}
        isGarbage c     = _suit c == Hearts || c == Card {_suit = Spades, _rank = 12}
    in
    playIf is2c && 
        if on_lead 
        then not (isGarbage card || heartsBroken || checkHandHasNo (not . isGarbage))
        else playIf matchesLead && not (isGarbage card && isFirstTrick)
    -- note: counting on lazy evaluation to not evaluate matches_lead if played is empty
    -- this runs into a problem when played is null, hearts are not yet broken, and someone
    -- tries to lead hearts
    -- playIf is2c && 
    -- (
    --     (on_lead 
    --     && (not $ isGarbage card || heartsBroken || checkHandHasNo isGarbage)
    --     )
    -- || 
    --     (
    --     not on_lead
    --     && playIf matches_lead 
    --     && not (isGarbage card && isFirstTrick)
    --     )
    -- )


