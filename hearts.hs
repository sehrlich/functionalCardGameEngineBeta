-- import qualified Data.Map.Strict as B -- for Zones
import qualified Data.Set as Z
import Data.Sequence ((|>), (<|), ViewR ((:>)), ViewL ((:<)))
import qualified Data.Sequence as S
import qualified Data.Foldable as F
import Control.Monad (forM, void) -- liftM, unless
import Data.Array.IO
import Data.Maybe (fromJust)
import Data.List (intercalate, maximumBy)
import System.Random
-- import Data.Vector 
-- consider replacing all list with sequences
-- import prelude as qualified
-- and importing all of sequence

type PlayerID = Int

data Suit = Clubs | Hearts | Spades | Diamonds deriving (Eq, Show, Ord)
data Card = Card 
            {_suit::Suit, _rank::Int} deriving (Eq, Ord) --Show

colorize :: [Int] -> String -> String
colorize options str = "\ESC[" 
                        ++ intercalate ";" [show i | i <-options] 
                        ++ "m" ++ str ++ "\ESC[0m"

_cardback :: String
_cardback = colorize [104] "()"

instance Show Card where
    show (Card s r) = 
                        let (col,pic) = case s of
                                    Clubs       -> ([1,30,47], "C")
                                    Spades      -> ([1,30,47], "S")
                                    Hearts      -> ([1,31,47], "H")
                                    Diamonds    -> ([1,31,47], "D")
                        in colorize col $ ("-A23456789TJQKA"!!r) : pic

type UZone = Z.Set Card

data Effect = Effect (World -> World) 
                | GetInput 
                | NewTrick

data PassDir = PassLeft | PassRight | PassAcross | NoPass deriving Eq
data Info = TrickInfo PlayerID [(Card,PlayerID)] Scores | FirstTrick PlayerID
data World = InRound Board Stack Info
            | StartGame 
            | StartRound PassDir Scores
            | PassingPhase Board PassDir
            | RoundOver Scores
            | GameOver Scores
type Stack = [Effect]
type Scores = S.Seq Int

type Board = S.Seq UZone

gameLoop :: World -> IO World
gameLoop world = 
            --world <- ioworld
            case world of
                -- for initialization
                StartGame -> 
                    -- get player names etc.
                    --
                    gameLoop $ StartRound PassLeft $ S.fromList [0,0,0,0]

                -- dataflow states, may not need to have them
                RoundOver scores -> do
                    putStrLn "Round Over"
                    -- check for shooting the moon
                    print scores
                    return $ RoundOver scores
                GameOver scores -> do
                    putStrLn "Game Over"
                    print scores
                    return $ GameOver scores

                -- World controlling events in a round
                StartRound passDir scores -> do
                    deck <- shuffle stdDeck
                    let h0 = Z.fromList $ take 13 deck
                    let h1 = Z.fromList $ take 13 $ drop 13 deck
                    let h2 = Z.fromList $ take 13 $ drop 26 deck
                    let h3 = Z.fromList $ take 13 $ drop 39 deck
                    let deal = S.fromList [h0,h1,h2,h3]
                    -- distribute deck to player hands

                    -- play round
                    RoundOver round_scores <- gameLoop $ PassingPhase deal passDir
                    
                    let new_scores = S.zipWith (+) round_scores scores
                    if checkScores new_scores then return $ GameOver new_scores
                    else gameLoop $ StartRound next_pass_dir new_scores
                    where checkScores = F.any (>100)
                          next_pass_dir = case passDir of 
                                        PassLeft    -> PassRight
                                        PassRight   -> PassAcross
                                        PassAcross  -> NoPass
                                        NoPass      -> PassLeft


                -- World when trying to pass
                PassingPhase deal passDir -> do
                    -- pass
                    board <- 
                        if passDir == NoPass 
                        then return deal
                        else 
                        let getSelection i = return Z.empty
                            rotate' (x :< xs) =  xs |> x
                            rotate = rotate' . S.viewl
                        in
                        do
                        -- rendering thing?
                        s0 <- getSelection 0
                        s1 <- getSelection 1
                        s2 <- getSelection 2
                        s3 <- getSelection 3
                        let s = S.fromList [s0,s1,s2,s3]
                        let s' = case passDir of
                                PassLeft    -> rotate s
                                PassAcross  -> rotate $ rotate s
                                PassRight   -> rotate $ rotate $ rotate s
                        return $ S.zipWith Z.union s' $ S.zipWith (Z.\\) deal s 

                    let who_starts = fromJust $ Z.member (Card Clubs 2) `S.findIndexL` board
                    gameLoop $ InRound board [NewTrick] $ FirstTrick who_starts

                -- World when in middle of round
                InRound board (now:on_stack) info -> do
                    -- eventually this will be server code
                    -- and rendering is client side responsibility
                    render board info
                    let world' = InRound board on_stack info
                    -- need to guarantee that stack is never empty
                    case now of 
                        NewTrick ->
                            let (w,s) = computeWinner info
                                nextTrick = TrickInfo w [] s
                                nextStep = if (>0) . Z.size $ board `S.index` 0
                                    then InRound board (GetInput:GetInput:GetInput:GetInput:NewTrick:on_stack) nextTrick
                                    else RoundOver s
                            in
                            gameLoop nextStep
                        GetInput -> do
                            -- get input from whomever cur_player is
                            -- right now its hot seat mode, so we ignore
                            player_input <- getMove world'
                            gameLoop $ InRound board (player_input:on_stack) info
                        Effect move ->
                            gameLoop $ move world'


computeWinner :: Info -> (PlayerID, Scores)
computeWinner (FirstTrick holds2c) = (holds2c, S.fromList [0,0,0,0])
computeWinner (TrickInfo _ played@((lead,_):_) scores) =
    let lead_suit = _suit lead
        (_best_card, winner) = maximumBy (cmpWith lead_suit) played
        pts (Card s r) | s==Hearts = 1
                       | r==12 && s==Spades = 13
                       | otherwise = 0
        trickVal = sum $ map (pts.fst) played 
        new_scores = S.adjust (+ trickVal) winner scores
    in
        (winner, new_scores)
    where cmpWith s (Card s1 r1,_) (Card s2 r2, _) 
            | s2 == s1  = compare r1 r2 
            | s1 == s   = GT
            | otherwise = LT
            

play :: Card -> World -> World
play card (InRound board _stack (TrickInfo cur_player played scores)) = 
    let new_board = S.adjust (Z.delete card) cur_player board 
        -- probably not worth making played a non-list structure just to 
        -- get nicer snoc
        new_played = played ++ [(card, cur_player)]
        next_player = (cur_player + 1) `mod` 4
    in
        InRound new_board _stack (TrickInfo next_player new_played scores)

getMove :: World -> IO Effect
getMove w@(InRound board _stack info) = do
    card <-  getCardFromHand hand
    if followsSuitIfAble card
    then return $ Effect (play card)
    else do 
        putStrLn "Illegal move: must follow suit"
        getMove w
    where TrickInfo cur_player played _scores = info 
          hand = board `S.index` cur_player
          followsSuitIfAble card =
                  -- TODO: ensure hearts cannot be lead until it has been broken
                  let lead_suit = _suit $ fst $ head played
                      matches_lead c = _suit c == lead_suit
                      has_lead = Z.foldr ((||).matches_lead) False hand
                  in
                  -- note: lazy evaluation ensures we only examine the head
                  -- of played when it is non-empty  
                  null played || matches_lead card || not has_lead 

getMultiCards :: Int -> UZone -> IO (Z.Set Card)
getMultiCards 0 _ = return Z.empty
--getMultiCards _ empty = return Z.empty
getMultiCards i hand = do
    card <- getCardFromHand hand
    others <- getMultiCards (i-1) (Z.delete card hand)
    return $ card `Z.insert` others


getCardFromHand :: UZone -> IO Card
getCardFromHand hand = do
    putStrLn "Choose a card:"
    card <- getInput
    if card `Z.member` hand
    then return card
    else do
        putStrLn "Error: Card not in hand"
        getCardFromHand hand

getInput :: IO Card
getInput = do
    putStrLn "Choose Card: " 
    -- for hearts players only choices in the play are which card to play 
    -- We'll check that it's a legal play before constructing the effect 
    mv <- getLine
    case parseMove mv of
        Invalid -> do 
                    putStrLn "Could not interpret move!"
                    getInput
        Valid c -> return c

data MoveType = Invalid | Valid Card
{- Try to parse it as a card
 - if that fails, try to parse it as asking for a meta option
 - help, quit, valid_play_list, etc.
 - else give up and return invalid
 -}
parseMove :: String -> MoveType
parseMove [r,s] = Valid (Card (readSuit s) (readRank r))
parseMove _ = Invalid

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

main :: IO ()
main = void $ gameLoop StartGame
-- main = gameLoop StartGame >> return ()
-- hlint recommended using Control.Monad.void

render :: Board -> Info -> IO ()
render board (TrickInfo cur_player played scores) = do 
    -- if we should only be rendering the current players hand then do some checking
    -- the following clears the screen
    putStrLn "\ESC[H\ESC[2J"

    showScore 0
    showScore 1
    showScore 2
    showScore 3

    putStrLn $ "Waiting on " ++ show cur_player
    putStrLn $ "Currently > " ++ show played
    renderBoard board
    where showScore i = putStrLn $ "Player " ++ show i ++ " Score:" ++ show (scores `S.index` i)

render board (FirstTrick i) = do
    putStrLn $ "Player " ++ show i ++ "leads the 2c"
    renderBoard board   

renderBoard :: Board -> IO ()
renderBoard board = do
    printHand 0
    printHand 1
    printHand 2
    printHand 3
    where printHand i = putStrLn $ (++) ( concat ["Player ", show i, " Hand: "] )
                        $ unwords $ map show $ Z.toList $ board `S.index` i

stdDeck :: [Card]
---- setting aces at 14
stdDeck = [Card s r | r <- [2..14], s <- [Clubs, Hearts, Spades, Diamonds]]

shuffle :: [a] -> IO [a]
-- shuffle x = return x
shuffle xs = do
        ar <- newArr n xs
        forM [1..n] $ \i -> do
            j <- randomRIO (i,n)
            vi <- readArray ar i
            vj <- readArray ar j
            writeArray ar j vi
            return vj
  where
    n = length xs
    newArr :: Int -> [a] -> IO (IOArray Int a)
    newArr n' =  newListArray (1,n') 
