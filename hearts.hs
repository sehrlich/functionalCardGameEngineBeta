{-# LANGUAGE ViewPatterns #-}

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
data Info = TrickInfo PlayerID (S.Seq (Card,PlayerID)) Scores | FirstTrick PlayerID
data World = InRound Board Stack Info
            | StartGame 
            | StartRound PassDir Scores
            | PassingPhase Board PassDir
            | RoundOver Scores
            | GameOver Scores
type Stack = [Effect]
type Scores = S.Seq Int

type Board = S.Seq UZone

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

{- Server code
 - some of this should be farmed out into new threads 
 -}

main :: IO ()
main = void $ gameLoop StartGame
-- main will need to start a server running gameLoop
-- this we'll also spin up a player thread and 3 ai threads
--
-- main = gameLoop StartGame >> return ()
-- hlint recommended using Control.Monad.void

gameLoop :: World -> IO World
-- for initialization
-- get player names etc.
--
gameLoop StartGame = gameLoop $ StartRound PassLeft $ S.fromList [0,0,0,0]

-- dataflow states, may not need to have them
gameLoop (RoundOver scores) 
    = do
    putStrLn "Round Over"
    -- check for shooting the moon
    let moon_shot = 26 `S.elemIndexL` scores
    scores' <- 
        case moon_shot of
            Nothing -> return scores
            Just p -> do
                putStrLn $ "Player " ++ show p ++ " shot the moon"
                return $ fmap (26-) scores
    return $ RoundOver scores'

gameLoop (GameOver scores) 
    = do
    putStrLn "Game Over"; print scores -- should really be send message to clients
    return $ GameOver scores

-- World controlling events in a round
gameLoop (StartRound passDir scores) 
    = do
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
gameLoop (PassingPhase deal passDir) 
    = do
    board <- 
        if passDir == NoPass then return deal else 
        let getValidatedSelection i 
                = do  
                 candCardSet <- msgClient i (StcGetPassSelection (deal `S.index` i) passDir)
                 validate candCardSet
                -- validate $ client (StcGetPassSelection (deal `S.index` i) passDir)
            validate (CtsPassSelection toPass) = return toPass
            rotate (S.viewl -> x :< xs) =  xs |> x
        in do
        s0 <- getValidatedSelection 0
        s1 <- getValidatedSelection 1
        s2 <- getValidatedSelection 2
        s3 <- getValidatedSelection 3
        let s = S.fromList [s0,s1,s2,s3]
        let s' = case passDir of
                PassLeft    -> rotate s
                PassAcross  -> rotate $ rotate s
                PassRight   -> rotate $ rotate $ rotate s
        return $ S.zipWith Z.union s' $ S.zipWith (Z.\\) deal s 

    let who_starts = fromJust $ Z.member (Card Clubs 2) `S.findIndexL` board
    gameLoop $ InRound board [NewTrick] $ FirstTrick who_starts

                -- World when in middle of round
gameLoop (InRound board (now:on_stack) info) 
    = do
    render (RenderInRound board info)
    let world' = InRound board on_stack info
    -- need to guarantee that stack is never empty
    case now of 
        NewTrick ->
            let (w,s) = computeWinner info
                nextTrick = TrickInfo w S.empty s
                nextStep = if (>0) . Z.size $ board `S.index` 0
                    then InRound board (GetInput:GetInput:GetInput:GetInput:NewTrick:on_stack) nextTrick
                    else RoundOver s
            in
            gameLoop nextStep
        GetInput -> do
            let hand = board `S.index` curPlayer info
            move <- msgClient (curPlayer info) (StcGetMove hand info)
            let player_input = validate move
            gameLoop $ InRound board (player_input:on_stack) info
            where validate (CtsMove move) = Effect (play move)
        Effect move ->
            gameLoop $ move world'

msgClient :: PlayerID -> ServerToClient -> IO ClientToServer
msgClient 0 = client
msgClient _ = aiclient
-- msgClient i m = do
--             putStrLn $"message to "++ show i
--             case i of
--                 0 -> client m
--                 otherwise -> aiclient m

curPlayer :: Info -> Int
curPlayer (TrickInfo p _ _) = p
curPlayer (FirstTrick p) = p

computeWinner :: Info -> (PlayerID, Scores)
computeWinner (FirstTrick holds2c) = (holds2c, S.fromList [0,0,0,0])
computeWinner (TrickInfo _ played@( S.viewl -> (lead,_) :< _) scores) =
    let lead_suit = _suit lead
        (_best_card, winner) = F.maximumBy (cmpWith lead_suit) played
        pts (Card s r) | s==Hearts = 1
                       | r==12 && s==Spades = 13
                       | otherwise = 0
        trickVal = F.sum $ fmap (pts.fst) played 
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
        new_played = played |> (card, cur_player)
        next_player = (cur_player + 1) `mod` 4
    in
        InRound new_board _stack (TrickInfo next_player new_played scores)

-- rewriting this for servery stuff
data RenderInfo = RenderInRound Board Info | Passing UZone PassDir| BetweenRounds
render :: RenderInfo -> IO ()

--render :: Board -> Info -> IO ()
render (RenderInRound board (TrickInfo curPlayer played scores)) = do 
    -- if we should only be rendering the current players hand then do some checking
    -- the following clears the screen
    putStrLn "\ESC[H\ESC[2J"

    renderBoard board curPlayer
    putStrLn $ "Currently:" ++ F.concat (fmap ((' ':).show . fst) played)
    mapM_ showScore [0..3]
    where showScore  i = putStrLn $ showPlayer i ++ " Score:" ++ show (scores `S.index` i)
          showPlayer i = {- colorize  [44 | i==curPlayer] $ -} "Player " ++ show i

render (RenderInRound board (FirstTrick i)) = do
    putStrLn $ "Player " ++ show i ++ "leads the 2c"
    renderBoard board i

render (Passing hand passDir) = renderHand hand

renderBoard :: Board -> Int -> IO ()
renderBoard board curPlayer = mapM_ printHand [0..3]
    where printHand i = do
                        putStr $ colorize  [44 | i==curPlayer] $ concat ["Player ", show i, " Hand:"]
                        putStr " "
                        renderHand $ board `S.index` i

renderHand :: UZone -> IO ()
renderHand hand = putStrLn $ unwords $ map show $ Z.toList hand

data Message = ClientToServer | ServerToClient

data ClientToServer = CtsMove Card 
                    | CtsPassSelection (Z.Set Card)
data ServerToClient = StcGetMove UZone Info 
                    | StcGetPassSelection UZone PassDir

{- Client Side code
 - actual mechanism of splitting it as thread to be determined
 -
 - Should split some validation stuff out so that
 - it is accessible to both server and client --
 - my client should always send valid input
 - if server receives bad messages, it should check them
 -
 - Also, rendering should go here
 -}

client :: ServerToClient -> IO ClientToServer 
client (StcGetMove board info) = do
    card <- getMove board info
    return $ CtsMove card

client (StcGetPassSelection hand passDir) = do
   render $ Passing hand passDir
   cardSet <- getMultiCards 3 hand
   -- do client validation here
   return $ CtsPassSelection cardSet


getMove :: UZone -> Info -> IO Card
getMove hand info = do
    card <-  getCardFromHand hand
    if followsSuitIfAble hand info card
    then return card
    else do 
        putStrLn "Illegal move: must follow suit"
        getMove hand info
    where TrickInfo cur_player played _scores = info 
          
followsSuitIfAble :: UZone -> Info -> Card -> Bool
followsSuitIfAble hand info@(TrickInfo _ played _) card =
      -- TODO: ensure hearts cannot be lead until it has been broken
      let lead_suit = _suit $ fst $ S.index played 0
          matches_lead c = _suit c == lead_suit
          has_lead = Z.foldr ((||).matches_lead) False hand
      in
      -- note: lazy evaluation ensures we only examine the head
      -- of played when it is non-empty  
      S.null played || matches_lead card || not has_lead 

getMultiCards :: Int -> UZone -> IO (Z.Set Card)
getMultiCards 0 _ = return Z.empty
--getMultiCards _ empty = return Z.empty
getMultiCards i hand = do
    card <- getCardFromHand hand
    others <- getMultiCards (i-1) (Z.delete card hand)
    return $ card `Z.insert` others


getCardFromHand :: UZone -> IO Card
getCardFromHand hand = do
    -- renderHand hand
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
        | otherwise = 0 
        -- temporary thing should correspond to card not in hand

{- The trivial ai -}
aiclient :: ServerToClient -> IO ClientToServer 
aiclient (StcGetMove hand info) = 
    case F.find (followsSuitIfAble hand info) $ Z.toList hand of 
        Nothing   -> error "apparently cannot play card"
        Just card -> return $ CtsMove card

aiclient (StcGetPassSelection hand passDir) = do
    let cardSet = Z.fromList $ take 3 $ Z.toList hand
    return $ CtsPassSelection cardSet
