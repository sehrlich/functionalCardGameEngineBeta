{-# LANGUAGE ViewPatterns, PatternSynonyms #-} -- for pattern matching on sequences
{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-} -- for the serializable nonsense
{-# OPTIONS_GHC -Wall -fno-warn-unused-imports #-}
-- {-# LANGUAGE TemplateHaskell #-} -- make lenses maybe
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}

import PlayingCards
-- import qualified Data.Map.Strict as B -- for Zones
import qualified Data.Set as Z
import Data.Sequence ((|>), (<|)) -- , ViewR ((:>)), ViewL ((:<)))
import qualified Data.Sequence as S
import qualified Data.Foldable as F
import Control.Monad (void, forever) -- liftM, unless
import Data.Maybe (fromJust)
import Data.List (intercalate)

-- for serialization
import Data.Typeable
import Data.Binary
import GHC.Generics (Generic)

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TMVar

-- import Async
-- import Control.Distributed.Process

-- import Data.Vector 
-- consider replacing all list with sequences
-- import prelude as qualified
-- and importing all of sequence


type PlayerID = Int
type Player = (TMVar ServerToClient, TMVar ClientToServer, ThreadId) -- ??


colorize :: [Int] -> String -> String
colorize options str = "\ESC[" 
                        ++ intercalate ";" [show i | i <-options] 
                        ++ "m" ++ str ++ "\ESC[0m"

type UZone = Z.Set Card

data Effect = Effect (World -> World) 
                | GetInput 
                | NewTrick
                | ComputeWinner

data PassDir = PassLeft | PassRight | PassAcross | NoPass deriving (Eq, Generic, Typeable)
data Info = TrickInfo PlayerID (S.Seq (Card,PlayerID)) Scores Bool
            deriving (Generic, Typeable)
data World = InRound Board Stack Info
            | StartGame 
            | StartRound PassDir Scores
            | PassingPhase Board PassDir
            | RoundOver Scores
            | GameOver Scores
            deriving (Generic, Typeable)
type Stack = [Effect]
type Scores = S.Seq Int
type Trick = S.Seq (Card, PlayerID)
type Board = S.Seq UZone

{- Server code
 - some of this should be farmed out into new threads 
 -}
--port :: Int
--port = 44444
constructPlayer :: (ServerToClient -> IO ClientToServer) -> IO Player
constructPlayer respondTo 
    = do
    inbox  <- newEmptyTMVarIO -- :: (TMVar ServerToClient)
    outbox <- newEmptyTMVarIO -- :: (TMVar ClientToServer)
    thread <- forkIO $ playerThread inbox outbox 
    return (inbox, outbox, thread)

    where playerThread inbox outbox = forever $ do 
            message <- atomically $ takeTMVar inbox
            response <- respondTo message
            atomically $ putTMVar outbox response

main :: IO ()
main = do
        p0 <- constructPlayer client
        p1 <- constructPlayer aiclient
        p2 <- constructPlayer aiclient
        p3 <- constructPlayer aiclient
        void $ gameLoop [p0,p1,p2,p3] StartGame

gameLoop :: [Player] -> World -> IO World
gameLoop players StartGame = gameLoop players $ StartRound PassLeft $ S.fromList [0,0,0,0]

-- dataflow states, may not need to have them
gameLoop _players (RoundOver scores) 
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
    -- send info to clients
    render (BetweenRounds scores)
    return $ RoundOver scores'

gameLoop _players (GameOver scores) 
    = do
    putStrLn "Game Over"; print scores -- should really be send message to clients
    -- msg clients game over
    return $ GameOver scores

-- World controlling events in a round
gameLoop players (StartRound passDir scores) 
    = do
    deck <- shuffle stdDeck
    let h0 = Z.fromList $ take 13 deck
    let h1 = Z.fromList $ take 13 $ drop 13 deck
    let h2 = Z.fromList $ take 13 $ drop 26 deck
    let h3 = Z.fromList $ take 13 $ drop 39 deck
    let deal = S.fromList [h0,h1,h2,h3]
    -- distribute deck to player hands 
    -- play round 
    RoundOver round_scores <- gameLoop players $ PassingPhase deal passDir
    
    let new_scores = S.zipWith (+) round_scores scores
    if checkScores new_scores then return $ GameOver new_scores
    else gameLoop players $ StartRound next_pass_dir new_scores
    where checkScores = F.any (>100)
          next_pass_dir = case passDir of 
                        PassLeft    -> PassRight
                        PassRight   -> PassAcross
                        PassAcross  -> NoPass
                        NoPass      -> PassLeft


                -- World when trying to pass
gameLoop players (PassingPhase deal passDir) 
    = do
    board <- 
        if passDir == NoPass then return deal else 
        let getValidatedSelection i 
                = do  
                 candCardSet <- msgClient (players!!i) (StcGetPassSelection (deal `S.index` i) passDir)
                 validate candCardSet
                -- validate $ client (StcGetPassSelection (deal `S.index` i) passDir)
            validate (CtsPassSelection toPass) = return toPass
            -- TODO would prefer this to be non-stupid
            validate _ = error "need to make this try-catch or somesuch"
            rotate (x :< xs) =  xs |> x
            rotate (Empty) =  S.empty
            rotate _ = error "this is not a sequence"
        in do
        s0 <- getValidatedSelection 0
        s1 <- getValidatedSelection 1
        s2 <- getValidatedSelection 2
        s3 <- getValidatedSelection 3
        let s = S.fromList [s0,s1,s2,s3]
        let s' = case passDir of
                NoPass      -> s
                PassLeft    -> rotate s
                PassAcross  -> rotate $ rotate s
                PassRight   -> rotate $ rotate $ rotate s
        return $ S.zipWith Z.union s' $ S.zipWith (Z.\\) deal s 

    let who_starts = fromJust $ Z.member (Card Clubs 2) `S.findIndexL` board
    gameLoop players $ InRound board [NewTrick] 
                     $ TrickInfo who_starts S.empty (S.fromList [0,0,0,0]) False

                -- World when in middle of round
gameLoop _players (InRound _board [] _info) 
    = error "stack is empty" 
    -- Fix this case
gameLoop players (InRound board (now:on_stack) info) 
    = do
    render (RenderServerState board info)
    let world' = InRound board on_stack info
    -- need to guarantee that stack is never empty
    case now of 
        NewTrick ->
            gameLoop players $ InRound board (GetInput:GetInput:GetInput:GetInput:ComputeWinner:on_stack) info
            -- consider computing winner at end of trick
            -- as new effect so 
            -- 4x get_input : computeWinner : NewTrick
        ComputeWinner -> 
            -- split new trick into here
            let (w,s,b) = computeWinner info
                nextTrick = TrickInfo w S.empty s b
                nextStep = if (>0) . Z.size $ board `S.index` 0
                    then InRound board (NewTrick:on_stack) nextTrick
                    else RoundOver s
            in
            gameLoop players nextStep
        GetInput -> do
            let hand = board `S.index` curPlayer info
            move <- msgClient (players!!curPlayer info) (StcGetMove hand info)
            let player_input = validate move
            gameLoop players $ InRound board (player_input:on_stack) info
            where validate (CtsMove move) = Effect (play move)
                  validate _ = error "recieved wrong type of message"
        Effect move ->
            gameLoop players $ move world'

msgClient :: Player -> ServerToClient -> IO ClientToServer
msgClient (inbox, outbox, _) message
    = do
    atomically $ putTMVar inbox message
    atomically $ takeTMVar outbox

curPlayer :: Info -> Int
curPlayer (TrickInfo p _ _ _) = p

computeWinner :: Info -> (PlayerID, Scores, Bool)
computeWinner (TrickInfo _ played@((lead,_) :< _) scores broken) =
    let lead_suit = _suit lead
        (_best_card, winner) = F.maximumBy (cmpWith lead_suit) played
        pts (Card s r) | s==Hearts = 1
                       | r==12 && s==Spades = 13
                       | otherwise = 0
        trickVal    = F.sum $ fmap (pts.fst) played 
        new_scores  = S.adjust (+ trickVal) winner scores
        isHeart c   = _suit c == Hearts
        broken'     = broken || F.foldr ((||).isHeart.fst) False played
    in
        (winner, new_scores, broken')
    where cmpWith s (Card s1 r1,_) (Card s2 r2, _) 
            | s2 == s1  = compare r1 r2 
            | s1 == s   = GT
            | otherwise = LT
computeWinner _ = error "empty trick"
            

play :: Card -> World -> World
play card (InRound board _stack (TrickInfo cur_player played scores bool)) = 
    let new_board = S.adjust (Z.delete card) cur_player board 
        new_played = played |> (card, cur_player)
        next_player = (cur_player + 1) `mod` 4
    in
        InRound new_board _stack (TrickInfo next_player new_played scores bool)
play _ _ = error "world not InRound"

-- rewriting this for servery stuff
data RenderInfo = RenderServerState Board Info 
                | Passing UZone PassDir 
                | BetweenRounds Scores 
                | RenderInRound UZone Trick Scores
render :: RenderInfo -> IO ()

--render :: Board -> Info -> IO ()
render (RenderInRound hand played scores) = do 
    -- if we should only be rendering the current players hand then do some checking
    -- the following clears the screen
    putStrLn "\ESC[H\ESC[2J"

    renderPlay played 
    renderHand hand
    renderScores scores

render (RenderServerState board info@(TrickInfo _ played scores _)) = do 
    -- if we should only be rendering the current players hand then do some checking
    -- the following clears the screen
    putStrLn "\ESC[H\ESC[2J"

    renderPlay played 
    renderBoard board $ curPlayer info
    renderScores scores

render (Passing hand _passDir) = renderHand hand

render (BetweenRounds scores) = renderScores scores

renderScores :: Scores -> IO ()
renderScores scores = mapM_ showScore [0..3]
    where showScore  i = putStrLn $ showPlayer i ++ " Score:" ++ show (scores `S.index` i)
          showPlayer i = {- colorize  [44 | i==curPlayer] $ -} "Player " ++ show i

renderBoard :: Board -> Int -> IO ()
renderBoard board activePlayer = mapM_ printHand [0..3]
    where printHand i = do
                        putStr $ colorize  [44 | i==activePlayer] $ concat ["Player ", show i, " Hand:"]
                        putStr " "
                        renderHand $ board `S.index` i

renderHand :: UZone -> IO ()
renderHand hand = putStrLn $ unwords $ map show $ Z.toList hand

renderPlay :: Trick -> IO ()
renderPlay played = putStrLn $ "Currently:" ++ F.concat (fmap ((' ':).show . fst) played)

data Message = ClientToServer | ServerToClient deriving (Generic, Typeable)

data ClientToServer = CtsMove Card 
                    | CtsPassSelection (Z.Set Card)
                    | CtsDisconnect
data ServerToClient = StcGetMove UZone Info 
                    | StcGetPassSelection UZone PassDir
                    | StcGameOver

{- The trivial ai -}
{- should replace with random choice -}
aiclient :: ServerToClient -> IO ClientToServer 
aiclient (StcGetMove hand info) = 
    case F.find (isValidPlay hand info) $ Z.toList hand of 
        Nothing   -> error "apparently cannot play card"
        Just card -> return $ CtsMove card

aiclient (StcGetPassSelection hand _passDir) = do
    let cardSet = Z.fromList $ take 3 $ Z.toList hand
    return $ CtsPassSelection cardSet

aiclient StcGameOver = return CtsDisconnect

{-- Client Side code
 -- actual mechanism of splitting it as thread to be determined
 --
 -- Should split some validation stuff out so that
 -- it is accessible to both server and client --
 -- my client should always send valid input
 -- if server receives bad messages, it should check them
 --
 -- Also, rendering should go here
 --}

client :: ServerToClient -> IO ClientToServer 
client (StcGetMove hand info) = do
    card <- getMove hand info
    return $ CtsMove card

client (StcGetPassSelection hand passDir) = do
   render $ Passing hand passDir
   cardSet <- getMultiCards 3 hand
   -- do client validation here
   return $ CtsPassSelection cardSet

client StcGameOver = return CtsDisconnect

getMove :: UZone -> Info -> IO Card
getMove hand info = do
    card <- getCardFromHand hand
    if isValidPlay hand info card
    then return card
    else do 
        putStrLn "Illegal move: must follow suit"
        getMove hand info
--    where TrickInfo cur_player played _scores = info 

-- This seems like an ideal thing to practice using quickCheck with
-- namely, no matter what the trick is, should always have at least one valid play
isValidPlay :: UZone -> Info -> Card -> Bool
isValidPlay hand _info@(TrickInfo _ played _ heartsBroken) card =
    let checkHandHasNo p    = not $ Z.foldr ((||).p) False hand
        playIf p            = p card || checkHandHasNo p
        on_lead         = S.null played
        isFirstTrick    = is2c $ fst $ S.index played 0
        matchesLead c   = _suit c == _suit (fst $ S.index played 0)
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

-- Patterns go at end of file since hlint can't parse them
pattern Empty   <- (S.viewl -> S.EmptyL)
pattern x :< xs <- (S.viewl -> x S.:< xs)
-- pattern xs :> x <- (S.viewr -> xs S.:> x)
