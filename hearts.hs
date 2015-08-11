{-# LANGUAGE ViewPatterns, PatternSynonyms #-} -- for pattern matching on sequences
{-# OPTIONS_GHC -Wall #-} -- -fno-warn-unused-imports #-}
-- {-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-} -- for the serializable nonsense
-- {-# LANGUAGE TemplateHaskell #-} -- make lenses maybe
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- import PlayingCards
import HeartsCommon
import HeartsClient
import qualified Data.Set as Z
import Data.Sequence ((|>)) -- , (<|))
import qualified Data.Sequence as S
import qualified Data.Foldable as F
import Control.Monad (void)
import Data.Maybe (fromJust)

-- for serialization
-- import Data.Typeable
-- import Data.Binary
-- import GHC.Generics (Generic)

-- import Control.Concurrent
import Control.Concurrent.STM
-- import Control.Concurrent.STM.TMVar
import qualified Control.Concurrent.Async as Async
-- import Control.Distributed.Process

import Control.Concurrent.Supply
-- we need to supply a splitsupply to each child

-- I should add object ids here upon generating shuffled deck
--
-- Eventually main should start a server that recieves connections
-- when it gets enough to start a game, it splits off a separate thread to host the game
main :: IO ()
main = do
        p0 <- constructGUIPlayer 0
        p1 <- constructPlayer aiclient 1
        p2 <- constructPlayer aiclient 2
        p3 <- constructPlayer aiclient 3
        void $ gameLoop [p0,p1,p2,p3] StartGame

msgClient :: Player -> ServerToClient -> IO ClientToServer
msgClient player message
    = msgClientInternal player (const message)

msgClientInternal :: Player -> (Int -> ServerToClient) -> IO ClientToServer
msgClientInternal (inbox, outbox, _, forPos) message
    = do
    print $ "server sent " ++ (show forPos) ++ ": " ++ (show $ message forPos)
    atomically $ putTMVar inbox (message forPos)
    gotBack <- atomically $ takeTMVar outbox
    print $ "recieved from " ++ (show forPos) ++ ": " ++ (show gotBack)
    return gotBack

broadcastInternal :: [Player] -> (Int -> ServerToClient) -> IO [ClientToServer]
broadcastInternal players message
    = Async.mapConcurrently (flip msgClientInternal message) players

{--| For use when everyone gets the same message --}
broadcast_ :: [Player] -> ServerToClient -> IO ()
broadcast_ players message
    = broadcastInternal players (const message) >> return ()

{--| For use when players get tailored messages --}
broadcast_' :: [Player] -> (Int -> ServerToClient) -> IO ()
broadcast_' players message
    = broadcastInternal players (message) >> return ()

gameLoop :: [Player] -> World -> IO World
gameLoop players StartGame
    = do
    broadcast_' players StcGameStart
    gameLoop players $ StartRound PassLeft $ S.fromList [0,0,0,0]

-- dataflow states, may not need to have them
gameLoop players (RoundOver scores)
    = do
    -- check for shooting the moon
    let moon_shot = 26 `S.elemIndexL` scores
    scores' <-
        case moon_shot of
            Nothing -> return scores
            Just _p -> return $ fmap (26-) scores
    broadcast_ players (StcRender $ BetweenRounds scores')
    return $ RoundOver scores'

gameLoop _players (GameOver scores)
    = do
    -- should really be send message to clients
    -- putStrLn "Game Over"; print scores
    -- msg clients game over
    return $ GameOver scores

-- World controlling events in a round
gameLoop players (StartRound passDir scores)
    = do
    sup <- newSupply
    deck <- shuffledDeck sup
    -- need shuffled deck to have HCards, so need to pass supply to shuffled deck with ids
    let deal = fmap (unorderPile) $ S.unfoldr (drawExactly 13) $ S.fromList deck
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
    -- print $ "prepare to pass"
    board <-
        if passDir == NoPass then return deal else
        let getValidatedSelection i
                = do
                candCardSet <- msgClient (players!!i) (StcGetPassSelection (deal `S.index` i) passDir)
                validate candCardSet
                -- validate $ client (StcGetPassSelection (deal `S.index` i) passDir)
            validate (CtsPassSelection toPass) = return toPass
            -- TODO would prefer this to be non-stupid
            validate e = error $ "need to make this try-catch or somesuch: " ++ show e
            rotate (x :< xs) =  xs |> x
            rotate (Empty) =  S.empty
            rotate _ = error "this is not a sequence"
        in do
        -- print $ "telling players their hands"
        broadcast_ players (StcRender $ Passing (deal `S.index` 0) passDir)
        -- TODO make getValidatedSelection a thing that can be broadcast (i.e. similar to broadcast internal)
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

    let who_starts = fromJust $ F.any (matches 2 Clubs) `S.findIndexL` board
    gameLoop players $ InRound board [NewTrick]
                     $ TrickInfo who_starts S.empty (S.fromList [0,0,0,0]) False

                -- World when in middle of round
gameLoop _players (InRound _board [] _info)
    = error "stack is empty"
    -- Fix this case
gameLoop players (InRound board (now:on_stack) info@(TrickInfo _w played scores _broken))
    = do
    -- broadcast_' players $ StcRender . \i -> RenderInRound (S.index board i) played scores
    broadcast_' players $ StcRender . (flip (flip RenderInRound (decorate info played)) scores) . (S.index board)
    let world' = InRound board on_stack info
    -- need to guarantee that stack is never empty
    case now of
        NewTrick ->
            gameLoop players $ InRound board (GetInput:GetInput:GetInput:GetInput:ComputeWinner:on_stack) info
        ComputeWinner ->
            let (w,s,b) = computeWinner info
                nextTrick = TrickInfo w S.empty s b
                nextStep = if (>0) . Z.size $ board `S.index` 0
                    then InRound board (NewTrick:on_stack) nextTrick
                    else RoundOver s
            in
            do
            broadcast_ players StcCleanTrick
            gameLoop players nextStep
        GetInput -> do
            let hand = board `S.index` curPlayer info
            move <- msgClient (players!!curPlayer info) (StcGetMove hand info)
            let player_input = validate move
            gameLoop players $ InRound board (player_input:on_stack) info
            where validate (CtsMove move) = 
                    -- check that move is valid, it is players hand, they don't have to follow suit, etc.
                    -- if so then
                    Effect (play move)
                    -- else send back message to client
                  validate e = error $ "recieved from " ++ (show $ curPlayer info) ++ " wrong type of message: " ++ show e
        Effect move ->
            gameLoop players $ move world'

play :: Card -> World -> World
play card (InRound board _stack (TrickInfo cur_player played scores bool)) =
    let new_board = S.adjust (Z.delete card) cur_player board
        new_played = played |> card
        next_player = (cur_player + 1) `mod` 4
    in
        InRound new_board _stack (TrickInfo next_player new_played scores bool)
play _ _ = error "world not InRound"


-- Patterns go at end of file since hlint can't parse them
pattern Empty   <- (S.viewl -> S.EmptyL)
pattern x :< xs <- (S.viewl -> x S.:< xs)
-- pattern xs :> x <- (S.viewr -> xs S.:> x)
