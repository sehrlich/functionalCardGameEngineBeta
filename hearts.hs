-- import Data.Map.Strict as Z -- for Zones
import Data.Set as Z
import Control.Monad (forM) -- liftM, unless
import Data.Array.IO
import Data.Maybe (isNothing)
import Data.List (intercalate)
import System.Random
-- import Data.Vector 

-- import System.IO
type PlayerID = Int
-- type ID = Int
-- type CardID = Int
-- data ZoneID = Deck | Hand Player | Collected Player | Play deriving (Eq, Show, Ord)

data Suit = Clubs | Hearts | Spades | Diamonds deriving (Eq, Show, Ord)
data Card = Card 
            {_suit::Suit, _rank::Int} deriving (Eq, Ord) --Show
-- data Card = Card 
--             { _id::ID, _suit::Suit, _rank::Int} deriving Eq --Show

-- consider adding a colorizing function for cleaniness sake 
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
        
--type OZone = [Card]-- ordered Zones
--type UZone = Z.Map ID Card -- unordered Zones
type UZone = Z.Set Card

data Effect = Effect (World -> World) 
                {-| Quit | Undo -}
                | GetInput {-| Pass -}

data PassDir = PassLeft {-| PassRight | PassAcross | NoPass-}
data Info = TrickInfo PlayerID (Maybe Suit) [Maybe Card] Scores
data World = InRound Board Stack Info
            | StartGame 
            | StartRound PassDir Scores
            | GameOver Scores
type Stack = [Effect]
type Scores = [Int]

type Board = [UZone]
-- type Board = [Zone]
-- type Board = Z.Map ID Zone
-- type Board = Z.Map String OZone
-- type Board = Z.Map ZoneID UZone

gameLoop :: IO World -> IO ()
gameLoop ioworld = do
            world <- ioworld
            case world of
                StartGame -> do
                    -- set scores to zero
                    -- get player names etc.
                    --
                    putStrLn "Start Game"
                    gameLoop $ return $ StartRound PassLeft [0,0,0,0]
                GameOver scores -> do
                    putStrLn "Game Over"
                    print scores
                    return ()
                StartRound _pass_dir scores ->
                    if checkScores scores
                        then gameLoop $ return $ GameOver scores
                        else do
                        deck <- shuffle stdDeck
                        let h0 = Z.fromList $ take 13 deck
                        let h1 = Z.fromList $ take 13 $ drop 13 deck
                        let h2 = Z.fromList $ take 13 $ drop 26 deck
                        let h3 = Z.fromList $ take 13 $ drop 39 deck
                        let board = [h0,h1,h2,h3]
                        -- distribute deck to player hands
                        -- pass
                        let who_starts = 0 -- TODO see who has two of clubs
                        let emptyInfo = TrickInfo who_starts Nothing 
                                        [Nothing, Nothing, Nothing, Nothing]
                                        scores
                        gameLoop $ return $ InRound board [GetInput] emptyInfo
                    where checkScores _ = False
                InRound board (now:on_stack) info -> do
                    -- eventually this will be server code
                    -- and rendering is client side responsibility
                    render board info
                    let world' = InRound board on_stack info
-- need to guarantee that stack is never empty
                    case now of 
                        GetInput -> do
                            -- get input from whomever cur_player is
                            -- right now its hot seat mode, so we ignore
                            player_input <- getMove world'
                            gameLoop $ return $ InRound board (player_input:on_stack) info
                        Effect move -> gameLoop $ return $ move world 

getMove :: World -> IO Effect
getMove w@(InRound board _stack info) = do
    card <-  getInput
    if holds card && followsSuitIfAble card
    then return $ Effect (play card)
    else do 
        putStrLn "Illegal move:"
        getMove w
    where TrickInfo cur_player lead _played _scores = info 
          hand = board !! cur_player
          holds c = Z.member c hand
          followsSuitIfAble card = 
            case lead of
                Nothing -> True
                Just lead_suit -> 
                    let matches_lead c = _suit c == lead_suit 
                        has_no_lead = Z.foldr ((||).matches_lead) True hand
                    in
                    matches_lead card || has_no_lead 


play :: Card -> World -> World
play card (InRound board _stack (TrickInfo cur_player lead played scores)) = 
    let new_board = board -- TODO remove card from board, add it to played
        -- played !! cur_player = Just card
        lead' = if isNothing lead then Just $ _suit card else lead
        next_player = (cur_player + 1) `mod` 4
    in
        InRound new_board _stack (TrickInfo next_player lead' played scores)

getInput :: IO Card
getInput = do
    putStrLn "Choose Card: " {- for hearts players only choices in the play are which card to play 
     - We'll check that it's a legal play before constructing the effect 
     -}
    mv <- getLine
    case parseMove mv of
        Invalid -> do 
                    putStrLn "Could not interpret move!"
                    getInput
        Valid c -> return c

data MoveType = Invalid | Valid Card
parseMove :: String -> MoveType
parseMove [r,s] = 
{- Try to parse it as a card
 - if that fails, try to parse it as asking for a meta option
 - help, quit, valid_play_list, etc.
 - else give up and return invalid
 -}
    --let rank = case r of
    Valid (Card (readSuit s) (readRank r))

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
        | r=='K' = 13
        | r=='Q' = 12
        | r=='J' = 11
        | r=='T' = 10
        | r `elem` "23456789" = read [r] ::Int

main :: IO ()
main = gameLoop $ return StartGame

render :: Board -> Info -> IO ()
render [h0,h1,h2,h3] (TrickInfo cur_player _lead played [s0,s1,s2,s3]) = do 
    -- if we should only be rendering the current players hand then do some checking
    putStr $ "Player 0 Score:" ++ show s0 ++ " Hand: "
    putStrLn $ unwords $ Prelude.map show $ Z.toList h0
    putStr $ "Player 1 Score:" ++ show s1 ++ " Hand: "
    putStrLn $ unwords $ Prelude.map show $ Z.toList h1
    putStr $ "Player 2 Score:" ++ show s2 ++ " Hand: "
    putStrLn $ unwords $ Prelude.map show $ Z.toList h2
    putStr $ "Player 3 Score:" ++ show s3 ++ " Hand: "
    putStrLn $ unwords $ Prelude.map show $ Z.toList h3
    putStrLn $ "Waiting on" ++ show cur_player
    putStrLn $ "Currently >" ++ show played
    return ()

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
