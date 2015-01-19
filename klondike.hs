import Data.Map.Strict as Z -- for Zones
import Control.Monad (liftM, forM) -- unless
import Data.Array.IO
import Data.Maybe (fromJust, isJust)
import System.Random

-- import System.IO
type ID = Int
-- type CardID = Int
data ZoneID = Deck | Active | Foundation Suit | Pile Int | HPile Int deriving (Eq, Show, Ord)

data Suit = Clubs | Hearts | Spades | Diamonds deriving (Eq, Show, Ord)
data Card = Card 
            { _id::ID, _suit::Suit, _rank::Int} deriving Eq --Show


diffColor :: Suit -> Suit -> Bool
diffColor Hearts Spades = True
diffColor Hearts Clubs = True
diffColor Diamonds Clubs = True
diffColor Diamonds Spades = True
diffColor Clubs Hearts = True
diffColor Clubs Diamonds = True
diffColor Spades Diamonds = True
diffColor Spades Hearts = True
diffColor _ _ = False

instance Show Card where
    show (Card _ s r) = 
                        let col = case s of
                                    Clubs       -> "\ESC[1;30;47m" 
                                    Spades      -> "\ESC[1;30;47m" 
                                    Hearts      -> "\ESC[1;31;47m" 
                                    Diamonds    -> "\ESC[1;31;47m" 
                        in col ++ ("-A23456789TJQKA"!!r) : head (show s) : "\ESC[0m" 
        
type OZone = [Card]-- ordered Zones

data Effect = Effect (IO World -> IO World) | Quit | Undo

data World = World Board Stack
type Stack = [Effect]

-- type Board = [Zone]
-- type Board = Z.Map ID Zone
-- type Board = Z.Map String OZone
type Board = Z.Map ZoneID OZone

gameLoop :: IO World -> IO ()
gameLoop ioworld = do
        -- world@(World _board _stack) <- ioworld
        -- unless (isFinal world) $ do
            render ioworld

            decision <- getInput --- should either get player command or get okay to resolve action
            case decision of
                    Effect move -> gameLoop $ move ioworld
                    Quit        -> return ()
            -- having multiple players means that we need only person with priority to okay it 
            -- and also make sure that priority gets passed around approriately 

data MoveType = Invalid | PileToPile ZoneID ZoneID | MultiPileToPile Int ZoneID ZoneID | Meta 
getInput :: IO Effect
getInput = do
    putStrLn "Choose Move: "
    mv <- getLine
    case parseMove (words mv) of
        Invalid -> do 
                    putStrLn "Invalid Move!" 
                    getInput
        Meta    -> return Quit -- check for quit, undo, restart, blah 
        PileToPile fromZ toZ  -> return $ Effect (liftM $ klondikeMove fromZ toZ)
        MultiPileToPile num fromZ toZ  -> return $ Effect (liftM $ klondikeMovePile num fromZ toZ)
        --  return $ Choice (liftM $ klondikeMove fromZ toZ)
        -- try taking reading two words as from and to (make sure limited to active, p# and *F) 
        -- try reading card to zone

parseMove :: [String] -> MoveType
parseMove [meta] = case meta of 
                    "quit" -> Meta
                    "deck" -> PileToPile Deck Active
                    _   -> Invalid
parseMove [fZ, tZ] =
        if isJust f && isJust t
        then PileToPile (if fZ == "deck" then Active else fromJust f) (fromJust t)
        else Invalid
            where 
                f = parseZone fZ
                t = parseZone tZ
                
parseMove [n, fZ, tZ] =
        if isJust f && isJust t
        then MultiPileToPile num (fromJust f) (fromJust t)
        else Invalid
            where 
                num = read n:: Int
                f = parseZone fZ
                t = parseZone tZ
parseMove _ = Invalid

parseZone :: String -> Maybe ZoneID
parseZone s = case s of 
              "d"       -> Just Deck
              "deck"    -> Just Deck
              "p1"      -> Just $ Pile 1
              "p2"      -> Just $ Pile 2
              "p3"      -> Just $ Pile 3
              "p4"      -> Just $ Pile 4
              "p5"      -> Just $ Pile 5
              "p6"      -> Just $ Pile 6
              "p7"      -> Just $ Pile 7
              "1"       -> Just $ Pile 1
              "2"       -> Just $ Pile 2
              "3"       -> Just $ Pile 3
              "4"       -> Just $ Pile 4
              "5"       -> Just $ Pile 5
              "6"       -> Just $ Pile 6
              "7"       -> Just $ Pile 7
              "cf"      -> Just $ Foundation Clubs
              "df"      -> Just $ Foundation Diamonds
              "hf"      -> Just $ Foundation Hearts
              "sf"      -> Just $ Foundation Spades
              "cF"      -> Just $ Foundation Clubs
              "dF"      -> Just $ Foundation Diamonds
              "hF"      -> Just $ Foundation Hearts
              "sF"      -> Just $ Foundation Spades
              _ -> Nothing

main :: IO ()
main = do
            shuffled <- initialState
            gameLoop $ return shuffled

cardback :: String
cardback = "\ESC[104m()\ESC[0m"

render :: IO World -> IO ()
render world = do 
    (World board _) <- world
    let get s = Z.findWithDefault [] s board
    let topcard p = case get p of 
                        []          -> "__"
                        (top:_rest)  -> show top
    let deck = if Prelude.null (get Deck) then "__" else cardback
    let pileString n = let  pn  = get (Pile n)
                            hpn = get (HPile n)
                        in (("\nP" ++ show n) ++).(':':) $ unwords $  [cardback | _ <- hpn] ++ Prelude.map show (reverse pn)

    putStrLn "\ESC[H\ESC[2J"
    putStrLn ("Deck:" ++ deck ++ "           "  ++ topcard (Foundation Clubs)       ++ " " 
                                                ++ topcard (Foundation Diamonds)    ++ " " 
                                                ++ topcard (Foundation Hearts)      ++ " " 
                                                ++ topcard (Foundation Spades))
    putStrLn ("Active:" ++  topcard Active)
    putStrLn $ pileString 1
    putStrLn $ pileString 2
    putStrLn $ pileString 3
    putStrLn $ pileString 4
    putStrLn $ pileString 5
    putStrLn $ pileString 6
    putStrLn $ pileString 7

stdDeck :: [Card]
--stdDeck = zipWith3 Card [1..52] (cycle [Clubs, Hearts, Spades, Diamonds]) (cycle [1..13]) -- setting aces to one for klondike
stdDeck = [Card 0 s r | r <- [1..13], s <- [Clubs, Hearts, Spades, Diamonds]]

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


initialState :: IO World
initialState = do
                shuffled <- shuffle stdDeck
                let base = Z.insert Deck shuffled $
                        Z.fromList $ zip [Deck, Active, 
                            Foundation Clubs, Foundation Diamonds, 
                            Foundation Hearts, Foundation Spades,
                            Pile 1, Pile 2, Pile 3, Pile 4, Pile 5, Pile 6, Pile 7,
                            HPile 1, HPile 2, HPile 3, HPile 4, HPile 5, HPile 6, HPile 7]
                            (repeat [])
                let dealt = klondikeForceMoveZone 1 Deck (Pile 1) $
                        klondikeForceMoveZone 1 Deck (Pile 2) $
                        klondikeForceMoveZone 1 Deck (Pile 3) $
                        klondikeForceMoveZone 1 Deck (Pile 4) $
                        klondikeForceMoveZone 1 Deck (Pile 5) $
                        klondikeForceMoveZone 1 Deck (Pile 6) $
                        klondikeForceMoveZone 1 Deck (Pile 7) $
                        klondikeForceMoveZone 1 Deck (HPile 2) $
                        klondikeForceMoveZone 2 Deck (HPile 3) $
                        klondikeForceMoveZone 3 Deck (HPile 4) $
                        klondikeForceMoveZone 4 Deck (HPile 5) $
                        klondikeForceMoveZone 5 Deck (HPile 6) $
                        klondikeForceMoveZone 6 Deck (HPile 7) 
                        base
                return $ World dealt [id]

-- Move the first card in the first zone to the second zone and return the new board
klondikeForceMoveZone :: Int -> ZoneID -> ZoneID -> Board -> Board
klondikeForceMoveZone pileSize fromZone toZone board =
            if failed then board else newBoard
            where
                failed = False -- should be changing this to fail if zone is empty
                -- card = head (Z.findWithDefault (error $ "zone: " ++ fromZone ++ " does not exist") fromZone board)
                -- Apparently the default gets evaluated
                card = take pileSize (Z.findWithDefault stdDeck fromZone board)
                newBoard = Z.adjust (drop pileSize) fromZone $ Z.adjust (card++) toZone board


klondikeMove :: ZoneID -> ZoneID -> World -> World
klondikeMove Deck Active (World board stack) 
            | Prelude.null $ fromJust (Z.lookup Deck board) =
                let curdeck = reverse (fromJust $ Z.lookup Active board)
                    newboard = Z.insert Deck curdeck $ Z.insert Active [] board
                in World newboard stack
klondikeMove (Pile i) (Pile j) world -- Temp meausure until movePile hack is fixed
            | i == j = klondikeMove (HPile i) (Pile i) world 
            
klondikeMove fromZone toZone world@(World board stack) = 
            if possible && validMove then newWorld else world
            where 
                Just fZ = Z.lookup fromZone board
                -- Just tZ = Z.lookup toZone board
                possible = not $ Prelude.null fZ
                -- this will check stuff like colors, putting right cards in foundation, if move card to empty pile, must be moving a king
                card = head fZ
                validMove = canPlace card toZone fromZone board 
                afterMove = Z.adjust tail fromZone $ Z.adjust (card:) toZone board
                newWorld = flipHidden fZ fromZone $ World afterMove stack 

flipHidden :: OZone -> ZoneID -> World -> World
flipHidden [_] (Pile i) w = klondikeMove (HPile i) (Pile i) w
flipHidden _ _  w = w

canPlace :: Card -> ZoneID -> ZoneID -> Board -> Bool
canPlace (Card _ s r) (Foundation f) _z b = f==s && r== if Prelude.null pile then 1 else _rank (head pile) + 1
                                                 -- && Prelude.null pile 
                                                where pile = fromJust $ Z.lookup (Foundation f) b
canPlace (Card _ _s _r) (Pile i) (HPile j) _ = i==j -- should maybe check that pile is empty, but eh
canPlace (Card _ s r) p@(Pile _i) _ b = 
        let xs = Z.findWithDefault [] p b
        in  if Prelude.null xs
            then  r== 13
            else (_rank (head xs) - 1) == r && diffColor s (_suit (head xs))
        -- colors differ and right rank
canPlace _ Active Deck _    = True
canPlace _ _ _ _            = False

klondikeMovePile :: Int -> ZoneID -> ZoneID -> World -> World
klondikeMovePile n fromID toID (World curBoard s)= 
                -- ASSUME THAT MOVE IS VALID
                -- THIS IS NOT A GOOD IDEA
                -- should work this back into regular klondikeMove
                
                let fromZone = fromJust $ Z.lookup fromID curBoard
                    chunk    = take n fromZone
                    newBoard = Z.adjust (drop n) fromID $ Z.adjust (chunk ++) toID curBoard
                in 
                    World newBoard s
