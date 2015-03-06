module HeartsTui
    ( renderText
    , clientTextBased
    )
    where

import HeartsCommon
import qualified Data.Set as Z
import qualified Data.Sequence as S
import qualified Data.Foldable as F
import Data.List (intercalate) -- colorize

clientTextBased :: ServerToClient -> IO ClientToServer

clientTextBased (StcRender rinfo) = do
    renderText rinfo
    return CtsAcknowledge

clientTextBased StcGameStart = do
    return CtsAcknowledge

clientTextBased (StcGetMove hand info) = do
    card <- getMove hand info
    return $ CtsMove card

clientTextBased (StcGetPassSelection hand _passDir) = do
   -- render $ Passing hand passDir
   cardSet <- getMultiCards 3 hand
   -- do client validation here
   return $ CtsPassSelection cardSet

clientTextBased StcGameOver = return CtsDisconnect

getMove :: Hand -> Info -> IO Card
getMove hand info = do
    card <- getCardFromHand hand
    if isValidPlay hand info card
    then return card
    else do
        putStrLn "Illegal move: must follow suit"
        getMove hand info

getMultiCards :: Int -> Hand -> IO (Z.Set Card)
getMultiCards 0 _ = return Z.empty
--getMultiCards _ empty = return Z.empty
getMultiCards i hand = do
    card <- getCardFromHand hand
    others <- getMultiCards (i-1) (Z.delete card hand)
    return $ card `Z.insert` others


getCardFromHand :: Hand -> IO Card
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
    -- TODO: try parsing meta options and so forth too
    case readCard mv of
        Nothing -> do
                    putStrLn "Could not interpret move!"
                    getInput
        Just c -> return c

renderText :: RenderInfo -> IO ()
renderText (Canonical ObjectList objList strList)
    = do
    putStrLn "\ESC[H\ESC[2J"
    putStrLn $ unwords $ map (pretty . snd) objList
    mapM_ putStrLn strList

renderText (RenderInRound hand played scores) = do
    -- if we should only be rendering the current players hand then do some checking
    -- the following clears the screen
    putStrLn "\ESC[H\ESC[2J"

    putStrLn $ renderTextPlay played
    putStrLn $ renderTextHand hand
    renderTextScores scores

renderText (RenderServerState board info) = do
    -- if we should only be rendering the current players hand then do some checking
    -- the following clears the screen
    putStrLn "\ESC[H\ESC[2J"

    putStrLn $ renderTextPlay $ playedSoFar info
    renderTextBoard board $ curPlayer info
    renderTextScores $ pointsCollected info

renderText (Passing hand _passDir) = putStrLn $ renderTextHand hand
renderText RenderEmpty = return ()

renderText (BetweenRounds scores) = renderTextScores scores

renderTextScores :: Scores -> IO ()
renderTextScores scores = mapM_ showScore [0..3]
    where showScore  i = putStrLn $ showPlayer i ++ " Score:" ++ show (scores `S.index` i)
          showPlayer i = {- colorize  [44 | i==curPlayer] $ -} "Player " ++ show i

renderTextBoard :: Board -> Int -> IO ()
renderTextBoard board activePlayer = mapM_ printHand [0..3]
    where printHand i = do
                        putStr $ colorize  [44 | i==activePlayer] $ concat ["Player ", show i, " Hand:"]
                        putStr " "
                        putStrLn $ renderTextHand $ board `S.index` i

renderTextHand :: Hand -> String
renderTextHand hand = unwords $ map pretty $ Z.toList hand

renderTextPlay :: Trick -> String
renderTextPlay played = "Currently:" ++ F.concat (fmap ((' ':).pretty ) played)


colorize :: [Int] -> String -> String
colorize options str = "\ESC["
                        ++ intercalate ";" [show i | i <-options]
                        ++ "m" ++ str ++ "\ESC[0m"
