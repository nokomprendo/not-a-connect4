{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict #-}

module NaC4.Client.Bot where

import NaC4.Game
import NaC4.Utils

import Control.Monad
import Control.Monad.ST
import qualified Data.Massiv.Array as A
import qualified Data.Vector.Mutable as VM  -- TODO use massiv ?
import Data.STRef
import System.Random.MWC

----------------------------------------------------------------------
-- Bot & helpers
----------------------------------------------------------------------

class Bot s b where
    genmove :: b -> Double -> Game s -> ST s Int

playoutBots :: (Bot s b1, Bot s b2) => b1 -> b2 -> Game s -> ST s Status
playoutBots botR botY g0 
    | isRunning g0 =
        let moveFunc = if _currentPlayer g0 == PlayerR then genmove botR 1
                                                       else genmove botY 1
        in moveFunc g0 >>= (`playK` g0) >>= playoutBots botR botY 
    | otherwise = return (_status g0) 

----------------------------------------------------------------------
-- Bot Zero
----------------------------------------------------------------------

data BotZero = BotZero

instance Bot s BotZero where
    genmove _bot _time _game = return 0

----------------------------------------------------------------------
-- BotRandom
----------------------------------------------------------------------

newtype BotRandom s = BotRandom { randomGen :: GenST s }

instance Bot s (BotRandom s) where
    genmove (BotRandom gen) _ = randomMove gen

{-# INLINE randomMove #-}
randomMove :: GenST s -> Game s -> ST s Int
randomMove gen game = uniformR (0, nMovesGame game - 1) gen

----------------------------------------------------------------------
-- BotMc
----------------------------------------------------------------------

data BotMc s = BotMc
    { mcNsims :: Int
    , mcGen :: GenST s
    }

instance Bot s (BotMc s) where
    genmove (BotMc nsims gen) _time game =

        let aux ki k s = if ki == nMovesGame game then return k else do
                            si <- evalMove game gen nsims ki
                            if si>s then aux (ki+1) ki si else aux (ki+1) k s
        in aux 0 0 (-1)

evalMove :: Game s -> GenST s -> Int -> Int -> ST s Double
evalMove game0 gen nsims k = do
    let player0 = _currentPlayer game0
    game1 <- cloneGame game0 >>= playK k

    let aux 0 s = return s
        aux i s = do status2 <- cloneGame game1 >>= playoutRandom gen 
                     aux (i - 1) (s + computeScore player0 status2)
    aux nsims 0

playoutRandom :: GenST s -> Game s -> ST s Status
playoutRandom gen g0 
    | isRunning g0 = randomMove gen g0 >>= (`playK` g0) >>= playoutRandom gen
    | otherwise = return (_status g0)

computeScore :: Player -> Status -> Double
computeScore PlayerR WinR = 1.0
computeScore PlayerY WinY = 1.0
computeScore _ Tie = 0.5
computeScore _ _ = 0.0

----------------------------------------------------------------------
-- BotMcts
----------------------------------------------------------------------

mctsKuct :: Double
mctsKuct = 0.5

data BotMcts s = BotMcts
    { mctsNiters :: Int
    , mctsGen :: GenST s
    }

instance Bot s (BotMcts s) where
    genmove (BotMcts niters gen) _time game = do
        root <- mkRoot game 
        replicateM_ niters $ do
            leaf <- selectAndExpand root
            status <- simulate gen leaf
            backpropagate status leaf
        bestNode root

type NodeRef s = STRef s (Node s)

data Node s = Node 
    { nodeGame :: Game s
    , nodePlayer :: Player     -- before move
    , nodeNmoves :: Int
    , nodeParent :: Maybe (NodeRef s)
    , nodeReward :: STRef s Double 
    , nodeNsims :: STRef s Int
    , nodeLastI :: STRef s Int
    , nodeChildren :: VM.STVector s (Node s)
    }

mkRoot :: Game s -> ST s (Node s)
mkRoot = mkNode return Nothing

mkLeaf :: Int -> NodeRef s -> ST s (Node s)
mkLeaf k node = do
    game <- nodeGame <$> readSTRef node
    mkNode (playK k) (Just node) game

mkNode :: (Game s -> ST s (Game s)) -> Maybe (NodeRef s) -> Game s -> ST s (Node s)
mkNode gameFunc pNode game0 = do
    game1 <- cloneGame game0 >>= gameFunc
    let nMoves = nMovesGame game1
        player0 = _currentPlayer game0
    Node game1 player0 nMoves pNode 
        <$> newSTRef 0 <*> newSTRef 0 <*> newSTRef 0 <*> VM.new nMoves

bestNode :: Node s -> ST s Int
bestNode root = do
    lastI <- readSTRef (nodeLastI root)
    when (lastI < nodeNmoves root) (error "niters too low")
    let bestNodeFunc (nn,ii) i node = do
            nsims <- readSTRef (nodeNsims node)
            return $ if nsims > nn then (nsims, i) else (nn, ii)
    snd <$> VM.ifoldM' bestNodeFunc (-1, -1) (nodeChildren root)

ucb1 :: Double -> Int -> Int -> Double
ucb1 cReward cNsims pNsims =
    let cNsimsD = fromIntegral cNsims 
        exploitation = cReward / cNsimsD
        exploration = sqrt (log (fromIntegral $ 1 + pNsims) / cNsimsD)
    in exploitation + mctsKuct * exploration

selectUcb :: Node s -> ST s (Node s)
selectUcb node = do
    pNsims <- readSTRef (nodeNsims node)
    let children = nodeChildren node
    let bestUcb1Func (sk, k) i n = do
            reward <- readSTRef (nodeReward n)
            nsims <- readSTRef (nodeNsims n)
            let si = ucb1 reward nsims pNsims 
            return $ if si > sk then (si, i) else (sk, k)
    (_, k) <- VM.ifoldM' bestUcb1Func (-1, -1) children 
    VM.read children k

selectAndExpand :: Node s -> ST s (Node s)
selectAndExpand node = 
    if isRunning (nodeGame node)
    then do
        lastI <- readSTRef (nodeLastI node)
        if lastI < nodeNmoves node
        then do
            nodeRef <- newSTRef node
            cNode <- mkLeaf lastI nodeRef
            VM.write (nodeChildren node) lastI cNode
            modifySTRef' (nodeLastI node) (+1)
            return cNode
        else selectUcb node >>= selectAndExpand 
    else return node 

simulate :: Gen s -> Node s -> ST s Status
simulate gen node = cloneGame (nodeGame node) >>= playoutRandom gen 

backpropagate :: Status -> Node s -> ST s ()
backpropagate status node = do
    modifySTRef' (nodeReward node) (+ computeScore (nodePlayer node) status)
    modifySTRef' (nodeNsims node) (+1)
    forM_ (nodeParent node) (readSTRef >=> backpropagate status)

----------------------------------------------------------------------
-- BotIO
----------------------------------------------------------------------

class BotIO b where
    genmoveIO :: b -> Double -> Game RealWorld -> IO Int

computeEmptyCells :: Game s -> ST s Int
computeEmptyCells game = 
    let f acc x = if x==CellE then acc+1 else acc
    in A.foldlS f 0 <$> A.freezeS (_cells game)

----------------------------------------------------------------------
-- BotMcTimeIO
----------------------------------------------------------------------

newtype BotMcTimeIO = BotMcTimeIO GenIO

instance BotIO BotMcTimeIO where
    genmoveIO (BotMcTimeIO gen) time game = do
        t0 <- myGetTime
        emptyCells <- stToIO $ computeEmptyCells game
        let ktime = if time < 5.0 then 1.0 else 4.0  -- TODO params 
            tcell = ktime * time / fromIntegral emptyCells
            t1 = t0 + tcell - 0.5 
            nmoves = nMovesGame game
            player0 = _currentPlayer game

        -- create a vector for computing scores
        let genFunc :: Int -> IO (Double, Game RealWorld)
            genFunc k = stToIO $ do
                g1 <- cloneGame game >>= playK k
                return (0, g1)
        scores <- VM.generateM nmoves genFunc

        -- compute scores incrementally
        let loop :: Int -> IO Int
            loop n = do
                replicateM_ 1000 $ VM.iforM_ scores $ \k (s, g) -> do
                    status <- stToIO (cloneGame g >>= playoutRandom gen)
                    let sk = computeScore player0 status
                    VM.write scores k (sk+s, g)
                t <- myGetTime
                if t < t1 then loop (n+1) else return n

        n <- loop 0
        putStrLn $ "  mc loop: " <> show n
        putStrLn $ "  mc time: " <> show tcell

        -- find best score 
        s0 <- fst <$> VM.read scores 0
        let bestFunc (bs, bk) k (s, _) = if s>bs then (s,k) else (bs, bk)
        snd <$> VM.ifoldl' bestFunc (s0, 0) scores

----------------------------------------------------------------------
-- BotMctsTimeIO
----------------------------------------------------------------------

newtype BotMctsTimeIO = BotMctsTimeIO GenIO

instance BotIO BotMctsTimeIO where
    genmoveIO (BotMctsTimeIO gen) time game = do
        t0 <- myGetTime
        emptyCells <- stToIO $ computeEmptyCells game
        let ktime = if time < 5.0 then 1.0 else 4.0  -- TODO params 
            tcell = ktime * time / fromIntegral emptyCells
            t1 = t0 + tcell - 0.5 

        root <- stToIO $ mkRoot game 

        let loop :: Int -> IO Int
            loop n = do

                stToIO $ replicateM_ 1000 $ do
                    leaf <- selectAndExpand root
                    status <- simulate gen leaf
                    backpropagate status leaf

                t <- myGetTime
                if t < t1 then loop (n+1) else return n

        n <- loop 0
        putStrLn $ "  mcts loop: " <> show n
        putStrLn $ "  mcts time: " <> show tcell

        stToIO $ bestNode root

