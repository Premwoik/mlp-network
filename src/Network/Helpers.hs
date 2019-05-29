module Network.Helpers
  ( showNet
  , resAll
  , tester
  , minMaxScaling
  , kFold
  , kFoldSummary
  , findBestNet
  , printNet
  , splits
  , getResult
  , createAllToAllConnections
  ) where

import           Control.Monad        (forM, forM_, zipWithM, zipWithM_)
import           Data.Aeson
import qualified Data.ByteString.Lazy as BS
import           Data.List.Split      (chunksOf)
import           Data.Maybe
import qualified Data.Vector          as Vec
import           Debug.Trace
import           Network
import           System.Random        (StdGen)
import qualified System.Random        as Rand
import           Data.List
import           Data.Ord
import VectorShuffling.Immutable

createAllToAllConnections :: MlpConfig -> MlpConfig
createAllToAllConnections conf@(MlpConfig True dims inputsNum _) =
  makeRes $
  foldl
    (\(index, conns, res) size -> (index + size + 1, [index .. index + size], ([] : replicate size conns) : res))
    (1, [-inputsNum .. 0], [])
    dims
  where
    makeRes (_, _, res) = conf {confConnections = [] : concat (reverse res)}
createAllToAllConnections conf@(MlpConfig False dims inputsNum _) =
  makeRes $
  foldl
    (\(index, conns, res) size -> (index + size, [index .. index + size - 1], replicate size conns : res))
    (0, [-inputsNum .. (-1)], [])
    dims
  where
    makeRes (_, _, res) = conf {confConnections = concat (reverse res)}

getResult :: Mlp -> [Double]
getResult (Mlp dims _ neurons) = Vec.toList . Vec.map nOutput $ outputsNeurons
  where
    outputsNeurons = Vec.slice (len - outputsNumber) outputsNumber neurons
    outputsNumber = last dims
    len = length neurons

printNet :: Mlp -> IO ()
printNet (Mlp _ _ arr) = mapM_ print arr

showNet :: ([Double], [Double]) -> Mlp -> IO ()
showNet (d, res) net = printNet $ forwardM d net

--findBestNet :: Int -> -> IO ()
findBestNet i cons learnData testData =
  fmap
    findBest
    (forM [1 .. i] $ \_ -> do
       g <- Rand.newStdGen
       net <- new g cons
       let learnedNet = learnM learnData net
       return $ tester testData learnedNet)
  where
    findBest = maximumBy (comparing fst)

resAll :: [([Double], [Double])] -> Mlp -> IO ()
resAll data' mlp = mapM_ (\(i, o) -> print (getResult (forwardM i mlp))) data'

tester :: [([Double], [Double])] -> Mlp -> (Double, Mlp)
tester data' mlp = makeRes $ sum $ map (\(i, destOutput) -> compare destOutput (getResult (forwardM i mlp))) data'
  where
    makeRes s = ((s / fromIntegral (length data')) * 100, mlp)
    compare dest res
      | otherwise =
        if snd (maxi dest) == snd (maxi res)
          then 1
          else 0

--      | traceShow (show dest ++ " | " ++ show res) False = undefined
normalize :: ([Double], [Double]) -> ([Double], [Double])
normalize (input, d) = (normalizeInput input, d)

normalizeInput :: [Double] -> [Double]
normalizeInput input = map (\x -> (x - minimum input) / v) input
  where
    v = maximum input - minimum input

minMaxScaling :: [([Double], [Double])] -> [([Double], [Double])]
minMaxScaling d = zipWith (\n (_, c) -> (n, c)) (scale (map fst d)) d
  where
    scale inputs =
      let maxMin = findMaxMin inputs
       in map (zipWith (\(max', min') val -> (val - min') / (max' - min')) maxMin) inputs

findMaxMin :: [[Double]] -> [(Double, Double)]
findMaxMin d = foldl (zipWith find) initAcc d
  where
    initAcc = map (\x -> (x, x)) $ head d
    find (max', min') val
      | val > max' = (val, min')
      | val < min' = (max', val)
      | otherwise = (max', min')

kFoldSummary :: [(Double, Mlp)] -> IO ()
kFoldSummary l = do
  zipWithM_ (\i (v, _) -> putStrLn ("K=" ++ show i ++ " accuracy: " ++ show v)) [1 ..] l
  putStrLn $ "Summary accuracy: " ++ show sumUpRes
  where
    sumUpRes = sum (map fst l) / len
    len = fromIntegral $ length l

kFold :: StdGen->  Int -> [([Double], [Double])] -> [([([Double], [Double])], [([Double], [Double])])]
kFold g k d = map prepareFold [0 .. k - 1]
  where
    prepareFold i = (\(train, test) -> (concat train, test)) (remove i split')
    split' = map (\x -> Vec.toList (fst (shuffle (Vec.fromList x) g))) (splits k d)

splits :: Int -> [([Double], [Double])] -> [[([Double], [Double])]]
splits k d = foldl (\acc x -> zipWith (++) (chunksOf (num x) x) acc) initAcc grouped
  where
    initAcc = replicate k []
    num x = ceiling $ fromIntegral (length x) / fromIntegral k
    grouped = groupBy (\(_, b) (_, b') -> b == b') d

remove :: Int -> [a] -> ([a], a)
remove i items = traceShow (show (length items)) (take i items ++ drop (1 + i) items, items !! i)

maxi xs = maximumBy (comparing fst) (zip xs [0 ..])
