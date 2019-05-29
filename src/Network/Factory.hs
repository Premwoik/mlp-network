module Network.Factory where

import qualified Data.AndLogic   as And
import qualified Data.Iris       as Iri
import qualified Data.Wine       as Wi
import           Network
import           Network.Helpers
import           System.Random   (StdGen)
import qualified System.Random   as Rand

irisOptimal = kFoldFac (MlpConfig False [7, 5, 3] 4 []) 100 10 Iri.data'

wineOptimal = kFoldFac (MlpConfig False [13, 7, 3] 13 []) 100 7 Wi.data'

kFoldFac :: MlpConfig -> Int -> Int -> IO [([Double], [Double])] -> IO [(Double, Mlp)]
kFoldFac conf epoch k data' = do
  g <- Rand.newStdGen
  net <- new g $ createAllToAllConnections conf
  map (learnAndTest net) . kFold g k . minMaxScaling <$> data'
  where
    addEpoch train = concat $ replicate epoch train
    learnAndTest net (train, test) = tester test $ learnM (addEpoch train) net

andLogicOptimal = andLogic (MlpConfig True [7, 4, 3, 2] 2 []) 1000 1

andLogic :: MlpConfig -> Int -> Int -> IO (Double, Mlp)
andLogic conf epoch genNetNum = do
  let cons = createAllToAllConnections conf
  learningData <- concat . replicate epoch <$> And.learningData
  testData <- And.testData
  findBestNet genNetNum cons learningData testData
