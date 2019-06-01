module Network.Factory where

import           AutoEncoder
import qualified Data.AndLogic      as And
import qualified Data.Iris          as Iri
import qualified Data.Wine          as Wi
import           Network
import           Network.Helpers
import           Network.MlpEncoder
import           System.Random      (StdGen)
import qualified System.Random      as Rand

irisOptimal = kFoldFac (MlpConfig False [7, 5, 3] 4 []) 100 13 Iri.data'

wineOptimal = kFoldFac (MlpConfig False [13, 7, 3] 13 []) 100 7 Wi.data'

andLogicOptimal = andLogicFac (MlpConfig True [7, 4, 3, 2] 2 []) 1000 1

irisEncOptimal =
  let mlpConf = MlpConfig False [7, 5, 3] 6 []
      aeConf = AutoEncoderConfig [3, 2, 3] 4
      cConns = replicate 5 [-1, -2, -3, -4] ++ replicate 3 [-5, -6]
   in kFoldWithEncoderFac (MlpEnConfig mlpConf aeConf cConns) 100 10 Iri.data'

--        , [-5, -6]
--        , [-5, -6]
wineEncOptimal =
  let mlpConf = MlpConfig False [13, 7, 3] 17 []
      aeConf = AutoEncoderConfig [7, 4, 7] 13
      cConns = replicate 5 [-1, -2, -3, -4, -5, -7, -8, -9, -10, -11, -12, -13] ++ replicate 8 [-14, -15, -16, -17]
   in kFoldWithEncoderFac (MlpEnConfig mlpConf aeConf cConns) 100 7 Wi.data'

-- FACTORIES
kFoldFac :: MlpConfig -> Int -> Int -> IO [([Double], [Double])] -> IO [(Double, Mlp)]
kFoldFac conf epoch k data' = do
  g <- Rand.newStdGen
  net <- new g $ createAllToAllConnections conf
  map (learnAndTest net) . kFold g k . minMaxScaling <$> data'
  where
    addEpoch train = concat $ replicate epoch train
    learnAndTest net (train, test) = tester test $ learn (addEpoch train) net

kFoldWithEncoderFac :: MlpEnConfig -> Int -> Int -> IO [([Double], [Double])] -> IO [(Double, MlpEncoder)]
kFoldWithEncoderFac (MlpEnConfig mlpC aeC customCons) epoch k data' = do
  g <- Rand.newStdGen
  uData <- data'
  readyEnc <- unwrapEncoder . enStruct . learn uData <$> newEnc g aeC
  net <- new g finalCons
  let mlpEnc = MlpEncoder net readyEnc
  return $ map (learnAndTest mlpEnc) . kFold g k . minMaxScaling $ uData
  where
    finalCons = replaceConns 0 customCons $ createAllToAllConnections mlpC
    addEpoch train = concat $ replicate epoch train
    learnAndTest net (train, test) = tester test $ learn (addEpoch train) net

andLogicFac :: MlpConfig -> Int -> Int -> IO (Double, Mlp)
andLogicFac conf epoch genNetNum = do
  let cons = createAllToAllConnections conf
  learningData <- concat . replicate epoch <$> And.learningData
  testData <- And.testData
  findBestNet genNetNum cons learningData testData
