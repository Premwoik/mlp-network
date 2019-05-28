module Network.Factory where

import Network
import System.Random (StdGen)
import qualified Data.Iris as Iri
import qualified Data.Wine as Wi
import Network.Helpers


irisOptimal = iris (MlpConfig False [4, 3] 4 []) 400 1

iris :: MlpConfig -> Int -> Int -> IO (Double, Mlp)
iris conf repl genNetNum = do
  let cons = createAllToAllConnections conf
  learningData <- concat . replicate repl . map normalize <$> Iri.learningData
  testData <- map normalize <$> Iri.testData
  findBestNet genNetNum cons learningData testData


wineOptimal = wine (MlpConfig False [13, 3] 13 []) 400 10

wine:: MlpConfig -> Int -> Int -> IO (Double, Mlp)
wine conf repl genNetNum = do
  let cons = createAllToAllConnections conf
  learningData <- concat . replicate repl . map normalize <$> Wi.learningData
  testData <- map normalize <$> Wi.testData
  findBestNet genNetNum cons learningData testData




