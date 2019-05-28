module Network.Helpers
  ( showNet
  , resAll
  , tester
  , normalizeInput
  , normalize
  , findBestNet
  ) where

import           Control.Monad        (forM, forM_, zipWithM)
import           Data.Aeson
import qualified Data.ByteString.Lazy as BS
import qualified Data.Iris            as Iris
import           Data.Maybe
import qualified Data.Wine            as Wine
import           Debug.Trace
import           Network
import           System.Random        (StdGen)
import qualified System.Random        as Rand

import           Data.List
import           Data.Ord

showNet :: ([Double], [Double]) -> Mlp -> IO ()
showNet (d, res) net = printN $ forwardM d net

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
resAll data' mlp = mapM_ (\(i, o) -> trace (show o) $ print (getResult (forwardM i mlp))) data'

tester :: [([Double], [Double])] -> Mlp -> (Double, Mlp)
tester data' mlp =
  makeRes $ sum $ map (\(i, destOutput) -> compare destOutput (getResult (forwardM (normalizeInput i) mlp))) data'
  where
    makeRes s = ((s / (fromIntegral (length data'))) * 100, mlp)
    compare dest res =
      if snd (maxi dest) == snd (maxi res)
        then 1
        else 0

normalize :: ([Double], [Double]) -> ([Double], [Double])
normalize (input, d) = (normalizeInput input, d)

normalizeInput :: [Double] -> [Double]
normalizeInput input = map (\x -> (x - minimum input) / v) input
  where
    v = maximum input - minimum input

maxi xs = maximumBy (comparing fst) (zip xs [0..])
