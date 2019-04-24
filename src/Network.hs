module Network
  ( Mlp(..)
  , createAllToAllConnections
  , new
  , printN
  , forward
  , getResult
  ) where

import           Data.List
import           Data.Maybe          (fromJust)
import           Data.Vector         (Vector, (!), (!?))
import qualified Data.Vector         as Vec
import qualified Data.Vector.Mutable as MVec
import           Debug.Trace

import           System.Random       (StdGen)
import qualified System.Random       as Rand

data Neuron = Neuron
  { nOutput  :: Float
  , nErr     :: Float
  , nWeights :: [Float]
  , nInputs  :: [InputConnection]
  } deriving (Show)

type InputConnection = Int

type NeuronConnections = [InputConnection]

type LayerDims = [Int]

type NeuronVector = Vector Neuron

data Mlp =
  Mlp LayerDims NeuronVector
  deriving (Show)

createAllToAllConnections :: Int -> LayerDims -> (LayerDims, [NeuronConnections])
createAllToAllConnections inputsNumb dims =
  ( dims
  , reverse $
    concat $
    getRes $
    foldl
      (\(index, conns, res) size -> (index + size, [index .. index + size - 1], replicate size conns : res))
      acc
      dims)
  where
    acc = (0, [-inputsNumb .. (-1)], [])
    getRes (_, _, res) = res

getResult :: Mlp -> [Float]
getResult (Mlp dims neurons) =
  Vec.toList . Vec.map nOutput $ outputsNeurons
  where
    outputsNeurons = Vec.slice (len - outputsNumber) outputsNumber neurons
    outputsNumber = last dims
    len = length neurons

--Vec.snoc has complexity O(n). Maybe there is any option to change it to 0(1).
-- Use mutable vector and modify instead of append?
forward :: [Float] -> Mlp -> Mlp
forward inputs (Mlp dims neurons) = Mlp dims $ foldl (\prev actual -> Vec.snoc prev (newActual actual prev)) Vec.empty neurons
  where
    newActual actual ready = actual {nOutput = sigmoid (loadRefs actual ready) (nWeights actual)}
    loadRefs neuron ready = map (`find` ready) (nInputs neuron)
    find ref ready
      | ref < 0 = inputs !! (abs ref - 1)
      | otherwise = nOutput $ ready ! ref

new :: StdGen -> (LayerDims, [NeuronConnections]) -> IO Mlp
new stdGen (dims, cons) =
  return . Mlp dims . Vec.fromList . getRes $ foldr (\c (gen, net) -> initNeuron (Rand.split gen) c net) (stdGen, []) cons
  where
    initNeuron (g1, g2) c net = (g1, Neuron 0 0 (initWeights g2 (length c)) c : net)
    initWeights g len = take len $ Rand.randomRs (0, 1) g
    getRes (_, res) = res

printN :: Mlp -> IO ()
printN (Mlp _ arr) = mapM_ print arr

empty :: Mlp
empty = Mlp [] Vec.empty

--PRIVATE
sigmoid :: [Float] -> [Float] -> Float
sigmoid inputs weights = 1 / (1 + exp (beta * sumI))
  where
    beta = -1
    sumI = sum $ zipWith (*) weights inputs
