{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric  #-}

module Network
  ( Mlp(..)
  , MlpConfig(..)
  , Neuron(..)
  , new
  , forward
  , forwardM
  , backpropagateM
  , learnM
  ) where

import           Control.Monad       (forM, forM_, zipWithM)
import           Control.Monad.ST
import           Data.List
import           Data.Maybe          (fromJust)
import           Data.Vector         (Vector, (!), (!?), (//))
import qualified Data.Vector         as Vec
import qualified Data.Vector.Mutable as MVec
import           Debug.Trace
import           GHC.Generics

import           Data.Aeson
import           System.Random       (StdGen)
import qualified System.Random       as Rand

data Neuron = Neuron
  { nOutput  :: Double
  , nErr     :: Double
  , nWeights :: [Double]
  , nInputs  :: [InputConnection]
  } deriving (Show, Generic)

instance FromJSON Neuron

instance ToJSON Neuron where
  toEncoding = genericToEncoding defaultOptions

type InputConnection = Int

type NeuronConnections = [InputConnection]

type LayerDims = [Int]

type NeuronVector = Vector Neuron

data Mlp = Mlp
  { mDims    :: LayerDims
  , mInputs  :: [Double]
  , mNeurons :: NeuronVector
  } deriving (Show, Generic)

instance FromJSON Mlp

instance ToJSON Mlp where
  toEncoding = genericToEncoding defaultOptions

type LearnData = ([Double], [Double])

learnM :: [LearnData] -> Mlp -> Mlp
learnM data' net =
  foldl (\net' (inputs, desireOutputs) -> backpropagateM desireOutputs (forwardM inputs net')) net data'

forward :: [Double] -> Mlp -> Mlp
forward inputs (Mlp dims _ neurons) =
  Mlp dims inputs $ foldl (\prev actual -> Vec.snoc prev (actual {nOutput = output actual prev})) Vec.empty neurons
  where
    output neuron prev = sigmoid (loadRefs neuron prev) (nWeights neuron)
    loadRefs neuron ready = map (`find` ready) (nInputs neuron)
    find ref ready
      | ref < 0 = inputs !! (abs ref - 1)
      | otherwise = nOutput $ ready ! ref

forwardM :: [Double] -> Mlp -> Mlp
forwardM inputs (Mlp dims _ neurons) =
  Mlp dims inputs $
  runST
    (do ns <- Vec.unsafeThaw neurons
        forM_ [0 .. size] $ \i -> do
          neuron <- MVec.unsafeRead ns i
          refs <- mapM (`find` ns) (nInputs neuron)
          let output = sigmoid refs (nWeights neuron)
          MVec.unsafeWrite ns i (neuron {nOutput = output, nErr = 0})
        Vec.unsafeFreeze ns)
  where
    size = length neurons - 1
    find ref ns
      | ref < 0 = return $ inputs !! (abs ref - 1)
      | otherwise = nOutput <$> MVec.unsafeRead ns ref

backpropagateM :: [Double] -> Mlp -> Mlp
backpropagateM desireOutput (Mlp dims inputs neurons) =
  Mlp dims inputs $
  runST
    (do ns <- Vec.unsafeThaw neurons
        let pOutput = zip [neuronsNum - outputsNum .. neuronsNum - 1] desireOutput
        forM_ pOutput $ \(i, d) -> MVec.modify ns (\n -> n {nErr = d - nOutput n}) i
        forM_ (reverse [0 .. neuronsNum - 1]) $ \i -> do
          actual <- MVec.unsafeRead ns i
          let zipped = zip (nInputs actual) (nWeights actual)
          newWeights <-
            forM zipped $ \(ref, weight) ->
              if ref >= 0
                then do
                  n <- MVec.unsafeRead ns ref
                  MVec.unsafeWrite ns ref $ n {nErr = nErr n + computeError actual weight}
                  return $ computeWeight actual weight (nOutput n)
                else return $ computeWeight actual weight (inputs !! (abs ref - 1))
          MVec.unsafeWrite ns i (actual {nWeights = newWeights})
        Vec.unsafeFreeze ns)
  where
    neuronsNum = length neurons
    outputsNum = length desireOutput

-- CREATION and HELPERS
data MlpConfig = MlpConfig
  { confBias         :: Bool
  , confDims         :: [Int]
  , confInputsNumber :: Int
  , confConnections  :: [NeuronConnections]
  } deriving (Show)

new :: StdGen -> MlpConfig -> IO Mlp
new stdGen (MlpConfig bias dims inputs cons) =
  return . Mlp dims [] . Vec.fromList . getRes $
  foldr (\c (gen, net) -> initNeuron (Rand.split gen) c net) (stdGen, []) cons
  where
    initNeuron (g1, g2) c net = (g1, Neuron 0 0 (initWeights g2 (length c)) c : net)
    initWeights g len = take len $ Rand.randomRs (-1, 1) g
    getRes (_, res) = res

empty :: Mlp
empty = Mlp [] [] Vec.empty

--PRIVATE
computeError :: Neuron -> Double -> Double
computeError n w = nErr n * w * (1 - nOutput n) * nOutput n

computeWeight :: Neuron -> Double -> Double -> Double
computeWeight neuron oldWeight yOutput = c --traceShow (show oldWeight ++ " :: " ++ show (ni * nErr neuron * (1 - nOutput neuron) * nOutput neuron * yOutput)) c
  where
    c = momentum * oldWeight + ni * nErr neuron * (1 - nOutput neuron) * nOutput neuron * yOutput
    ni = 0.3
    momentum = 1

sigmoid :: [Double] -> [Double] -> Double
sigmoid [] [] = 1
sigmoid inputs weights = 1 / (1 + exp (beta * sumI))
  where
    beta = -1
    sumI = sum $ zipWith (*) weights inputs
