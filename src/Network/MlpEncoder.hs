module Network.MlpEncoder where

import qualified AutoEncoder     as AE
import Network
import           Network.Helpers (getResult)
import           System.Random   (StdGen)
import Debug.Trace

data MlpEnConfig = MlpEnConfig
  { mlpConfig   :: MlpConfig
  , aeConfig    :: AE.AutoEncoderConfig
  , customConns :: [NeuronConnections] --only first layer
  }

data MlpEncoder = MlpEncoder
  { mlp :: Mlp
  , ae  :: Mlp
  }

instance Network MlpEncoder where
  backpropagate desOut (MlpEncoder mlp ae) = MlpEncoder (backpropagate desOut mlp) ae

  learn data' net = foldl (\net' (inputs, desireOutputs) -> backpropagate desireOutputs (forward inputs net')) net data'

  forward data' (MlpEncoder mlp ae) = MlpEncoder (forward mlpInput mlp) ae
    where
      mlpInput = data' ++ getResult (forward data' ae)

  getResult (MlpEncoder mlp _) = getResult mlp
