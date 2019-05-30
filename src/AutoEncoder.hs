module AutoEncoder where

import qualified Network as N
import Network(Mlp, MlpConfig(..))
import Network.Helpers(createAllToAllConnections)
import System.Random(StdGen)

data AutoEncoderConfig = AutoEncoderConfig {enConfHidden :: [Int], ecConfInputsNumber :: Int}
data AEnc= AEnc {enType :: Type, enStruct :: Mlp}

data Type = EncoderDecoder | Encoder | Decoder


forwardM :: [Double] -> AEnc-> AEnc
forwardM d t@(AEnc type' mlp) = AEnc type' $ N.forwardM d mlp

backpropagateM :: [Double] -> AEnc -> AEnc
backpropagateM desO t@(AEnc type' mlp) = AEnc type' $ N.backpropagateM desO mlp

learnM :: [[Double]] -> AEnc -> AEnc
learnM d t@(AEnc type' mlp) =
  AEnc type' $ N.learnM (map (\x -> (x,x)) d) mlp

new :: StdGen -> AutoEncoderConfig -> IO AEnc
new stdGen conf@(AutoEncoderConfig hid inputsNum) =
  AEnc EncoderDecoder <$> N.new stdGen toMlpConfig
  where
    toMlpConfig = createAllToAllConnections $ MlpConfig False layers inputsNum []
    layers = (inputsNum : hid) ++ [inputsNum]

unwrapEncoder :: Mlp -> Mlp
unwrapEncoder m = N.empty

unwrapDecoder :: Mlp -> Mlp
unwrapDecoder m = N.empty

