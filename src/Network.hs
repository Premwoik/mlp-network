module Network (NetworkConn(..), reverseConn, forward, getResult, initializeNet, createConn, testForward, testBackward) where

import qualified Data.Vector as Vec
import Data.Vector ((!?), Vector)
import Data.Maybe (fromJust)
import Data.List
import Debug.Trace

import qualified System.Random as Rand

data Perc = Perc {output :: Float, err :: Float, weights :: [Float], dWeights :: [Float]}

type Input = [Float]

data NetworkConn = Default [Int] | CustomConn [[[Connection]]]

instance Show NetworkConn where
  show a = show "123"

type Connection = (Int, Int)

data Layer = Layer {lPers :: (Vector Perc), lForwardConn :: [[Connection]], lBackwardConn :: [[Connection]]}

data Mlp = Mlp (Vector Layer)


initializeNet :: NetworkConn -> IO Mlp
initializeNet (Default sizes) =
  initializeNet (CustomConn (createConn 0 sizes))
initializeNet (CustomConn conns) = return $ Mlp $ Vec.empty


createConn :: Int -> [Int] -> [[[Connection]]]
createConn _ (_ : []) = []
createConn i (s:s1:ss) =
  consInLayer : createConn (i+1) (s1:ss)
  where
    consInLayer  = [percCons | _ <- [1..s1]]
    percCons = [(i, j) | j <- [0..s-1]]

reverseConn :: [[[Connection]]] -> [[[Connection]]]
reverseConn c =
  let
--    take :: Int -> Int -> [Connection]
    take l p = (c !! l) !! p
--    find :: Connection -> [Connection] -> Bool
    find con cons =
      (any (\c' -> c' == con) cons)
    layerSize l = length (c !! l)
    layersNum = length c
    scan c'@(l', _) =
      [(l, p) | l <- [l' .. layersNum - 1], p <- [0.. layerSize l - 1], find c' (take l p)]
  in
  [[scan (l,p) | p <- [0 .. layerSize l - 2]] | l <- [0 .. layersNum - 1]]


reverseConn2 :: [Int] -> [[[Connection]]] -> [[[Connection]]]
reverseConn2 sizes cons =

 p

testBackward :: [[[Connection]]]
testBackward =
  [ [ [(0, 0), (0, 1)] -- 1, 0
    , [(0, 0), (0, 1)] -- 1, 1
    , [(0, 0), (0, 1)] -- 1, 2
    ]
  , [ [(1,0), (1,1), (1,2)] -- 2, 0
    , [(1,0), (1,1), (1,2)] -- 2, 1
    ]
  ]

testForward :: [[[Connection]]]
testForward =
  [ [ [(1,0), (1,1), (1,2)] -- 0, 0
    , [(1,0), (1,1), (1,2)] -- 0, 1
    ]
  , [ [(2,0), (2,1)] -- 1,0
    , [(2,0), (2,1)] -- 1,1
    , [(2,0), (2,1)] -- 1,2
    ]
  ]



getResult :: Mlp -> [Float]
getResult (Mlp layers) =
  Vec.toList . Vec.map output . lPers . Vec.last $ layers



--i assumed that first layer is first in the array
forward :: Input -> Mlp -> Mlp
forward input mlp@(Mlp layers) =
  Mlp $ Vec.map activateLayer layers
  where
    activateLayer layer@(Layer percs conn _) =
      layer {lPers = Vec.fromList $ zipWith activate (Vec.toList percs) conn}
    activate perc conn =
      sigmoid (findYs conn) perc
    findYs c =
      map output (fromJust (findCon c mlp))

--PRIVATE

sigmoid :: [Float] -> Perc -> Perc
sigmoid input perc =
  Perc output 0 weights' []
  where 
    weights' = weights perc
    sumI = sum $ zipWith (\x y -> x * y) weights' input
    beta = -1
    output = 1 / (1 + exp (beta * sumI))

findCon :: [Connection] -> Mlp -> Maybe [Perc]
findCon cons (Mlp layers) = mapM get cons 
  where 
    get (i, j) = layers !? i >>= (\layer -> (lPers layer) !? j)


