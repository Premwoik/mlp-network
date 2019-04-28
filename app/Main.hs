module Main where

import Network
import System.Random (StdGen)
import qualified System.Random as Rand

main :: IO ()
main = do
  g <- Rand.newStdGen
--  let g = Rand.mkStdGen 10
--  let cons = createAllToAllConnections 5 [10, 100, 1000, 2000, 100, 10, 2]
  let cons = createAllToAllConnections (MlpConfig True [2, 1] 2 [])
  net <- new g cons
  let learnData = concat $ replicate 2000 [([1,0], [0, 0]), ([0,1], [0, 0]), ([0,0], [0, 0]), ([1,1], [1, 1])]
--  let res = forward [0, 0, 1, 0, 0] net
  putStrLn "----------------------FORWARD-------------------"
--  print $ getResult res
  printN net
--  printN res
  putStrLn "----------------------RESULTS-------------------"
  let learnedNet = learnM learnData net
  let fo = forward [1,0] learnedNet
  printN fo
  print $ getResult fo

  return ()

