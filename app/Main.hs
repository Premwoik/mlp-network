module Main where

import Network
import System.Random (StdGen)
import qualified System.Random as Rand

main :: IO ()
main = do
  g <- Rand.newStdGen
  let cons = createAllToAllConnections 5 [10, 100, 1000, 200, 100, 10, 2]
  print $ length $ snd cons
  net <- new g cons
  let res = forward [0, 0, 1, 0, 0] net
--  printN net
  putStrLn "----------------------FORWARD-------------------"
--  printN res
  putStrLn "----------------------RESULTS-------------------"
  print $ getResult res
  return ()

