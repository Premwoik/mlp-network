module Main where

import           Data.AndLogic        as And
import qualified Data.ByteString.Lazy as BS
import           Data.Vector
import           Debug.Trace
import qualified Network.Factory      as Factory
import           Network.Helpers

main :: IO ()
main = do
--  res <- Factory.irisOptimal
--  (bRes, bNet) <- Factory.andLogicOptimal
  res <- Factory.wineOptimal
  putStrLn "RESULT:"
  kFoldSummary res
  return ()

-- PRIVATE
--  BS.writeFile "net" $ encode bNet
--  readNet <- BS.readFile "net"
