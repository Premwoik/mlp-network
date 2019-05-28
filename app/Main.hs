module Main where

import qualified Data.ByteString.Lazy as BS
import           Debug.Trace
import qualified Network.Factory      as Factory
import           Network.Helpers

main :: IO ()
main = do
  (bRes, bNet) <- Factory.irisOptimal
--    (bRes, bNet) <- Factory.wineOptimal
  putStrLn "RESULT:"
  printNet bNet
  print bRes
  return ()
-- PRIVATE
--  BS.writeFile "net" $ encode bNet
--  readNet <- BS.readFile "net"
