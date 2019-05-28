module Main where

import Network
import qualified Network.Factory as Factory
import qualified Data.ByteString.Lazy as BS
import Debug.Trace





main :: IO ()
main = do
--    (bRes, bNet) <- Factory.irisOptimal
    (bRes, bNet) <- Factory.wineOptimal


    putStrLn "RESULT:"
    printN bNet
    print bRes
    return ()

-- PRIVATE

--  BS.writeFile "net" $ encode bNet
--  readNet <- BS.readFile "net"



