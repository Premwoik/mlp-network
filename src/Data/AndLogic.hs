module Data.AndLogic
  ( learningData
  , testData
  ) where

data' = [([0, 1], [0]), ([1, 1], [1]), ([0, 1], [0]), ([1, 0], [0])]

learningData :: IO [([Double], [Double])]
learningData = return data'

testData :: IO [([Double], [Double])]
testData = return data'
