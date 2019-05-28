module Data.AndLogic(learningData, testData) where

datas =
    [([0,1], [0])
    ,([1,1], [1])
    ,([0,1], [0])
    ,([1,0], [0])
    ]


learningData :: IO [([Double], [Double])]
learningData = return datas


testData :: IO [([Double], [Double])]
testData = return datas

