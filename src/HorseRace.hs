module HorseRace where

import           Control.Monad
import           Data.List
import           System.IO

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE

    -- Auto-generated code below aims at helping you parse
    -- the standard input according to the problem statement.

    input_line <- getLine
    let n = read input_line :: Int

    horsePowers <- replicateM n $ do
                          input_line <- getLine
                          let pi = read input_line :: Int
                          return pi
    let highestValue = maximum $ map abs $differences $ sort horsePowers
              where differences [] = []
                    differences [x] = []
                    differences [x,y] = [x - y]
                    differences (x:y:xs) = x - y : differences(y:xs)

    -- Write answer to stdout
    print highestValue
    return ()
