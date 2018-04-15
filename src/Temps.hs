module Temps where

import           Control.Monad
import           Data.List     (elemIndex, elemIndices)
import           Data.Maybe
import           System.IO

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE

    -- Auto-generated code below aims at helping you parse
    -- the standard input according to the problem statement.

    input_line <- getLine
    let n = read input_line :: Int -- the number of temperatures to analyse
    input_line <- getLine
    let input = words input_line

    lines <- forM [0..(n-1)] $ \i -> do
        let t = read (input!!i) :: Int -- a temperature expressed as an integer ranging from -273 to 5526
        return t
    let absLine = map abs lines
    let minimumValueIndexes = elemIndices (minimum absLine) absLine
    let values = map (\x -> lines !! x) minimumValueIndexes
    if null lines then print "0" else print $ maximum values
    return ()
