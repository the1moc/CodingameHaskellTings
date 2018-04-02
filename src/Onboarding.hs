module Onboarding where

import           Control.Monad
import           System.IO

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE

    -- CodinGame planet is being attacked by slimy insectoid aliens.
    -- <---
    -- Hint:To protect the planet, you can implement the pseudo-code provided in the statement, below the player.

    loop

loop :: IO ()
loop = do
    input_line <- getLine
    let enemy1 = input_line :: String -- name of enemy 1
    input_line <- getLine
    let dist1 = read input_line :: Int -- distance to enemy 1
    input_line <- getLine
    let enemy2 = input_line :: String -- name of enemy 2
    input_line <- getLine
    let dist2 = read input_line :: Int -- distance to enemy 2
    putStrLn $ if dist2 > dist1 then enemy1 else enemy2
    loop
