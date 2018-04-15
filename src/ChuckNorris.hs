{-# LANGUAGE OverloadedStrings #-}

module ChuckNorris where

import           Control.Monad
import           Data.Char
import           Data.List.Split
import           Data.Text.Encoding
import           System.IO

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering -- DO NOT REMOVE

    -- Auto-generated code below aims at helping you parse
      -- the standard input according to the problem statement.
  message <- getLine
  let decodedMessage = concatMap (updateLength . toBinary . ord) message
  putStrLn . init $ concatMap generateString $ separate decodedMessage
                    where generateString ('0':xs) = "00 0" ++ xs ++ " "
                          generateString ('1':xs) = "0 0" ++ replicate (length xs) '0' ++ " "

separate :: String -> [[Char]]
separate [] = []
separate ['0'] = ["0"]
separate ['1'] = ["1"]
separate ('0':xs) = ('0' : takeWhile (=='0') xs) : separate (dropWhile (=='0') xs)
separate ('1':xs) = ('1' : takeWhile (=='1') xs) : separate (dropWhile (=='1') xs)

updateLength :: String -> String
updateLength s
  | length s > 7 = tail s
  | length s == 7 = s
  | otherwise = take (length s - 7) (cycle "0") ++ s

toBinary :: Int -> String
toBinary 0 = "0"
toBinary n
  | n `mod` 2 == 1 = toBinary (n `div` 2) ++ "1"
  | n `mod` 2 == 0 = toBinary (n `div` 2) ++ "0"
