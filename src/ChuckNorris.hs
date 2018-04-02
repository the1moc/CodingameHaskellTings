{-# LANGUAGE OverloadedStrings #-}

module ChuckNorris where

import           Control.Monad
import           Data.ByteString.Char8 (pack)
import           Data.List.Split
import           Data.Text             (unpack)
import           Data.Text.Encoding
import           System.IO

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE

    -- Auto-generated code below aims at helping you parse
    -- the standard input according to the problem statement.
    message <- getLine
    let decodedMessage = unpack $ decodeASCII $ pack message
    putStrLn . concatMap generateString $ separate decodedMessage
                    where generateString ('0':xs) = "00 0" ++ xs ++ " "
                          generateString ('1':xs) = "0 0" ++ take (length xs) (cycle "0") ++ " "

separate :: String -> [String]
separate [] = []
separate ['0'] = ["00 0"]
separate ['1'] = ["0 0"]
separate ('0':xs) = ('0' : takeWhile (=='0') xs) : separate (dropWhile (=='0') xs)
separate ('1':xs) = ('1' : takeWhile (=='1') xs) : separate (dropWhile (=='1') xs)
