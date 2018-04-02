module Thor where

import           Control.Monad
import           System.IO

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE

    input_line <- getLine
    let input = words input_line
    let lightx = read (input!!0) :: Int -- the X position of the light of power
    let lighty = read (input!!1) :: Int -- the Y position of the light of power
    let initialtx = read (input!!2) :: Int -- Thor's starting X position
    let initialty = read (input!!3) :: Int -- Thor's starting Y position
    loop (lightx - initialtx) (lighty - initialty)

loop :: Int -> Int -> IO ()
loop diffx diffy = do
  input_line <- getLine
  let remainingturns = read input_line :: Int -- The remaining amount of turns Thor can move. Do not remove this line.
  let hd = if diffx > 0 then "E" else
                            case diffx of
                            0 -> ""
                            _ -> "W"
  let newX
        | diffx > 0 = (diffx - 1)
        | diffx < 0 = (diffx + 1)
        | otherwise =  0

  let vd = if diffy > 0 then "S" else
                            case diffy of
                            0 -> ""
                            _ -> "N"
  let newY
        | diffy > 0 = (diffy - 1)
        | diffy < 0 = (diffy + 1)
        | otherwise =  0
  putStrLn (vd ++ hd)
  loop newX newY
