import           Control.Monad
import           Data.List
import           Data.Maybe
import           System.IO

data Coordinate = Coord Int Int Int Int Int Int
  deriving Show

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE

    -- Don't let the machines win. You are humanity's last hope...

    input_line <- getLine
    let width = read input_line :: Int -- the number of cells on the X axis
    input_line <- getLine
    let height = read input_line :: Int -- the number of cells on the Y axis

    lines <- replicateM height $ do
        line <- getLine
        let zippedLine = zip [0..] line
        return (zippedLine)
    let zippedLines = zip [0..] lines
    -- Three coordinates: a node, its right neighbor, its bottom neighbor
    let results = map (\(lineIndex, line) -> checkPositions lineIndex line) zippedLines
          where checkPositions lineIndex line = map (\node -> checkPosition node lineIndex width height) line
    mapM_ (\l -> mapM_ (\x -> putStrLn $ getValue $ fromJust x) l) results
    return ()

checkPosition :: (Int, Char) -> Int -> Int -> Int -> Maybe Coordinate
-- checkPosition '.' _ _ _ _ = Nothing
checkPosition (index, node) row width height
  | index == 0 && row == 0 = Just $ Coord index row (index + 1) row index (row + 1)
  | index == 0 && row == (height - 1) = Just $ Coord index row (index + 1) row (-1) (-1)
  | index == (width - 1) && row == (height - 1) = Just $ Coord index row (-1) (-1) (-1) (-1)
  | index == (width - 1) && row == 0 = Just $ Coord index row (-1) (-1) index (row + 1)
  | otherwise = Just $ Coord index row (index + 1) row index (row + 1)

getValue :: Coordinate -> String
getValue (Coord a b c d e f) = show a ++ " " ++ show b ++ " " ++  show c ++ " " ++ show d ++ " " ++  show e ++ " " ++  show f
