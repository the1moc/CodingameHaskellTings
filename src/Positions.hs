module Positions where

import           Control.Monad
import           Data.List
import           Data.Maybe
import           System.IO

data Node = Node { valid                    :: Bool,
                   placement                :: Placement,
                   rightNeighbourPlacement  :: Placement,
                   bottomNeighbourPlacement :: Placement }
                   deriving Show

data Placement = Placement { position :: Int, row :: Int  }
  deriving Show
  -- deriving (Show)

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE

    input_line <- getLine
    let width = read input_line :: Int -- the number of cells on the X axis
    input_line <- getLine
    let height = read input_line :: Int -- the number of cells on the Y axis

    lines <- replicateM height $ do
        line <- getLine
        let unformedNodesWithIndexes = zip [0..] line
        return unformedNodesWithIndexes

    let linesWithIndexes = zip [0..] lines
    let nodes = map createNodes linesWithIndexes
          where createNodes (lineIndex, unformedNodes) = map (\node -> createNode node lineIndex) unformedNodes
    let validNodes = filter (\n -> isNodeValid n) $ flattenNodes nodes
    let nodeStrings = map (\n -> generateNodeString n nodes) validNodes
    mapM_ putStrLn nodeStrings
    return ()

-- Create a node with a given position and validity.
createNode :: (Int, Char) -> Int -> Node
createNode (i, '0') lineIndex = Node { valid = True,
                             placement = Placement { position = i, row = lineIndex },
                             rightNeighbourPlacement = Placement { position = i + 1, row = lineIndex },
                             bottomNeighbourPlacement = Placement { position = i, row = lineIndex + 1 }  }
createNode (i, '.') lineIndex = Node { valid = False,
                             placement = Placement { position = i, row = lineIndex },
                             rightNeighbourPlacement = Placement { position = i + 1, row = lineIndex },
                             bottomNeighbourPlacement = Placement { position = i, row = lineIndex + 1 } }


flattenNodes :: [[Node]] -> [Node]
flattenNodes = concat

isNodeValid :: Node -> Bool
isNodeValid (Node True _ _ _) = True
isNodeValid (Node False _ _ _) = False

generateNodeString :: Node -> [[Node]] -> String
generateNodeString node nodes = thisNodePosition ++ rightNodePosition ++ bottomNodePosition
    where thisNodePosition = (show $ row (placement node)) ++ " " ++ (show $ position (placement node)) ++ " "
          rightNodePosition =
            let rightRow = safeLookup (row (rightNeighbourPlacement node)) nodes
                in case rightRow of
                    Nothing -> "-1 -1 "
                    Just a  -> let rightPosition = safeLookup (position (rightNeighbourPlacement node)) $ fromJust rightRow
                                  in case rightPosition of
                                      Nothing -> "-1 -1 "
                                      Just b -> case valid b of
                                          True -> (show $ position (placement b)) ++ " " ++ (show $ row (placement b)) ++ " "
                                          False -> "-1 -1 "
          bottomNodePosition =
            let rightRow = safeLookup (row (bottomNeighbourPlacement node)) nodes
              in case rightRow of
                Nothing -> "-1 -1"
                Just a  -> let bottomPosition = safeLookup (position (bottomNeighbourPlacement node)) $ fromJust rightRow
                              in case bottomPosition of
                                  Nothing -> "-1 -1"
                                  Just b -> case valid b of
                                      True -> (show $ position (placement b)) ++ " " ++ (show $ row (placement b))
                                      False -> "-1 -1"


getValue :: Node -> String
getValue (Node a b c d) = show a ++ " " ++ show b ++ " " ++  show c ++ " " ++ show d

-- Safe index function to replace shitty !!
safeLookup :: Int -> [a] -> Maybe a
safeLookup _ []       = Nothing
safeLookup (-1) _     = Nothing
safeLookup 0 (x : _)  = Just x
safeLookup i (_ : xs) = safeLookup (i - 1) xs
