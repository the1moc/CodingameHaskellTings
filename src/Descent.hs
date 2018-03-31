import           Control.Monad
import           Data.List
import           Data.Maybe
import           System.IO

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    loop

loop :: IO ()
loop = do
    mountains <- replicateM 8 $ do
        input_line <- getLine
        let mountainh = read input_line :: Int -- represents the height of one mountain.
        return (mountainh)
    putStrLn . show . fromMaybe (-1) $ elemIndex (maximum mountains) mountains
    loop
