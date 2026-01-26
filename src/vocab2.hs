{-
    In Haskell, we tend to express our ideas in types and functions first and then proceed with implementing them.
-}
import Data.Char
import Data.List (group, sort)
import qualified Data.Text  as T
import qualified Data.Text.IO as TIO
import System.Environment -- Module for reading commandline arguments into a list of strings


type Entry = (T.Text, Int)      -- One vocabulary entry
type Vocabulary = [Entry]

extractVocab :: T.Text -> Vocabulary
extractVocab t = map buildEntry $ group $ sort ws
    where
        ws = map T.toCaseFold $ filter (not . T.null) $ map cleanWord $ T.words t
        buildEntry xs@(x:_) = (x, length xs)
        cleanWord = T.dropAround (not.isLetter)

printAllWords :: Vocabulary -> IO ()
printAllWords vocab = do
    putStrLn "All words: "
    TIO.putStrLn $ T.unlines $ map fst vocab

processTextFile :: FilePath -> IO ()
processTextFile fname = do
    text <- TIO.readFile fname
    let vocab = extractVocab text
    printAllWords vocab

main :: IO ()
main = do
    args <- getArgs
    case args of
        [fname] -> processTextFile fname
        _ -> putStrLn "Usage: vocab-builder filename"
