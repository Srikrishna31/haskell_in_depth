{-
    In Haskell, we tend to express our ideas in types and functions first and then proceed with implementing them.
-}
{- To avoid explicit calls to the T.pack function for String literals by teaching the compiler to regard String literals as values of type Text. -}
{-# LANGUAGE OverloadedStrings #-}

import Data.Char
import Data.List (group, sort, sortBy)
import Data.Ord
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment -- Module for reading commandline arguments into a list of strings
import Control.Monad (when)
import Fmt

type Entry = (Text, Int) -- One vocabulary entry

type Vocabulary = [Entry]

extractVocab :: Text -> Vocabulary
extractVocab t = map buildEntry $ group $ sort ws
  where
    ws = map T.toCaseFold $ filter (not . T.null) $ map cleanWord $ T.words t
    buildEntry xs@(x : _) = (x, length xs)
    cleanWord = T.dropAround (not . isLetter)

allWordsReport :: Vocabulary -> Text
allWordsReport vocab =
    fmt $ nameF "All words" $ unlinesF (allWords vocab)

wordCountReport :: Vocabulary -> Text
wordCountReport vocab = T.unlines [part1, part2]
  where
    (total, unique) = wordsCount vocab
    part1 =
      T.append
        "Total number of words: "
        (T.pack $ show total)
    part2 =
      T.append
        "Number of unique words: "
        (T.pack $ show unique)

wordCountReport' :: Vocabulary -> Text
wordCountReport' vocab = fmt $
    "Total number of words: " +|total|+
    "\nNumber of unique words: " +|unique|+ "\n"
    where
        (total, unique) = wordsCount vocab

allWords  :: Vocabulary -> [Text]
allWords  = map fst

{-
    The operators +| and |+ are used for including variables and formatters. Sometimes we need to call show for our variable (if the fmt package doesn't know how to
    convert it to textual form). This can be done implicitly via the other pair of operators , +|| and ||+. formatters from the fmt package are powerful enough to present
    tuples, lists and even associated lists, but we can always provide a formatter for our data by writing a function returning Builder, which is the data type used for
    efficiently constructing Text values.
        - The nameF function gives a name to the rest of the output.
        - The unlinesF function combines elements of the list into one Builder.
        - The blockListF' function formats list elements in the given way and presents them line by line.
-}
frequentWordsReport :: Vocabulary -> Int -> Text
frequentWordsReport vocab num =
    fmt $ nameF "Frequent words"
          $ blockListF' "" fmtEntry reportData
    where
        reportData = take num $ wordsByFrequency vocab
        fmtEntry (t, n) = ""+|t|+": "+|n|+""



wordsCount :: Vocabulary -> (Int, Int)
wordsCount vocab = (sum $ map snd vocab, length vocab)

wordsByFrequency :: Vocabulary -> Vocabulary
wordsByFrequency = sortBy (comparing $ Down . snd)

printAllWords :: Vocabulary -> IO ()
printAllWords vocab = do
  putStrLn "All words: "
  TIO.putStrLn $ T.unlines $ map fst vocab

processTextFile :: FilePath -> Bool -> Int -> IO ()
processTextFile fname withAllWords n = do
  text <- TIO.readFile fname
  let vocab = extractVocab text
  when withAllWords $ TIO.putStrLn $ allWordsReport vocab
  TIO.putStrLn $ wordCountReport' vocab
  TIO.putStrLn $ frequentWordsReport vocab n

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["-a", fname, num] -> processTextFile fname True (read num)
    [fname, num] -> processTextFile fname False (read num)
    _ -> putStrLn "Usage: vocab3 [-a] filename freq_words_num"
