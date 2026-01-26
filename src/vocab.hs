import Data.Char
import Data.List (group, sort)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Environment -- Module for reading commandline arguments into a list of strings

main = do
  [fname] <- getArgs
  text <- TIO.readFile fname
  let ws =
        map head $
          group $
            sort $
              map T.toCaseFold $
                filter (not . T.null) $
                  map (T.dropAround $ not . isLetter) $
                    T.words text
  TIO.putStrLn $ T.unwords ws
  print $ length ws
