module Main where

import           Tsv2csv

main :: IO ()
main = do
  ss <- getContents
  putStrLn $ toCsv . fromTsv $ ss
