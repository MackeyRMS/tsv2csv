module Tsv2csv where

import           Data.List       (intercalate)
import           Data.List.Split (splitOn)


fromTsv :: String -> [[String]]
fromTsv = map (splitOn "\t") . lines

toCsv :: [[String]] -> String
toCsv = unlines . map (intercalate ",") . map (map csvEscape)

csvEscape :: String -> String
csvEscape s
  | ','  `elem` s = s'
  | '\"' `elem` s = s'
  | '\n' `elem` s = s'
  | otherwise     = s
  where s' = ('\"' : s) ++ "\""

