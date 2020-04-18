module Stefan.Day2 where

import Input (readLines)
import Text.Read (readMaybe)
import Data.Map (Map)
import Data.Set (Set, member, insert, empty)
import qualified Data.Map as Map

letterCount :: String -> Map.Map Char Int
letterCount line = foldl (\acc letter -> Map.insertWith (+) letter 1 acc) Map.empty line

checkSum :: [String] -> Int
checkSum ids =
  let allCounts = map letterCount ids
      occurances num = length.filter (elem num)
  in occurances 3 allCounts * occurances 2 allCounts

findOneOff :: [String] -> String
findOneOff ids = head $ foldr (\comboCounts acc -> acc ++ (validPairs comboCounts)) [] allComboCounts
  where removeLetter pos id = take pos id ++ drop (pos + 1) id
        validPairs = Map.keys . (Map.filter (> 1))
        allCombinations = (map (\pos -> map (removeLetter pos) ids ) [0..(length ids)])
        wordCount = foldr (\word words -> Map.insertWith (+) word 1 words) Map.empty
        allComboCounts = map wordCount allCombinations

main :: IO ()
main = do
   input <- readLines "day2_sg.txt"
   print $ checkSum input
   print $ findOneOff input
