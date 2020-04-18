module Stefan.Day1 where

import Input (readLines)
import Text.Read (readMaybe)
import Data.Set (Set, member, insert, empty)
repeater :: Set Int -> [Int] -> Maybe Int
repeater visited (x:xs) = if member x visited
                             then Just x
                             else repeater (insert x visited) xs
main :: IO ()
main = do
   input <- readLines "day1_sg.txt"
   let nums = map (read.(filter (/= '+'))) input
   print $ foldl (+) 0 nums
   let firstRepeat = repeater empty ( scanl1 (\acc x -> acc + x) (cycle nums))
   print firstRepeat
  
