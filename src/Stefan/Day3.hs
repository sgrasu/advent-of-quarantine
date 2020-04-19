module Stefan.Day3 where

import Input (readLines)
import Data.Map (Map)
import qualified Data.Map as Map

data Patch = Patch { claim :: String
                   , offsets :: (Int, Int)
                   , dims :: (Int, Int)}

data Fabric = Fabric { tiles :: Map (Int, Int) Int
                     , patches :: [Patch]}

getTiles :: Patch -> [(Int, Int)]
getTiles (Patch {offsets =  (offsetX, offsetY), dims =  (width, height)}) =
   [(offsetX + x, offsetY + y) | x <- [0 .. width - 1], y <- [0 .. height - 1]]


addPatch :: Patch -> Fabric -> Fabric
addPatch patch fabric=
  let updatedPatches = fabric {patches = patch : (patches fabric)}
      insertTile tile fabric = fabric {tiles = Map.insertWith (+) tile 1 (tiles fabric)}
  in foldr insertTile updatedPatches $ getTiles patch

parsePatch :: String -> Patch
parsePatch patchStr = Patch {claim = claim, offsets = offsets, dims = dims}
  where [claim, _, offsetStr, dimStr] = words patchStr
        readFn (x, y) = (read x, read (drop 1 y))
        offsets = readFn.(break(','==)).fst.(break (':' ==)) $ offsetStr
        dims = readFn.(break('x'==)) $ dimStr

countOverlaps :: Fabric -> Int
countOverlaps = Map.size.(Map.filter (1 <)).tiles

isUnique :: Fabric -> Patch -> Bool
isUnique fabric patch =
  foldr (\tile unique -> unique && (Map.lookup tile (tiles fabric) == Just 1)) True (getTiles patch)

main :: IO ()
main = do
  input <- readLines "day3_sg.txt"
  let fabric = foldr (\ patch fabric -> addPatch (parsePatch patch) fabric) (Fabric Map.empty []) input
  print $ countOverlaps fabric
  print $ map claim $ filter (isUnique fabric) (patches fabric)
