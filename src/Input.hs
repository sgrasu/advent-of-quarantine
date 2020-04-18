module Input (readLines) where
readLines :: FilePath -> IO [String]

readLines path = fmap lines (readFile ("src/input/" ++ path))