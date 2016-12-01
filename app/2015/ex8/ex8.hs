stripEnds :: String -> String
stripEnds s = take ((length s) - 2) (drop 1 s)

getStrLn :: String -> Int
getStrLn s = getStrLn' (stripEnds s) where
  getStrLn' s = case s of
    [] -> 0
    ('\\' : '\\' : xs) -> 1 + getStrLn' xs
    ('\\' : '"' : xs) -> 1 + getStrLn' xs
    ('\\' : 'x' : _ : _ : xs) -> 1 + getStrLn' xs
    (_:xs) -> 1 + getStrLn' xs

escapeChar :: Char -> String
escapeChar '"' = "\\\""
escapeChar '\\' = "\\\\"
escapeChar c = [c]

escapeString :: String -> String
escapeString = concatMap escapeChar

main :: IO ()
main = do
  inputLines <- lines <$> getContents
  let totalLiteralLength = sum $ map length inputLines
      totalStringLength = sum $ map getStrLn inputLines

  putStrLn $ "Length difference 1: " ++ show (totalLiteralLength - totalStringLength)

  let totalEscapedLength = sum $ map ((+ 2) . length . escapeString) inputLines

  putStrLn $ "Length difference 2: " ++ show (totalEscapedLength - totalLiteralLength)
