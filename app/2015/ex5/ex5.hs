isVowel :: Char -> Bool
isVowel c = elem c "aeiou"

countIf :: (a -> Bool) -> [a] -> Int
countIf pred xs = length $ filter pred xs

hasThreeVowels :: String -> Bool
hasThreeVowels s = (countIf isVowel s) >= 3

hasDouble :: String -> Bool
hasDouble (c1 : c2 : cs) =  if c1 == c2 then True else hasDouble (c2 : cs)
hasDouble _ = False

hasNoIllegalPairs :: String -> Bool
hasNoIllegalPairs (c1 : c2 : cs) = if elem [c1, c2] ["ab", "cd", "pq", "xy"] then
                                    False
                                   else
                                    hasNoIllegalPairs (c2 : cs)
hasNoIllegalPairs _ = True


hasDisjointRepeatingPair :: String -> Bool
hasDisjointRepeatingPair [] = False
hasDisjointRepeatingPair (c1:cs) = hasDisjointRepeatingPairHelper [] c1 cs where
  hasDisjointRepeatingPairHelper _ _ [] = False
  hasDisjointRepeatingPairHelper _ _ (c1:[]) = False
  hasDisjointRepeatingPairHelper prevPairs prevChar (c1:c2:cs) =
    let newPair = [c1, c2] in
      if elem newPair prevPairs then
        True
      else
        hasDisjointRepeatingPairHelper ([prevChar, c1] : prevPairs) c1 (c2:cs)

hasSandwich :: String -> Bool
hasSandwich (c1 : c2 : c3 : cs) = if c1 == c3 then
                                    True
                                  else
                                    hasSandwich (c2 : c3 : cs)
hasSandwich _ = False

isStringNice :: String -> Bool
isStringNice s = and [hasThreeVowels s, hasDouble s, hasNoIllegalPairs s]

isStringNice2 :: String -> Bool
isStringNice2 s = and [ hasDisjointRepeatingPair s, hasSandwich s]

main :: IO ()
main = do
  input <- getContents
  strings <- return $ lines input

  numNiceStrings <- return $ countIf isStringNice strings
  putStrLn $ "Number of nice strings: " ++ show numNiceStrings

  numNiceStrings2 <- return $ countIf isStringNice2 strings
  putStrLn $ "Number of nice strings 2: " ++ show numNiceStrings2
