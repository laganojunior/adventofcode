type Dimension = (Int, Int, Int)

split :: (a -> Bool) -> [a] -> [[a]]
split pred [] = []
split pred (x:xs) =
  if pred x then
    [] : split pred xs
  else
    case split pred xs of
      (firstSplit : restSplits) -> (x : firstSplit) : restSplits
      [] -> [[x]]

parseLine :: String -> Dimension
parseLine s = case split (== 'x') s of
  (wStr : lStr : hStr : []) -> (read wStr, read lStr, read hStr)
  _ -> error $ "Parse Error: " ++ s

faceAreas :: Dimension -> [Int]
faceAreas (w, l, h) = [(w * l), (w * l),
                       (l * h), (l * h),
                       (w * h), (w * h)]

wrappingPaperNeeded :: Dimension -> Int
wrappingPaperNeeded dim = let fas = faceAreas dim in
  (sum fas) + (minimum fas)

facePerims :: Dimension -> [Int]
facePerims (w, l, h) = [2 * (w + l), 2 * (w + l),
                        2 * (l + h), 2 * (l + h),
                        2 * (w + h), 2 * (w + h)]

volume :: Dimension -> Int
volume (w, l, h) = w * l * h

ribbonNeeded :: Dimension -> Int
ribbonNeeded dim = minimum (facePerims dim) + volume dim

main :: IO ()
main = do
  input <- getContents
  dims <- return $ map parseLine (lines input)

  totalWrappingPaperNeeded <- return $ sum $ map wrappingPaperNeeded dims
  putStrLn $ "Wrapping Paper Needed: " ++ show totalWrappingPaperNeeded

  totalRibbonNeeded <- return $ sum $ map ribbonNeeded dims
  putStrLn $ "Ribbon Needed: " ++ show totalRibbonNeeded
