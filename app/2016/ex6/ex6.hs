import Data.Function
import Data.List
import Debug.Trace

positionFrequencies :: (Ord a) => [[a]] -> [[(a, Int)]]
positionFrequencies xs = map ((map (\x -> (head x, length x))) . group . sort) (transpose xs)

mostFrequentByPosition :: (Ord a) => [[a]] -> [a]
mostFrequentByPosition xs = map (fst . head . (sortBy (compare `on` (negate . snd)))) (positionFrequencies xs)

leastFrequentByPosition :: (Ord a) => [[a]] -> [a]
leastFrequentByPosition xs = map (fst . head . (sortBy (compare `on` snd))) (positionFrequencies xs)

main :: IO ()
main = do
  messages <- lines <$> getContents
  putStrLn $ "Decoded Most Frequenct: " ++ (mostFrequentByPosition messages)
  putStrLn $ "Decoded Least Frequent: " ++ (leastFrequentByPosition messages)
