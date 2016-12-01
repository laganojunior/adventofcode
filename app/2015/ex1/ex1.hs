import Data.List

charToMove :: Char -> Int
charToMove '(' = 1
charToMove ')' = -1
charToMove _ = 0

reductions :: (b -> a -> b) -> b -> [a] -> [b]
reductions f curr [] = []
reductions f curr (x : xs) = let next = f curr x in
  next : (reductions f next xs)

main :: IO ()
main = do
  input <- getContents
  moves <- return $ map charToMove input
  finalFloor <- return $ sum moves

  putStrLn $ "Final Floor: " ++ show finalFloor

  allFloors <- return $ reductions (+) 0 moves
  case findIndex (\x -> x < 0) allFloors of
    Just idx -> putStrLn $ "First basement idx (1 indexed): " ++ show (idx + 1)
    Nothing -> putStrLn $ "Basement never reached"
