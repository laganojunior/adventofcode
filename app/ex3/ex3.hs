import Data.List

type Position = (Int, Int)
type Move = (Int, Int)
data State = State Position [Position]

uniqAdd :: (Eq a) => [a] -> a -> [a]
uniqAdd s x = if elem x s
               then s
               else x : s

charToMove :: Char -> Move
charToMove '>' = (1, 0)
charToMove '<' = (-1, 0)
charToMove '^' = (0, 1)
charToMove 'v' = (0, -1)
charToMove _ = (0, 0)

movePosition :: Position -> Move -> Position
movePosition (posX, posY) (moveX, moveY) = (posX + moveX, posY + moveY)

doMove :: State -> Move -> State
doMove (State curr visited) move = let newPos = movePosition curr move in
  State newPos (uniqAdd visited newPos)

getVisited :: State -> [Position]
getVisited (State _ visited) = visited

alternate :: [a] -> ([a], [a])
alternate [] = ([], [])
alternate (x1 : []) = ([x1], [])
alternate (x1 : x2 : xs) = case alternate xs of
  (rest1, rest2) -> (x1 : rest1, x2 : rest2)

main :: IO ()
main = do
  input <- getContents
  moves <- return $ map charToMove input
  initialState <- return $ State (0, 0) []
  finalState <- return $ foldl doMove initialState moves
  putStrLn $ "Unique positions visited alone: " ++ show (length (getVisited finalState))

  (santaMoves, roboSantaMoves) <- return $ alternate moves
  finalSantaState <- return $ foldl doMove initialState santaMoves
  finalRoboSantaState <- return $ foldl doMove initialState roboSantaMoves
  allUniqVisited <- return $ nub ((getVisited finalSantaState) ++ (getVisited finalRoboSantaState))
  putStrLn $ "Unique positions visited with robo santa: " ++ show (length allUniqVisited)
