import Control.Monad.State
import Debug.Trace

type Coords = (Integer, Integer)
data Direction = DirLeft | DirRight | DirUp | DirDown deriving Show

parseDirection :: Char -> Direction
parseDirection 'L' = DirLeft
parseDirection 'R' = DirRight
parseDirection 'U' = DirUp
parseDirection 'D' = DirDown

parseDirections :: String -> [Direction]
parseDirections s = map parseDirection s

move1 :: Coords -> Direction -> Coords
move1 (x, y) direction =
  let (dx, dy) = case direction of
        DirLeft -> (-1, 0)
        DirRight -> (1, 0)
        DirDown -> (0, 1)
        DirUp -> (0, -1)
      newx = (min (max (x + dx) 0) 2)
      newy = (min (max (y + dy) 0) 2) in
    (newx, newy)

coordsToCode1 :: Coords -> String
coordsToCode1 (x, y) = show (y * 3 + x + 1)

move2 :: Coords -> Direction -> Coords
move2 (x, y) direction =
  let (dx, dy) = case direction of
        DirLeft -> (-1, 0)
        DirRight -> (1, 0)
        DirDown -> (0, 1)
        DirUp -> (0, -1)
      newx = x + dx
      newy = y + dy in
    if (abs newx + abs newy) > 2 then
      (x, y)
    else
      (newx, newy)

coordsToCode2 :: Coords -> String
coordsToCode2 (0, -2) = "1"
coordsToCode2 (-1, -1) = "2"
coordsToCode2 (0, -1) = "3"
coordsToCode2 (1, -1) = "4"
coordsToCode2 (-2, 0) = "5"
coordsToCode2 (-1, 0) = "6"
coordsToCode2 (0, 0) = "7"
coordsToCode2 (1, 0) = "8"
coordsToCode2 (2, 0) = "9"
coordsToCode2 (-1, 1) = "A"
coordsToCode2 (0, 1) = "B"
coordsToCode2 (1, 1) = "C"
coordsToCode2 (0, 2) = "D"

getNextCoords :: (Coords -> Direction -> Coords) -> [Direction] -> State Coords Coords
getNextCoords move directions = do
  currCoords <- get
  let nextCoords = foldl move currCoords directions
  put nextCoords
  return $ nextCoords

getCode :: [[Direction]] -> Coords -> (Coords -> Direction -> Coords) -> (Coords -> String) -> String
getCode directionsList initial move coordsToCode = concat (map coordsToCode (evalState (mapM (getNextCoords move) directionsList) initial))

main :: IO ()
main = do
  directionsList <- ((map parseDirections) . lines) <$> getContents
  putStrLn $ "Bathroom code 1 is: " ++ getCode directionsList (1, 1) move1 coordsToCode1
  putStrLn $ "Bathroom code 2 is: " ++ getCode directionsList (-2, 0) move2 coordsToCode2
