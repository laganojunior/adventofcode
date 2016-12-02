import Data.List.Split (splitOn)

data TurnDirection = TurnLeft | TurnRight deriving Show
data Command = Command TurnDirection Integer deriving Show
type Coords = (Integer, Integer)
data Direction = North | West | South | East deriving Show
type State = (Coords, Direction)

parseCommand :: String -> Command
parseCommand s =
  let turnDirection = if ((head s) == 'L') then TurnLeft else TurnRight
      numSteps = read (tail s) :: Integer in
    Command turnDirection numSteps

parseCommands :: String -> [Command]
parseCommands s = map parseCommand (splitOn ", " s)

rotateLeft :: Direction -> Direction
rotateLeft North = West
rotateLeft West = South
rotateLeft South = East
rotateLeft East = North

rotateRight :: Direction -> Direction
rotateRight North = East
rotateRight East = South
rotateRight South = West
rotateRight West = North

directionVector :: Direction -> (Integer, Integer)
directionVector North = (0, 1)
directionVector West = (-1, 0)
directionVector South = (0, -1)
directionVector East = (1, 0)

applyVector :: Coords -> (Integer, Integer)-> Integer -> Coords
applyVector (x, y) (dx, dy) n = (x + dx * n, y + dy * n)

walkForward :: Direction -> Coords -> Integer -> Coords
walkForward direction coords numSteps = applyVector coords (directionVector direction) numSteps

runCommand :: State -> Command -> State
runCommand (coords, direction) (Command turnDirection numSteps) =
  let newDirection = case turnDirection of
        TurnLeft -> rotateLeft direction
        TurnRight -> rotateRight direction
      newCoords = walkForward newDirection coords numSteps in
    (newCoords, newDirection)

runCommandAllVisited :: State -> Command -> (State, [Coords])
runCommandAllVisited (coords, direction) (Command turnDirection numSteps) =
  let newDirection = case turnDirection of
        TurnLeft -> rotateLeft direction
        TurnRight -> rotateRight direction
      visitedCoords = take (fromInteger numSteps) (tail (iterate (\c -> walkForward newDirection c 1) coords))
      finalCoord = last visitedCoords in
    ((finalCoord, newDirection), visitedCoords)

getAllVisited :: [Command] -> [Coords]
getAllVisited commands = getAllVisited' ((0, 0), North) [(0, 0)] commands where
  getAllVisited' state visited [] = visited
  getAllVisited' state visited (command:restCommands) =
    let (newState, newVisited) = runCommandAllVisited state command in
      getAllVisited' newState (visited ++ newVisited) restCommands

coordDistanceFromOrigin :: Coords -> Integer
coordDistanceFromOrigin (x, y) = (abs x) + (abs y)

firstRepeating :: (Eq a) => [a] -> a
firstRepeating l = firstRepeatingHelper l [] where
  firstRepeatingHelper [] seen = error "No repeats?"
  firstRepeatingHelper (x:xs) seen =
    if elem x seen then
      x
    else
      firstRepeatingHelper xs (x : seen)

main :: IO ()
main = do
  input <- getContents
  let commands = parseCommands input
      startState = ((0, 0), North)
      (endCoords, _) = foldl runCommand startState commands

  putStrLn $ "Distance from origin: " ++ (show (coordDistanceFromOrigin endCoords))

  let allCoords = getAllVisited commands
      firstRepeatingCoord = firstRepeating allCoords

  putStrLn $ "Distance from origin 2: " ++ (show (coordDistanceFromOrigin firstRepeatingCoord))
