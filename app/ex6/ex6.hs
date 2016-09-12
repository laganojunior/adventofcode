import Control.Applicative ((<|>))
import Text.Parsec (parse, try)
import Text.Parsec.Char (digit, string, char, space)
import Text.Parsec.Combinator (many1)
import Text.Parsec.String (Parser)

----

data CommandType = TurnOff | TurnOn | Toggle
type Coord = (Integer, Integer)
data CoordRange = CoordRange Coord Coord
data Command = Command CommandType CoordRange

numParser :: Parser Integer
numParser = do
  n <- many1 digit
  return (read n)

commandTypeParser :: Parser CommandType
commandTypeParser = try (string "turn on") *> return TurnOn <|>
                    try (string "turn off") *> return TurnOff <|>
                    (string "toggle") *> return Toggle

coordParser :: Parser Coord
coordParser = do
  x <- numParser
  char ','
  y <- numParser
  return (x, y)

commandParser :: Parser Command
commandParser = do
  commandType <- commandTypeParser
  space
  coord1 <- coordParser
  space
  string "through"
  space
  coord2 <- coordParser
  return $ Command commandType (CoordRange coord1 coord2)

-----

rangesIntersect :: CoordRange -> CoordRange -> Bool
rangesIntersect (CoordRange (minX1, minY1) (maxX1, maxY1)) (CoordRange (minX2, minY2) (maxX2, maxY2)) =
  (((minX1 <= minX2) && (minX2 <= maxX1)) || ((minX2 <= minX1) && (minX1 <= maxX2))) &&
  (((minY1 <= minY2) && (minY2 <= maxY1)) || ((minY2 <= minY1) && (minY1 <= maxY2)))

singleDimRanges :: Integer -> Integer -> Integer -> Integer -> [((Integer, Integer), Bool)]
singleDimRanges minTarget minQuery maxQuery maxTarget
  -- Complete miss
  | maxQuery < minTarget || minQuery > maxTarget = [((minTarget, maxTarget), False)]

  -- Target completely contains query, handling empty ranges when boundaries align
  | minQuery > minTarget && maxQuery < maxTarget = [((minTarget, minQuery-1), False), ((minQuery, maxQuery), True), ((maxQuery+1, maxTarget), False)]
  | minQuery == minTarget && maxQuery < maxTarget = [((minQuery, maxQuery), True), ((maxQuery+1, maxTarget), False)]
  | minQuery > minTarget && maxQuery == maxTarget = [((minTarget, minQuery-1), False), ((minQuery, maxQuery), True)]
  | minQuery == minTarget && maxQuery == maxTarget = [((minQuery, maxQuery), True)]

  -- Query strictly includes target
  | minQuery < minTarget && maxQuery > maxTarget = [((minTarget, maxTarget), True)]

  -- Query strictly extends off left
  | minQuery < minTarget && maxQuery < maxTarget = [((minTarget, maxQuery), True), ((maxQuery+1, maxTarget), False)]
  | minQuery < minTarget && maxQuery == maxTarget = [((minTarget, maxQuery), True)]

  -- Query strictly extends off right
  | minQuery > minTarget && maxQuery > maxTarget = [((minTarget, minQuery-1), False), ((minQuery, maxTarget), True)]
  | minQuery == minTarget && maxQuery > maxTarget = [((minQuery, maxTarget), True)]

  | otherwise = error $ "Non exhaustive: " ++ (show (minTarget, minQuery, maxQuery, maxTarget))

-- Takes a target range and query range, and returns a list of subranges within the target with bools indicating if
-- the range is within the query
rangeQuery :: CoordRange -> CoordRange -> [(CoordRange, Bool)]
rangeQuery (CoordRange (minTargetX, minTargetY) (maxTargetX, maxTargetY)) (CoordRange (minQueryX, minQueryY) (maxQueryX, maxQueryY)) =
  let xRanges = singleDimRanges minTargetX minQueryX maxQueryX maxTargetX
      yRanges = singleDimRanges minTargetY minQueryY maxQueryY maxTargetY in
    do
      ((x1, x2), inQueryX) <- xRanges
      ((y1, y2), inQueryY) <- yRanges
      return ((CoordRange (x1, y1) (x2, y2)), inQueryX && inQueryY)

data CoordRangeTree a = Leaf CoordRange a | Node CoordRange [(CoordRangeTree a)]

coordRangeUpdate :: CoordRangeTree a -> CoordRange -> (a -> a) -> CoordRangeTree a
coordRangeUpdate (Leaf targetCoordRange val) queryCoordRange f =
  let subRanges = rangeQuery targetCoordRange queryCoordRange in
    if length subRanges == 1 then
      let (range, intersect) = head subRanges in
        if intersect then
          Leaf targetCoordRange (f val)
        else
          Leaf targetCoordRange val
    else
      Node targetCoordRange
       (do
          (subRange, intersects) <- subRanges
          if intersects then
            return $ Leaf subRange (f val)
          else
            return $ Leaf subRange val)

coordRangeUpdate (Node targetCoordRange children) queryCoordRange f =
  if rangesIntersect targetCoordRange queryCoordRange then
    Node targetCoordRange (map (\child -> coordRangeUpdate child queryCoordRange f) children)
  else
    Node targetCoordRange children

coordRangeTreeFoldMap :: (CoordRange -> a -> b) -> ([b] -> b) -> CoordRangeTree a -> b
coordRangeTreeFoldMap f combine (Leaf coordRange val) = f coordRange val
coordRangeTreeFoldMap f combine (Node coordRange children) = combine (map (coordRangeTreeFoldMap f combine)  children)

rangeSize :: CoordRange -> Integer
rangeSize (CoordRange (minX, minY) (maxX, maxY)) = ((maxX - minX + 1) * (maxY - minY + 1))

lightsOnUpdate :: CoordRangeTree Bool -> Command -> CoordRangeTree Bool
lightsOnUpdate tree (Command commandType coordRange) =
  let updateFunc = case commandType of
        TurnOff -> const False
        TurnOn -> const True
        Toggle -> not in
    coordRangeUpdate tree coordRange updateFunc

numLightsOnInRange :: CoordRange -> Bool -> Integer
numLightsOnInRange _ False = 0
numLightsOnInRange coordRange True = rangeSize coordRange

numLightsOn :: [Command] -> CoordRange -> Integer
numLightsOn commands coordRange =
  let startState = Leaf coordRange False
      endState = foldl lightsOnUpdate startState commands in
    coordRangeTreeFoldMap numLightsOnInRange sum endState

brightnessUpdate :: CoordRangeTree Integer -> Command -> CoordRangeTree Integer
brightnessUpdate tree (Command commandType coordRange) =
  let updateFunc = case commandType of
        TurnOff -> (\n -> max 0 (n - 1))
        TurnOn -> (+ 1)
        Toggle -> (+ 2) in
    coordRangeUpdate tree coordRange updateFunc

brightnessInRange :: CoordRange -> Integer -> Integer
brightnessInRange coordRange brightness = brightness * (rangeSize coordRange)

brightness :: [Command] -> CoordRange -> Integer
brightness commands coordRange = 
  let startState = Leaf coordRange 0
      endState = foldl brightnessUpdate startState commands in
    coordRangeTreeFoldMap brightnessInRange sum endState

parseWithError :: Parser a -> String -> a
parseWithError p a = case (parse p "" a) of
                      (Right res) -> res

main :: IO ()
main = do
  inputLines <- lines <$> getContents
  let commands = map (parseWithError commandParser) inputLines
      totalLightsOn = numLightsOn commands (CoordRange (0, 0) (999, 999))
  putStrLn $ "Total Lights On: " ++ (show totalLightsOn)

  let endBrightness = brightness commands  (CoordRange (0, 0) (999, 999))

  putStrLn $ "Brightness: " ++ (show endBrightness)

