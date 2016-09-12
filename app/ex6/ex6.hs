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
  (((minX1 <= minX2) && (minX2 <= maxX1)) || ((minX2 <= minX1) && (maxX1 <= maxX2))) &&
  (((minY1 <= minY2) && (minY2 <= maxY1)) || ((minY2 <= minY1) && (maxY1 <= maxY2)))

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

rangeSize :: CoordRange -> Integer
rangeSize (CoordRange (minX, minY) (maxX, maxY)) = ((maxX - minX + 1) * (maxY - minY + 1))

subRangeLightsOn :: CoordRange -> CommandType -> [Command] -> Integer
subRangeLightsOn coordRange commandType restCommands =
  let s = rangeSize coordRange in
    case commandType of
      TurnOff -> 0
      TurnOn  -> s
      Toggle  -> s - (numLightsOnRev restCommands coordRange)

numLightsOnRev :: [Command] -> CoordRange -> Integer
numLightsOnRev [] range = 0
numLightsOnRev ((Command commandType commandRange) : restCommands) range =
  let subRanges = rangeQuery range commandRange in
    sum $ do
      (subRange, intersects) <- subRanges
      if intersects then
        return $ subRangeLightsOn subRange commandType restCommands
      else
        return $ numLightsOnRev restCommands subRange

numLightsOn commands coordRange = numLightsOnRev (reverse commands) coordRange

subRangeBrightness :: CoordRange -> CommandType -> [Command] -> Integer -> Integer
subRangeBrightness coordRange commandType restCommands brightnessPerLight =
  let newBrightnessPerLight = case commandType of
        TurnOff -> max (brightnessPerLight - 1) 0
        TurnOn  -> brightnessPerLight + 1
        Toggle  -> brightnessPerLight + 2 in
    brightnessAdd restCommands coordRange newBrightnessPerLight

brightnessAdd :: [Command] -> CoordRange -> Integer -> Integer
brightnessAdd [] range brightnessPerLight = brightnessPerLight * (rangeSize range)
brightnessAdd ((Command commandType commandRange) : restCommands) range brightnessPerLight =
  let subRanges = rangeQuery range commandRange in
    sum $ do
      (subRange, intersects) <- subRanges
      if intersects then
        return $ subRangeBrightness subRange commandType restCommands brightnessPerLight
      else
        return $ brightnessAdd restCommands subRange brightnessPerLight

brightness commands coordRange = brightnessAdd commands coordRange 0

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

