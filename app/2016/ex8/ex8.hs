import Control.Applicative ((<|>))
import Text.Parsec (parse, try)
import Text.Parsec.Char (char, digit, string, letter)
import Text.Parsec.Combinator (many1)
import Text.Parsec.String (Parser)

import Debug.Trace

data Command = Rect Int Int | RotateRow Int Int | RotateCol Int Int deriving Show
type Screen = [[Bool]]

parseRect :: Parser Command
parseRect = do
  string "rect "
  n <- many1 digit
  char 'x'
  m <- many1 digit

  return $ Rect (read n) (read m)

parseRotateRow :: Parser Command
parseRotateRow = do
  string "rotate row y="
  row <- many1 digit
  string " by "
  shift <- many1 digit

  return $ RotateRow (read row) (read shift)

parseRotateCol :: Parser Command
parseRotateCol = do
  string "rotate column x="
  col <- many1 digit
  string " by "
  shift <- many1 digit

  return $ RotateCol (read col) (read shift)

parseCommand :: Parser Command
parseCommand = try parseRect <|>
               try parseRotateRow <|>
               parseRotateCol

parseWithError :: Parser a -> String -> a
parseWithError p a = case (parse p "" a) of
                  (Right res) -> res
                  (Left err) -> error $ "Error in parsing string: \"" ++ a ++ "\" " ++ show err


----------

replaceNth :: Int -> a -> [a] -> [a]
replaceNth n newVal (x:xs)
  | n == 0 = newVal:xs
  | otherwise = x:replaceNth (n-1) newVal xs

runCommand :: Screen -> Command -> Screen
runCommand screen (Rect width height) =
  let topRows = take height screen
      botRows = drop height screen

      modTopRows = map (\row -> (replicate width True) ++ (drop width row)) topRows in
    modTopRows ++ botRows

runCommand screen (RotateRow row shift) =
  let topRows = take row screen
      rowToMod = screen !! row
      rowLength = length rowToMod
      botRows = drop (row + 1) screen
      modRow = (drop (rowLength - shift) rowToMod) ++ (take (rowLength - shift) rowToMod) in
    topRows ++ [modRow] ++ botRows

runCommand screen (RotateCol col shift) = map modRow (zip [0..] screen)
  where modRow (idx, row) =
          let takeFromIdx = if idx < shift then
                              idx + (length screen) - shift
                            else
                              idx - shift
              shiftVal = (screen !! takeFromIdx) !! col in

            (take col row) ++ [shiftVal] ++ (drop (col + 1) row)
boolToInt :: Bool -> Int
boolToInt True = 1
boolToInt False = 0

numPixelsOn :: Screen -> Int
numPixelsOn screen = sum (map (sum . (map boolToInt)) screen)

boolToChar :: Bool -> Char
boolToChar True = '#'
boolToChar False = '.'

printPixelsOn :: Screen -> IO ()
printPixelsOn screen = mapM_ printRow screen where
  printRow row = putStrLn $ (map boolToChar row)

initialScreen :: Int -> Int -> Screen
initialScreen width height = replicate height (replicate width False)

main :: IO ()
main = do
  inputLines <- lines <$> getContents
  let commands = map (parseWithError parseCommand) inputLines
      finalScreen = foldl runCommand (initialScreen 50 6) commands

  putStrLn $ "Num pixels on big screen: " ++ (show (numPixelsOn finalScreen))

  printPixelsOn finalScreen
