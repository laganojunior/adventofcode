import Control.Applicative ((<|>))
import Control.Monad.State
import Data.Bits
import Data.Word
import qualified Data.Map.Strict as Map
import Text.Parsec (parse, try)
import Text.Parsec.Char (digit, string, letter)
import Text.Parsec.Combinator (many1)
import Text.Parsec.String (Parser)

import Debug.Trace

----

type NodeName = String
data CircuitNode = CircuitNodeConstant Word16 |
                   CircuitNodeReference NodeName deriving Show
data Circuit = CircuitConstant CircuitNode |
               CircuitAnd CircuitNode CircuitNode |
               CircuitOr CircuitNode CircuitNode |
               CircuitNot CircuitNode |
               CircuitLShift CircuitNode Int |
               CircuitRShift CircuitNode Int deriving Show


circuitNodeParser :: Parser CircuitNode
circuitNodeParser = try ((many1 letter) >>= (return . CircuitNodeReference)) <|>
                    try ((many1 digit)  >>= (return . CircuitNodeConstant . read))

circuitConstantParser :: Parser (NodeName, Circuit)
circuitConstantParser = do
  node <- circuitNodeParser
  string " -> "
  name <- many1 letter
  return (name, CircuitConstant node)

circuitAndParser :: Parser (NodeName, Circuit)
circuitAndParser = do
  subnode1 <- circuitNodeParser
  string " AND "
  subnode2 <- circuitNodeParser
  string " -> "
  name <- many1 letter
  return (name, CircuitAnd subnode1 subnode2)

circuitOrParser :: Parser (NodeName, Circuit)
circuitOrParser = do
  subnode1 <- circuitNodeParser
  string " OR "
  subnode2 <- circuitNodeParser
  string " -> "
  name <- many1 letter
  return (name, CircuitOr subnode1 subnode2)

circuitLShiftParser :: Parser (NodeName, Circuit)
circuitLShiftParser = do
  subnode <- circuitNodeParser
  string " LSHIFT "
  n <- many1 digit
  string " -> "
  name <- many1 letter
  return (name, CircuitLShift subnode (read n))

circuitRShiftParser :: Parser (NodeName, Circuit)
circuitRShiftParser = do
  subnode <- circuitNodeParser
  string " RSHIFT "
  n <- many1 digit
  string " -> "
  name <- many1 letter
  return (name, CircuitRShift subnode (read n))

circuitNotParser :: Parser (NodeName, Circuit)
circuitNotParser = do
  string "NOT "
  subnode <- circuitNodeParser
  string " -> "
  name <- many1 letter
  return (name, CircuitNot subnode)

connectionParser :: Parser (NodeName, Circuit)
connectionParser = try circuitConstantParser <|>
                   try circuitAndParser <|>
                   try circuitOrParser <|>
                   try circuitLShiftParser <|>
                   try circuitRShiftParser <|>
                   try circuitNotParser

parseWithError :: Parser a -> String -> a
parseWithError p a = case (parse p "" a) of
                  (Right res) -> res
                  (Left err) -> error $ "Error in parsing string: \"" ++ a ++ "\" " ++ show err

------

lookupWithError :: (Ord k) => k -> Map.Map k a -> a
lookupWithError key map = case (Map.lookup key map) of
  Just val -> val
  Nothing -> error $ "Error looking up key"

circuitNodeValue :: Map.Map NodeName Circuit -> CircuitNode -> State (Map.Map NodeName Word16) Word16
circuitNodeValue circuitMap (CircuitNodeConstant val) = return val
circuitNodeValue circuitMap (CircuitNodeReference name) = getConnectionValue circuitMap name

getConnectionValue :: Map.Map NodeName Circuit -> NodeName -> State (Map.Map NodeName Word16) Word16
getConnectionValue connectionMap name =
  do
    knownValues <- get
    if Map.member name knownValues then do
      return (lookupWithError name knownValues)
    else do
      value <- case lookupWithError name connectionMap of
                 CircuitConstant node -> circuitNodeValue connectionMap node
                 CircuitAnd node1 node2 -> (.&.) <$> circuitNodeValue connectionMap node1 <*> circuitNodeValue connectionMap node2
                 CircuitOr node1 node2 -> (.|.) <$> circuitNodeValue connectionMap node1 <*> circuitNodeValue connectionMap node2
                 CircuitNot node -> complement <$> (circuitNodeValue connectionMap node)
                 CircuitLShift node n -> (flip shiftL) n <$> (circuitNodeValue connectionMap node)
                 CircuitRShift node n -> (flip shiftR) n <$> (circuitNodeValue connectionMap node)
      knownValues' <- get
      put (Map.insert name value knownValues')
      return value

main :: IO ()
main = do
  inputLines <- lines <$> getContents
  let connectionMap = Map.fromList (map (parseWithError connectionParser) inputLines)
      aVal1 = (evalState (getConnectionValue connectionMap "a") Map.empty)

  putStrLn $ "First value of a: " ++ show aVal1

  let connectionMap2 = Map.insert "b" (CircuitConstant (CircuitNodeConstant aVal1)) connectionMap
      aVal2 = (evalState (getConnectionValue connectionMap2 "a") Map.empty)

  putStrLn $ "Second value of a: " ++ show aVal2
