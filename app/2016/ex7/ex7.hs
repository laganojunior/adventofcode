import Control.Applicative ((<|>))
import qualified Data.Set as Set
import Text.Parsec (parse, try)
import Text.Parsec.Char (char, noneOf)
import Text.Parsec.Combinator (many1)
import Text.Parsec.String (Parser)

import Debug.Trace

----

data IPPart = OutsideBrackets String | InsideBrackets String deriving Show
type IPAddress = [IPPart]

parseInsideBrackets :: Parser IPPart
parseInsideBrackets = do
  char '['
  s <- many1 (noneOf "[]")
  char ']'
  return (InsideBrackets s)

parseOutsideBrackets :: Parser IPPart
parseOutsideBrackets = do
  s <- many1 (noneOf "[]")
  return (OutsideBrackets s)

parseIPPart :: Parser IPPart
parseIPPart = try parseInsideBrackets <|> parseOutsideBrackets

parseIPAddress :: Parser IPAddress
parseIPAddress = do
  ipparts <- many1 parseIPPart
  return ipparts

parseWithError :: Parser a -> String -> a
parseWithError p a = case (parse p "" a) of
                      (Right res) -> res

hasAbba :: String -> Bool
hasAbba [] = False
hasAbba (x : []) = False
hasAbba (x : y : []) = False
hasAbba (x : y : z : []) = False
hasAbba (x : rest@(y : z : w : _)) =
  if x /= y && x == w && y == z then
    True
  else
    hasAbba rest

getAbas :: String -> [String]
getAbas [] = []
getAbas (x : []) = []
getAbas (x : y : []) = []
getAbas (x : rest@(y : z : _)) =
  if x == z && x /= y then
    [x, y, z] : getAbas rest
  else
    getAbas rest

correspondingBab :: String -> String
correspondingBab (x : y : z : []) =[y, x, y]

ipAddressSupportsTLS :: IPAddress -> Bool
ipAddressSupportsTLS ipparts = ipAddressSupportsTLS' ipparts False where
  ipAddressSupportsTLS' ipparts hasOutsideAbba = case ipparts of
    [] -> hasOutsideAbba
    (ippart : restIpparts) -> case ippart of
      OutsideBrackets s -> ipAddressSupportsTLS' restIpparts (hasOutsideAbba || (hasAbba s))
      InsideBrackets s -> if hasAbba s then
          False
        else
          ipAddressSupportsTLS' restIpparts hasOutsideAbba

getAbaSets :: IPAddress -> (Set.Set String, Set.Set String)
getAbaSets ipparts = foldl getAbaSets' (Set.empty, Set.empty) ipparts
  where getAbaSets' (correspondingOutsideAbaSet, insideAbaSet) ippart = case ippart of
          OutsideBrackets s -> (foldl (\s x -> Set.insert x s) correspondingOutsideAbaSet (map correspondingBab (getAbas s)), insideAbaSet)
          InsideBrackets s -> (correspondingOutsideAbaSet, foldl (\s x -> Set.insert x s) insideAbaSet (getAbas s))

ipAddressSupportsSSL :: IPAddress -> Bool
ipAddressSupportsSSL ipparts =
  let (correspondingOutsideAbaSet, insideAbaSet) = getAbaSets ipparts in
    (Set.size (Set.intersection correspondingOutsideAbaSet insideAbaSet)) > 0

main :: IO()
main = do
  inputLines <- lines <$> getContents
  let ipAddresses = map (parseWithError parseIPAddress) inputLines
      tlsAddresses = filter ipAddressSupportsTLS ipAddresses

  putStrLn $ "Num TLS addresses: " ++ (show (length tlsAddresses))

  let sslAddresses = filter ipAddressSupportsSSL ipAddresses
  putStrLn $ "Num SSL addresses: " ++ (show (length sslAddresses))

