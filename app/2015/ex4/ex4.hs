import Data.ByteString.Lazy.Char8 (pack)
import Data.Digest.Pure.MD5       (md5)
import Data.List (isPrefixOf)
import Control.Monad

md5FromString :: String -> String
md5FromString = show . md5 . pack

paddedMD5 :: String -> Integer -> String
paddedMD5 key i = (md5FromString (key ++ show i))

checkInt :: String -> Integer -> Integer -> Bool
checkInt key prefixLength i = isPrefixOf (replicate (fromInteger prefixLength) '0') (paddedMD5 key i)

firstGoodInt :: String -> Integer -> Integer
firstGoodInt key prefixLength = head $ filter (checkInt key prefixLength) [1..]

main :: IO ()
main = do
  input <- getContents
  key <- return $ head $ lines input
  let ret = (firstGoodInt key 5)
  putStrLn $ "First good int with 5 zeroes is: " ++ show ret ++ " making " ++ paddedMD5 key ret

  let ret2 = (firstGoodInt key 6)
  putStrLn $ "First good int with 6 zeroes is: " ++ show ret2 ++ " making " ++ paddedMD5 key ret2
