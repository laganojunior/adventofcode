import Data.ByteString.Lazy.Char8 (pack)
import Data.Char
import Data.Digest.Pure.MD5       (md5)
import Data.List (isPrefixOf)
import Data.Maybe
import Control.Monad
import Debug.Trace

md5FromString :: String -> String
md5FromString = show . md5 . pack

checkMd5 :: String -> Bool
checkMd5 m = isPrefixOf (replicate 5 '0') m

generateMd5FromKeySalt :: String -> String -> String
generateMd5FromKeySalt key salt = md5FromString (key ++ salt)

getGoodMd5s :: String -> [String]
getGoodMd5s key = filter checkMd5 (map ((generateMd5FromKeySalt key) . show) [1..])

replaceNth :: Int -> a -> [a] -> [a]
replaceNth n newVal (x:xs)
     | n == 0 = newVal:xs
     | otherwise = x:replaceNth (n-1) newVal xs

getPositionedPassword :: [String] -> String
getPositionedPassword md5s = getPositionedPassword' md5s (replicate 8 Nothing)
  where getPositionedPassword' :: [String] -> [Maybe Char] -> String
        getPositionedPassword' (md5:restMd5s) solved =
          let fullPassword = trace ("Password state so far:" ++ (show solved)) catMaybes solved in
            if (length fullPassword == 8) then
             fullPassword
            else
              let positionChar = md5 !! 5
                  passwordChar = md5 !! 6 in
                if isDigit positionChar then
                  let position = digitToInt positionChar in
                    if (position >= 8) then
                      getPositionedPassword' restMd5s solved
                    else
                      case solved !! position of
                        Nothing -> getPositionedPassword' restMd5s (replaceNth position (Just passwordChar) solved)
                        Just _ -> getPositionedPassword' restMd5s solved
                else
                  getPositionedPassword' restMd5s solved

main :: IO ()
main = do
  let key = "reyedfim"
      goodMd5s = (getGoodMd5s key)
      password = (take 8 (map (!! 5) goodMd5s))

  putStrLn $ "Password is: " ++ password

  let password2 = getPositionedPassword goodMd5s
  putStrLn $ "Password 2 is: " ++ password2
