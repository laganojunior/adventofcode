import Data.Char
import Data.List
import Data.List.Split
import Debug.Trace

data RoomCode = RoomCode {getCode :: String, getSectorId :: Integer, getCheckSum :: String} deriving Show


parseRoomCode :: String -> RoomCode
parseRoomCode s =
  let parts = splitOn "-" s
      lastPart = last parts
      code = concat (init parts)

      (sectorIdStr : checkSum : _) = (splitOn "[" (init lastPart)) in
    (RoomCode code (read sectorIdStr) checkSum)

calcCheckSum :: String-> String
calcCheckSum code =
  let frequencies = map (\x -> (head x, length x)) (group (sort code))
      sorter :: (Char, Int) -> (Char, Int) -> Ordering
      sorter (x, nx) (y, ny)
        | nx > ny = LT
        | nx < ny = GT
        | otherwise = if x < y then LT else GT in
    take 5 $ map fst (sortBy sorter frequencies)

checkLegalRoom :: RoomCode -> Bool
checkLegalRoom (RoomCode code _ checkSum) =
  (calcCheckSum code) == checkSum

decrypt :: RoomCode -> String
decrypt (RoomCode code sectorId _) =
  let shiftNum = ((fromInteger sectorId) `mod` 26) :: Int
      base = ord 'a' in
    map (chr . (+ base) . (\x -> x `mod` 26) . (shiftNum +) . (\x -> x - base) . ord) code

main :: IO ()
main = do
  roomCodes <- ((map parseRoomCode) . lines) <$> getContents

  let legalRooms = filter checkLegalRoom roomCodes
      sectorIds = map getSectorId legalRooms

  putStrLn $ "Sector id sum: " ++ (show (sum sectorIds))

  let storageRoom = head (filter (("northpoleobjectstorage" == ) . decrypt) roomCodes)

  putStrLn $ "North pole stuff in: " ++ (show (getSectorId storageRoom))
