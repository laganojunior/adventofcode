import Data.List

type Triangle = (Integer, Integer, Integer)

parseTriangle :: String -> Triangle 
parseTriangle s =
  let (a : b : c : _) = map read (words s) in
    (a, b, c)

legalTriangle :: Triangle -> Bool
legalTriangle (a, b, c) =
  let (smallest : middle : largest : _) = sort [a, b, c] in
    smallest + middle > largest

transposeTriangles :: [Triangle] -> [Triangle]
transposeTriangles [] = []
transposeTriangles ((a1, b1, c1) : (a2, b2, c2) : (a3, b3, c3) : rest) = (a1, a2, a3) : (b1, b2, b3) : (c1, c2, c3) : transposeTriangles rest

main :: IO ()
main = do
  input <- getContents

  let triangles = (map parseTriangle) (lines input)
      numLegal = length (filter legalTriangle triangles)

  putStrLn $ "Number of legal triangles 1: " ++ show numLegal

  let triangles' = transposeTriangles triangles
      numLegal2 = length (filter legalTriangle triangles')

  putStrLn $ "Number of legal triangles 2: " ++ show numLegal2
