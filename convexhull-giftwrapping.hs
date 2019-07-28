import Text.Printf
import Data.Function
import Data.List

solve :: [(Int, Int)] -> Double
solve points = calculatePerimeter points leftMostPoint leftMostPoint 0
    where
        leftMostPoint = findLeftMostPoint points

calculatePerimeter :: [(Int, Int)] -> (Int, Int) -> (Int, Int) -> Double -> Double
calculatePerimeter points leftMostPoint currentPoint currentPerimeter
  | currentPoint == leftMostPoint && currentPerimeter /= 0 = currentPerimeter
  | otherwise = 
    let nextPoint = findNextPoint currentPoint points
        distance = dist currentPoint nextPoint
      in calculatePerimeter points leftMostPoint nextPoint (currentPerimeter + distance)


findLeftMostPoint :: [(Int, Int)] -> (Int, Int)
findLeftMostPoint = minimumBy (compare `on` fst) 

findNextPoint currentPoint points = maximumBy (findRelativeDirectionTo currentPoint) (filter (/= currentPoint) points)

findRelativeDirectionTo (x,y) (ax,ay) (bx,by)
  | cp > 0 = GT
  | cp < 0 = LT
  | otherwise = EQ
  where
    p1 = (ax - x, ay - y)
    p2 = (bx - x, by - y)
    crossProduct (x1, y1) (x2, y2) = x1 * y2 - y1 * x2
    cp = crossProduct p1 p2

dist :: (Int, Int) -> (Int, Int) -> Double
dist (x1, y1) (x2, y2) = sqrt $ fromIntegral ((x1 - x2) ^ 2 + (y1 - y2) ^ 2)

d1, d2 :: [(Int, Int)]
d1 = [(1, 1),(2, 5),(3, 3),(5, 3),(3, 2),(2, 2)]
d2 = [(3, 2),(2, 5),(4, 5)]

main :: IO ()
main = do
  n <- readLn :: IO Int
  content <- getContents
  let  
    points = map (\[x, y] -> (x, y)). map (map (read::String->Int)). map words. lines $ content
    ans = solve points
  printf "%.1f\n" ans
