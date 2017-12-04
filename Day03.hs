import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

sideSize :: Integer -> Integer
sideSize n = n * 2 - 1
spiralSize n | n == 1 = 1
spiralSize n | n > 1 = let g = sideSize n in g * g - spiralSize (n - 1)

cellCount :: Integer -> Integer
cellCount n = head . filter (>=n) . fmap ((^2) . sideSize) $ [1..]

data Cell = Cell Integer (Integer, Integer) deriving Show
cellValue (Cell v _) = v
cellX (Cell _ c) = fst c
cellY (Cell _ c) = snd c

iteration :: Integer -> [Cell]
iteration 1 = [Cell 1 (0,0)]
iteration n = zipWith Cell [((sideSize (n - 1))^2 + 1)..] (iterationCoords n)

iterationCoords n = right ++ top ++ left ++ bottom
  where
  side = sideSize n
  offsets z = [0..(z - 1)]
  maxCoord = side `div` 2
  right   = fmap (\y -> (maxCoord       , -maxCoord + 1 + y)) $ offsets (side - 2)
  top     = fmap (\x -> (maxCoord - x   , maxCoord))          $ offsets side
  left    = fmap (\y -> (-maxCoord      , maxCoord - 1 - y))  $ offsets (side - 2)
  bottom  = fmap (\x -> (-maxCoord + x  , -maxCoord))         $ offsets side

coordsInOrder = (0,0) : concatMap iterationCoords [2..]

iterations = zipWith Cell [1..] coordsInOrder

cellSteps (Cell _ (x,y)) = (abs x + abs y)

steps n = cellSteps $ iterations !! (n - 1)

getValue _ (0,0) = 1
getValue previous (x,y) = sum . Maybe.catMaybes $ fmap (flip Map.lookup previous) [(x + ox, y + oy) | ox <- [-1,0,1], oy <- [-1,0,1]]

valuesGo :: Map.Map (Integer, Integer) Integer -> [(Integer, Integer)] -> [Integer]
valuesGo previous (h : t) =
  let
    nextValue = getValue previous h
    previous' = Map.insert h nextValue previous
  in nextValue : valuesGo previous' t

values :: [Integer]
values = valuesGo Map.empty coordsInOrder

main = do
  print $ take 25 iterations
  print $ steps 1
  print $ steps 12
  print $ steps 23
  print $ steps 1024
  print $ steps 265149
  print $ take 25 values
  print $ head $ filter (> 265149) values 