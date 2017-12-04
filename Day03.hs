
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
iteration n = zipWith (\c v -> Cell v c) (right ++ top ++ left ++ bottom) values
  where
  side = sideSize n
  values = [((sideSize (n - 1))^2 + 1)..]
  offsets z = [0..(z - 1)]
  maxCoord = side `div` 2
  right   = fmap (\y -> (maxCoord       , -maxCoord + 1 + y)) $ offsets (side - 2)
  top     = fmap (\x -> (maxCoord - x   , maxCoord))          $ offsets side
  left    = fmap (\y -> (-maxCoord      , maxCoord - 1 - y))  $ offsets (side - 2)
  bottom  = fmap (\x -> (-maxCoord + x  , -maxCoord))         $ offsets side

iterations = concatMap iteration [1..]

cellSteps (Cell _ (x,y)) = (abs x + abs y)

steps n = cellSteps $ iterations !! (n - 1)

main = do
  print $ iteration 1
  print $ iteration 2
  print $ iteration 3
  print $ steps 1
  print $ steps 12
  print $ steps 23
  print $ steps 1024
  print $ steps 265149
