{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

import qualified Control.Monad
import qualified Data.Maybe
import qualified Data.List
import qualified Data.Set
import qualified Data.String.Interpolate
import           Hedgehog ((===))
import qualified Hedgehog
import qualified System.IO
import qualified System.Exit

example = [Data.String.Interpolate.i|0	2	7	0|]
input = [Data.String.Interpolate.i|2	8	8	5	4	2	3	1	5	5	1	2	15	13	5	14|]

parse :: String -> [Int]
parse = fmap read . words

test = Hedgehog.withTests 1 . Hedgehog.property

set i v [] = []
set 0 v (x : xs) = v : xs
set n v (x : xs) = x : set (n - 1) v xs
findRedist xs = let m = maximum xs in (Data.Maybe.fromJust $ Data.List.elemIndex m xs, m)
blocks n l = zipWith (+) (replicate l (n `div` l)) (take (n `mod` l) (repeat 1) ++ repeat 0)
shiftRight n xs = let len = length xs in take len (drop (len - n) (xs ++ xs))
distribute (i, v) xs = zipWith (+) (shiftRight (i + 1) (blocks v (length xs))) (set i 0 xs)
next xs = let rd = findRedist xs in distribute rd xs
answer zs = go Data.Set.empty zs
  where
  go s xs =
    let xs' = next xs
    in if Data.Set.member xs' s
        then Data.Set.size s + 1
        else go (Data.Set.insert xs' s) xs'
answerB zs = go Data.Set.empty zs
  where
  go s xs =
    let xs' = next xs
    in if Data.Set.member xs' s
        then (1 +) $ length $ takeWhile (/= xs') $ drop 1 $ iterate next xs'
        else go (Data.Set.insert xs' s) xs'

prop_example_parse = test $ parse example === [0,2,7,0]
prop_example_findRedist = test $ findRedist (parse example) === (2, 7)
prop_blocks_7_4 = test $ blocks 7 4 === [2,2,2,1]
prop_shift = test $ shiftRight 3 [2,2,2,1] === [2,2,1,2]
prop_example_dist_1 = test $ iterate next (parse example) !! 1 === [2,4,1,2]
prop_example_dist_2 = test $ iterate next (parse example) !! 2 === [3,1,2,3]
prop_example_dist_3 = test $ iterate next (parse example) !! 3 === [0,2,3,4]
prop_example_dist_4 = test $ iterate next (parse example) !! 4 === [1,3,4,1]
prop_example_dist_5 = test $ iterate next (parse example) !! 5 === [2,4,1,2]
prop_exmaple_answer = test $ answer (parse example) === 5
prop_exmaple_answerB = test $ answerB (parse example) === 4
prop_input_answer = test $ answer (parse input) === 3156
prop_input_answerB = test $ answerB (parse input) === 1610

main = do
  let setLineBuffering x = System.IO.hSetBuffering x System.IO.LineBuffering
  setLineBuffering System.IO.stdout
  setLineBuffering System.IO.stderr

  result <- Hedgehog.checkSequential $$(Hedgehog.discover)

  Control.Monad.unless result System.Exit.exitFailure
