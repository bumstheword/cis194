module Sorting where

mergeSortSplit :: [a] -> ([a], [a])
mergeSortSplit xs = (take n xs, drop n xs)
    where n = div (length xs) 2

mergeSortMerge :: Ord a => [a] -> [a] -> [a]
mergeSortMerge xs [] = xs
mergeSortMerge [] ys = ys
mergeSortMerge (x:xs) (y:ys)
    | x <= y    = x : mergeSortMerge xs (y:ys)
    | otherwise = y : mergeSortMerge (x:xs) ys


mergeSort :: Ord a => [a] -> [a]
mergeSort xs
    | (length xs) > 1 = mergeSortMerge (mergeSort ls) (mergeSort rs)
    | otherwise = xs
  where (ls, rs) = mergeSortSplit xs 

