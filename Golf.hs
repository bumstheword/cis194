module Golf where
{-
everyNth' :: Int -> Int -> [a] -> [a]
everyNth' _ _ [] = []
everyNth' cur n (x:xs) 
	| cur == n = x : everyNth' 1 n xs
	| otherwise = everyNth' (cur + 1) n xs

everyNth :: Int -> [a] -> [a]
everyNth 0 _ = []
everyNth 1 xs = xs
everyNth n xs = everyNth' 1 n xs

skips :: [a] -> [[a]]
skips [] = []
skips xs = skips' 1 xs
	where 
		skips' :: Int -> [a] -> [[a]]
		skips' [] = []
		skips' cur xs = everyNth cur xs : 
-}

import Data.List

localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:list)
	| y > x && y > z = y : localMaxima (y:z:list)
	| otherwise = localMaxima (y:z:list)
localMaxima _ = []

-- Histogram 
histBase :: String
histBase = "==========\n0123456789\n"

histogram :: [Integer] -> String
histogram xs = (unlines . reverse) (buildhisto xs) ++ "==========\n0123456789\n\n"

buildhisto :: [Integer] -> [String]
buildhisto [] = []
buildhisto xs = histoline xs : buildhisto xs'
	where xs' = xs \\ [0..9]

histoline :: [Integer] -> String
histoline xs = map mark [0..9]
	where mark x | elem x xs = '*'
				 | otherwise = ' '













