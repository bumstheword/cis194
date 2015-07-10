data IntList = Empty
			 | Cons Int IntList
	deriving Show

buildIntList :: [Int] -> IntList
buildIntList [] = Empty
buildIntList (x:xs) = Cons x (buildIntList xs)

toInts :: IntList -> [Int]
toInts Empty = []
toInts (Cons x xs) = x : (toInts xs)

mapIntList :: (Int -> Int) -> IntList -> IntList
mapIntList _ Empty       = Empty
mapIntList f (Cons x xs) = Cons (f x) (mapIntList f xs)

filterIntList :: (Int -> Bool) -> IntList -> IntList
filterIntList _ Empty = Empty
filterIntList f (Cons x xs) 
	| f x       = Cons x (filterIntList f xs)
	| otherwise = filterIntList f xs

-- Polymorphic Data Type 
data List t = E 
			| C t (List t)
	deriving Show
	
buildList :: [t] -> List t
buildList [] = E
buildList (x:xs) = C x (buildList xs)

mapList :: (a -> b) -> List a -> List b
mapList _ E = E
mapList f (C x xs) = C (f x) (mapList f xs)

filterList :: (t -> Bool) -> List t -> List t
filterList _ E = E
filterList f (C x xs) 
	| f x       = C x (filterList f xs)
	| otherwise = filterList f xs