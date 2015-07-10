module MyTree 
{ insert
, build
, inOrder
, sort
) where

data IntTree = Leaf
			 | Node IntTree Int IntTree
	deriving (Show, Eq)

insert :: Int -> IntTree -> IntTree
insert x Leaf = Node Leaf x Leaf
insert x (Node left node right)
	| x < node = Node (insert x left) node right
	| otherwise = Node left node (insert x right)

build :: [Int] -> IntTree
build [] = Leaf
build (x:xs) = insert x (build xs)

inOrder :: IntTree -> [Int]
inOrder Leaf = []
inOrder this@(Node left node right) 
	| this == Node Leaf node Leaf = node : []
	| otherwise = inOrder left ++ node : inOrder right

sort :: [Int] -> [Int]
sort = inOrder . build 